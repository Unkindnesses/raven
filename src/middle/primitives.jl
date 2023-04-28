# Core primitives – pack, packcat, part and nparts – are dealt with in
# middle-end lowering, but we define type inference here, and implement some
# simple built-in functions.

partial_part(data::Union{Pack,Primitive,Type{<:Primitive}}, i::Integer) =
  0 <= i <= nparts(data) ? part(data, i) : ⊥

# TODO: HACK: we assume index != 0 when indexing dynamically.
# Should instead have a seperate `index` function that enforces this.
partial_part(data::Pack, i::Type{<:Integer}) =
  reduce(union, parts(data))

partial_part(data::VPack, i::Union{Int,Type{<:Integer}}) =
  i == 0 ? data.tag : data.parts

partial_part(data::Or, i) =
  reduce(union, partial_part.(data.patterns, (i,)))

partial_part(data::Recursive, i) =
  partial_part(unroll(data), i)

partial_nparts(x::Pack) = nparts(x)
partial_nparts(::VPack) = Int64

partial_nparts(x::Or) =
  reduce(union, partial_nparts.(x.patterns))

partial_nparts(x::Recursive) = partial_nparts(unroll(x))

partial_widen(x::Primitive) = typeof(x)
partial_widen(x) = x

# Fast, approximate equality check; basically a stand-in for pointer equality.
# TODO extend to handle VPack
partial_shortcutEquals(a, b) =
  isvalue(a) && isvalue(b) ? Int32(a == b) :
  !isempty(intersect(symbolValues(a), symbolValues(b))) ? Int32 :
  Int32(false)

# Needed by dispatchers, since a user-defined method would need runtime matching
# to deal with unions.
partial_isnil(x::Union{Primitive,Type{<:Primitive},VPack}) = Int32(0)
partial_isnil(x::Pack) = Int32(x == nil)
partial_isnil(x::Or) = any(==(nil), x.patterns) ? Int32 : Int32(0)

# Duct tape until the thatcher algorithm works.
partial_notnil(x::Pack) = tag(x) == tag"Nil" ? ⊥ : x

function partial_notnil(x::Or)
  ps = filter(x -> tag(x) != tag"Nil", x.patterns)
  return length(ps) == 1 ? ps[1] : Or(ps)
end

partial_symstring(x::Tag) = string(x)
partial_symstring(x::Or) = RString

partial_function(f, I, O) = Int32
partial_invoke(f, I, O, xs...) = rvtype(O)

pack_method = RMethod(tag"common.core.pack", lowerpattern(rvx"args"), args -> pack(parts(args)...), true)
part_method = RMethod(tag"common.core.part", lowerpattern(rvx"[data, i]"), partial_part, true)
nparts_method = RMethod(tag"common.core.nparts", lowerpattern(rvx"[x]"), partial_nparts, true)
packcat_method = RMethod(tag"common.core.packcat", lowerpattern(rvx"args"), args -> packcat(parts(args)...), true)
widen_method = RMethod(tag"common.core.widen", lowerpattern(rvx"[x]"), partial_widen, true)
shortcutEquals_method = RMethod(tag"common.core.shortcutEquals", lowerpattern(rvx"[a, b]"), partial_shortcutEquals, true)

isnil_method = RMethod(tag"common.core.isnil", lowerpattern(rvx"[x]"), partial_isnil, true)
notnil_method = RMethod(tag"common.core.notnil", lowerpattern(rvx"[x]"), partial_notnil, true)
symstring_method = RMethod(tag"common.core.symstring", lowerpattern(rvx"[x: Tag]"), partial_symstring, true)

function_method = RMethod(tag"common.core.function", lowerpattern(rvx"[f, I, O]"), partial_function, true)
# TODO Has to be in main because Raven overloads from there
invoke_method = RMethod(tag"common.core", tag"invoke", lowerpattern(rvx"[f: Int32, I, O, xs...]"), partial_invoke, true)

const primitives = [
  pack_method,
  part_method,
  nparts_method,
  packcat_method,
  widen_method,
  shortcutEquals_method,
  isnil_method,
  notnil_method,
  symstring_method,
  function_method,
  invoke_method,
]

# Primitive implementations
# Invoked from middle-end lowering. `inline` primitives replace the call with a
# definition. `outline` ones return a lowered IR fragment, to be called as a
# normal function.

const inlinePrimitive = IdDict{RMethod,Any}()
const outlinePrimitive = IdDict{RMethod,Any}()

inlinePrimitive[widen_method] = function (pr, ir, v)
  T = exprtype(ir, ir[v].expr.args[2])
  val = T isa Integer ? T : ir[v].expr.args[2]
  pr[v] = val
end

symoverlap(x::Tag, ys::Or) = [i for (i, y) in enumerate(ys.patterns) if x == y]
symoverlap(xs::Or, y::Tag) = symoverlap(y, xs)

inlinePrimitive[shortcutEquals_method] = function (pr, ir, v)
  if isvalue(ir[v].type)
    pr[v] = Expr(:tuple)
  else # symbol case
    a, b = ir[v].expr.args[2:3]
    Ta, Tb = exprtype(ir, [a, b])
    Tb isa Or && ((a, Ta, b, Tb) = (b, Tb, a, Ta))
    ov = symoverlap(Ta, Tb)
    length(ov) == 1 || error("not implemented")
    i = insert!(pr, v, Expr(:ref, a, 1))
    pr[v] = xcall(WIntrinsic(i32.eq, i32), i, Int32(ov[1]))
  end
end

inlinePrimitive[isnil_method] = function (pr, ir, v)
  x = ir[v].expr.args[2]
  T = exprtype(ir, x)
  if ir[v].type isa Int32
    pr[v] = ir[v].type
  else
    i = findfirst(==(nil), T.patterns)
    j = insert!(pr, v, Expr(:ref, x, 1))
    pr[v] = xcall(WIntrinsic(i32.eq, i32), j, Int32(i))
  end
  isreftype(T) && push!(pr, Expr(:release, x))
end

inlinePrimitive[notnil_method] = function (pr, ir, v)
  x = ir[v].expr.args[2]
  T = exprtype(ir, x)
  if T == ir[v].type
    pr[v] = x
  elseif ir[v].type == ⊥
    # TODO make sure `not` in dispatcher infers
    delete!(pr, v)
    replace!(pr, v, panic!(pr, "notnil(nil)"))
  else
    @assert T isa Or && !(ir[v].type isa Or)
    pr[v] = Expr(:tuple, [insert!(pr, v, Expr(:ref, x, i)) for i = 2:length(layout(T))]...)
  end
end

inlinePrimitive[symstring_method] = function (pr, ir, v)
  if ir[v].type isa String
    pr[v] = Expr(:tuple)
  end
end

function string!(ir, s)
  s = push!(ir, stmt(Expr(:tuple), type = s))
  push!(ir, stmt(xcall(part_method, s, 1), type = RString))
end

outlinePrimitive[symstring_method] = function (T::Or)
  ir = IR(meta = FuncInfo(tag"symstring"))
  x = argument!(ir, type = T)
  union_cases!(ir, T, x) do S, _
    @assert S isa Tag
    string!(ir, string(S))
  end
  return ir
end

# UB if inferred output type is not `O`
# TODO wrap with a type check / conversion
inlinePrimitive[function_method] = function (pr, ir, v)
  f, I, O = exprtype(ir, ir[v].expr.args[2:end])
  @assert all(isvalue, (f, I, O))
  pr[v] = Expr(:func, f, rvtype(I), rvtype(O))
end

inlinePrimitive[invoke_method] = function (pr, ir, v)
  f, I, O, args = ir[v].expr.args[2:end]
  I, O = rvtype.(exprtype(ir, [I, O]))
  # TODO conversion
  @assert issubset(exprtype(ir, args), I)
  delete!(pr, v)
  args = cast!(pr, exprtype(ir, args), I, args)
  v′ = push!(pr, stmt(ir[v], expr = Expr(:call_indirect, f, args)))
  replace!(pr, v, v′)
end

# Core module / prelude

function core()
  mod = RModule(tag"common.core")
  mod[Symbol("false")] = Int32(0)
  mod[Symbol("true")] = Int32(1)
  for meth in primitives
    # TODO: do defs/exports in Raven
    f = name(meth.name)
    mod.defs[f] = meth.name
    push!(mod.exports, f)
    method!(mod, meth)
  end
  push!(mod.exports, Symbol("true"))
  push!(mod.exports, Symbol("false"))
  return mod
end

function prelude!(mod)
  import!(mod, core(), [map(m -> name(m.name), primitives)..., Symbol("true"), Symbol("false")])
  return mod
end
