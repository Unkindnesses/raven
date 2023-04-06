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

pack_method = RMethod(:pack, lowerpattern(rvx"args"), args -> pack(parts(args)...), true)
part_method = RMethod(:part, lowerpattern(rvx"[data, i]"), partial_part, true)
nparts_method = RMethod(:nparts, lowerpattern(rvx"[x]"), partial_nparts, true)
packcat_method = RMethod(:packcat, lowerpattern(rvx"args"), args -> packcat(parts(args)...), true)
widen_method = RMethod(:widen, lowerpattern(rvx"[x]"), partial_widen, true)
shortcutEquals_method = RMethod(:shortcutEquals, lowerpattern(rvx"[a, b]"), partial_shortcutEquals, true)

isnil_method = RMethod(:isnil, lowerpattern(rvx"[x]"), partial_isnil, true)
notnil_method = RMethod(:notnil, lowerpattern(rvx"[x]"), partial_notnil, true)
symstring_method = RMethod(:symstring, lowerpattern(rvx"[x: Tag]"), partial_symstring, true)

partial_function(f, I, O) = Int32
partial_invoke(f, I, O, xs...) = rvtype(O)

function_method = RMethod(:function, lowerpattern(rvx"[f, I, O]"), partial_function, true)
invoke_method = RMethod(:invoke, lowerpattern(rvx"[f: Int32, I, O, xs...]"), partial_invoke, true)

function primitives!(mod)
  mod[Symbol("false")] = Int32(0)
  mod[Symbol("true")] = Int32(1)
  method!(mod, :pack, pack_method)
  method!(mod, :part, part_method)
  method!(mod, :nparts, nparts_method)
  method!(mod, :packcat, packcat_method)
  method!(mod, :widen, widen_method)
  method!(mod, :shortcutEquals, shortcutEquals_method)

  method!(mod, :isnil, isnil_method)
  method!(mod, :notnil, notnil_method)
  method!(mod, :symstring, symstring_method)

  method!(mod, :function, function_method)
  method!(mod, :invoke, invoke_method)
  return mod
end

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
