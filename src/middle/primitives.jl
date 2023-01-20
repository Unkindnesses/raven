# Core primitives – pack, packcat, part and nparts – are dealt with in
# middle-end lowering, but we define type inference here.

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
partial_notnil(x::Pack) = tag(x) == :Nil ? ⊥ : x

function partial_notnil(x::Or)
  ps = filter(x -> tag(x) != :Nil, x.patterns)
  return length(ps) == 1 ? ps[1] : Or(ps)
end

partial_symstring(x::Symbol) = String(x)
partial_symstring(x::Or) = String

pack_method = RMethod(:pack, lowerpattern(rvx"args"), args -> pack(parts(args)...), true)
part_method = RMethod(:part, lowerpattern(rvx"[data, i]"), partial_part, true)
nparts_method = RMethod(:nparts, lowerpattern(rvx"[x]"), partial_nparts, true)
packcat_method = RMethod(:packcat, lowerpattern(rvx"args"), args -> packcat(parts(args)...), true)
widen_method = RMethod(:widen, lowerpattern(rvx"[x]"), partial_widen, true)
shortcutEquals_method = RMethod(:shortcutEquals, lowerpattern(rvx"[a, b]"), partial_shortcutEquals, true)

isnil_method = RMethod(:isnil, lowerpattern(rvx"[x]"), partial_isnil, true)
notnil_method = RMethod(:notnil, lowerpattern(rvx"[x]"), partial_notnil, true)
symstring_method = RMethod(:symstring, lowerpattern(rvx"[x: Symbol]"), partial_symstring, true)

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
  return mod
end

# Primitive lowering
# Invoked from middle-end lowering

const lowerPrimitive = IdDict{RMethod,Any}()

lowerPrimitive[widen_method] = function (cx, pr, ir, v)
  T = exprtype(cx.mod, ir, ir[v].expr.args[2])
  val = T isa Integer ? T : ir[v].expr.args[2]
  pr[v] = val
end

symoverlap(x::Symbol, ys::Or) = [i for (i, y) in enumerate(ys.patterns) if x == y]
symoverlap(xs::Or, y::Symbol) = symoverlap(y, xs)

lowerPrimitive[shortcutEquals_method] = function (cx, pr, ir, v)
  if isvalue(ir[v].type)
    pr[v] = Expr(:tuple)
  else # symbol case
    a, b = ir[v].expr.args[2:3]
    Ta, Tb = exprtype(cx.mod, ir, [a, b])
    Tb isa Or && ((a, Ta, b, Tb) = (b, Tb, a, Ta))
    ov = symoverlap(Ta, Tb)
    length(ov) == 1 || error("not implemented")
    i = insert!(pr, v, Expr(:ref, a, 1))
    pr[v] = xcall(WIntrinsic(i32.eq, i32), i, Int32(ov[1]))
  end
end

lowerPrimitive[isnil_method] = function (cx, pr, ir, v)
  x = ir[v].expr.args[2]
  T = exprtype(cx.mod, ir, x)
  if ir[v].type isa Int32
    pr[v] = ir[v].type
  else
    i = findfirst(==(nil), T.patterns)
    j = insert!(pr, v, Expr(:ref, x, 1))
    pr[v] = xcall(WIntrinsic(i32.eq, i32), j, Int32(i))
  end
  isreftype(T) && push!(pr, Expr(:release, x))
end

lowerPrimitive[notnil_method] = function (cx, pr, ir, v)
  x = ir[v].expr.args[2]
  T = exprtype(cx.mod, ir, x)
  if T == ir[v].type
    pr[v] = x
  elseif ir[v].type == ⊥
    # TODO make sure `not` in dispatcher infers
    s = insert!(pr, v, Expr(:ref, "notnil(nil)"))
    pr[v] = xcall(WIntrinsic(WebAssembly.Call(:panic), ⊥), s)
  else
    @assert T isa Or && !(ir[v].type isa Or)
    pr[v] = Expr(:tuple, [insert!(pr, v, Expr(:ref, x, i)) for i = 2:length(layout(T))]...)
  end
end

function symstring_ir(T::Or)
  ir = IR(meta = FuncInfo(:symstring))
  x = argument!(ir, type = T)
  union_cases!(ir, T, x) do S, _
    @assert S isa Symbol
    Expr(:ref, String(S))
  end
  return ir
end

lowerPrimitive[symstring_method] = function (cx, pr, ir, v)
  if ir[v].type isa String
    pr[v] = Expr(:tuple)
  else
    T = exprtype(cx, ir, ir[v].expr.args[2])
    S = (symstring_method, T)
    if !haskey(cx.frames, S)
      cx.frames[S] = symstring_ir(T)
    end
  end
end
