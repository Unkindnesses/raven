# Core primitives – data, datacat, part and nparts – are dealt with in
# middle-end lowering, but we define type inference here.

partial_part(data::Union{Data,Primitive,Type{<:Primitive}}, i::Integer) =
  0 <= i <= nparts(data) ? part(data, i) : ⊥

# TODO: HACK: we assume index != 0 when indexing dynamically.
# Should instead have a seperate `index` function that enforces this.
partial_part(data::Data, i::Type{<:Integer}) =
  reduce(union, parts(data))

partial_part(data::VData, i::Union{Int,Type{<:Integer}}) =
  i == 0 ? data.tag : data.parts

partial_part(data::Or, i) =
  reduce(union, partial_part.(data.patterns, (i,)))

partial_nparts(x::Data) = nparts(x)
partial_nparts(::VData) = Int64

partial_widen(x::Primitive) = typeof(x)
partial_widen(x) = x

# Fast, approximate equality check; basically a stand-in for pointer equality.
# TODO extend to handle vdata
partial_shortcutEquals(a, b) =
  Int32(isvalue(a) && isvalue(b) && a == b)

# Needed by dispatchers, since a user-defined method would need runtime matching
# to deal with unions.
partial_isnil(x::Union{Primitive,Type{<:Primitive}}) = Int32(0)
partial_isnil(x::Data) = Int32(x == data(:Nil))
partial_isnil(x::Or) = any(==(data(:Nil)), x.patterns) ? Int32 : Int32(0)

# Duct tape until the thatcher algorithm works.
partial_notnil(x::Data) = tag(x) == :Nil ? ⊥ : x

function partial_notnil(x::Or)
  ps = filter(x -> tag(x) != :Nil, x.patterns)
  return length(ps) == 1 ? ps[1] : Or(ps)
end

partial_symstring(x::Symbol) = String(x)

data_method = RMethod(:data, lowerpattern(rvx"args"), args -> data(parts(args)...), true)
part_method = RMethod(:part, lowerpattern(rvx"[data, i]"), partial_part, true)
nparts_method = RMethod(:nparts, lowerpattern(rvx"[x]"), partial_nparts, true)
datacat_method = RMethod(:datacat, lowerpattern(rvx"args"), args -> datacat(parts(args)...), true)
widen_method = RMethod(:widen, lowerpattern(rvx"[x]"), partial_widen, true)
shortcutEquals_method = RMethod(:shortcutEquals, lowerpattern(rvx"[a, b]"), partial_shortcutEquals, true)

isnil_method = RMethod(:isnil, lowerpattern(rvx"[x]"), partial_isnil, true)
notnil_method = RMethod(:notnil, lowerpattern(rvx"[x]"), partial_notnil, true)
symstring_method = RMethod(:symstring, lowerpattern(rvx"[x: Symbol]"), partial_symstring, true)

function primitives!(mod)
  mod[Symbol("false")] = Int32(0)
  mod[Symbol("true")] = Int32(1)
  method!(mod, :data, data_method)
  method!(mod, :part, part_method)
  method!(mod, :nparts, nparts_method)
  method!(mod, :datacat, datacat_method)
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

lowerPrimitive[shortcutEquals_method] = function (cx, pr, ir, v)
  @assert isvalue(ir[v].type)
  pr[v] = Expr(:tuple)
end

lowerPrimitive[isnil_method] = function (cx, pr, ir, v)
  x = ir[v].expr.args[2]
  T = exprtype(cx.mod, ir, x)
  if ir[v].type isa Int32
    pr[v] = ir[v].type
  else
    i = findfirst(==(data(:Nil)), T.patterns)
    j = insert!(pr, v, Expr(:ref, x, 1))
    pr[v] = xcall(WIntrinsic(i32.eq, i32), j, Int32(i))
  end
end

lowerPrimitive[notnil_method] = function (cx, pr, ir, v)
  x = ir[v].expr.args[2]
  T = exprtype(cx.mod, ir, x)
  if T == ir[v].type
    pr[v] = x
  elseif ir[v].type == ⊥
    # TODO make sure `not` in dispatcher infers
    pr[v] = xcall(WIntrinsic(WebAssembly.unreachable, ⊥))
  else
    @assert T isa Or && !(ir[v].type isa Or)
    pr[v] = Expr(:tuple, [insert!(pr, v, Expr(:ref, x, i)) for i = 2:length(layout(T))]...)
  end
end

lowerPrimitive[symstring_method] = function (cx, pr, ir, v)
  @assert ir[v].type isa String
  pr[v] = Expr(:tuple)
end
