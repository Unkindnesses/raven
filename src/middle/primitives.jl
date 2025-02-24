struct Partial
  val
  typ
end

(f::Partial)(xs...) = all(isvalue, xs) ? f.val(xs...) : f.typ(xs...)

for (op, f) in [(:shl, <<), (:shr_u, >>), (:shr_s, >>), (:and, &), (:or, |),
                (:xor, ^), (:add, +), (:sub, -), (:mul, *),
                (:div_u, ÷), (:div_s, ÷), (:rem_u, rem), (:rem_s, rem)]
  T = endswith(string(op), "_s") ? Int64 : UInt64
  @eval $op(x::Bits{N}, y::Bits{N}) where N = Bits{N}($f($T(x), $T(y)))
end

for (op, f) in [(:eq, ==), (:ne, !=), (:gt_u, >), (:lt_u, <),
                (:ge_u, >=), (:le_u, <=), (:gt_s, >), (:lt_s, <),
                (:ge_s, >=), (:le_s, <=)]
  T = endswith(string(op), "_s") ? Int64 : UInt64
  @eval $op(x::Bits{N}, y::Bits{N}) where N = Bits{1}($f($T(x), $T(y)))
end

# Core primitives – pack, packcat, part and nparts – are dealt with in
# IR expansion, but we define type inference here, and implement some
# simple built-in functions.

partial_part(data::Union{Pack,Primitive,Type{<:Primitive}}, i::Integer) =
  0 <= i <= nparts(data) ? part(data, i) : ⊥

# TODO: HACK: we assume index != 0 when indexing dynamically.
# Should instead have a seperate `index` function that enforces this.
partial_part(data::Pack, i::Type{<:Integer}) =
  reduce(union, parts(data))

partial_part(data::Union{Primitive,Type{<:PrimitiveNumber}}, t::Type{<:Integer}) = data

partial_part(data::VPack, i::ValOrType{Int64}) =
  i == 0 ? data.tag : data.parts

partial_part(data::Onion, i::ValOrType{Int64}) =
  reduce(union, partial_part.(data.types, (i,)))

partial_part(data::Recursive, i::ValOrType{Int64}) =
  partial_part(unroll(data), i)

function partial_part(data, x::Pack)
  @assert tag(x) == tag"common.Int"
  partial_part(data, isvalue(x) ? Int64(part(x, 1)) : Int64)
end

partial_nparts(x::Pack) = RInt64(nparts(x))
partial_nparts(::VPack) = RInt64()
partial_nparts(::String) = nparts(RString())
partial_nparts(x::Union{Primitive,Type{<:PrimitiveNumber}}) = RInt64(nparts(x))

partial_nparts(x::Onion) =
  reduce(union, partial_nparts.(x.types))

partial_nparts(x::Recursive) = partial_nparts(unroll(x))

partial_widen(x::String) = RString()
partial_widen(x::PrimitiveNumber) = typeof(x)
partial_widen(x) = x

partial_widen(x::Pack) =
  isvalue(x) && tag(x) == tag"common.Int" ?
    pack(tag"common.Int", typeof(part(x, 1))) :
    x

# Fast, approximate equality check; basically a stand-in for pointer equality.
# TODO extend to handle VPack
partial_shortcutEquals(a, b) =
  isvalue(a) && isvalue(b) ? RBool(a == b) :
  !isempty(intersect(symbolValues(a), symbolValues(b))) ? RBool() :
  RBool(false)

partial_bitsize(::ValOrType{Bits{N}}) where N = RInt64(N)

partial_bitcast(::ValOrType{Bits{N}}, x::Bits) where N = Bits{N}(UInt64(x))
partial_bitcast(::ValOrType{Bits{N}}, x::Type{<:Bits}) where N = Bits{N}

partial_bitcast_s(::ValOrType{Bits{N}}, x::Bits) where N = Bits{N}(Int64(x))
partial_bitcast_s(::ValOrType{Bits{N}}, x::Type{<:Bits}) where N = Bits{N}

partial_bitop(::ValOrType{Bits{N}}, x::ValOrType{Bits{N}}) where N = Bits{N}
partial_bitcmp(::ValOrType{Bits{N}}, x::ValOrType{Bits{N}}) where N = Bits{1}
partial_biteqz(x::Bits) = Bits{1}(x.value == 0)
partial_biteqz(::Type{<:Bits}) = Bits{1}

# Needed by dispatchers, since a user-defined method would need runtime matching
# to deal with unions.
partial_isnil(x::Union{Primitive,Type{<:Primitive},VPack}) = RBool(false)
partial_isnil(x::Pack) = RBool(x == nil)
partial_isnil(x::Onion) = any(==(nil), x.types) ? RBool() : RBool(false)

partial_notnil(x::Pack) = tag(x) == tag"common.Nil" ? ⊥ : x
partial_notnil(x::Union{Primitive,Type{<:Primitive}}) = x

function partial_notnil(x::Onion)
  ps = filter(x -> tag(x) != tag"common.Nil", x.types)
  return length(ps) == 1 ? ps[1] : Onion(ps)
end

partial_notnil(x::Recursive) = recursive(partial_notnil(unroll(x)))

partial_tagcast(x::Union{Pack,VPack,Primitive,Type{<:PrimitiveNumber}}, t::Tag) =
  tag(x) == t ? x : ⊥

function partial_tagcast(x::Onion, t::Tag)
  ps = filter(x -> tag(x) == t, x.types)
  return isempty(ps) ? ⊥ : only(ps)
end

partial_tagcast(x::Recursive, t::Tag) = partial_tagcast(unroll(x), t)

partial_tagstring(x::Tag) = string(x)
partial_tagstring(x::Onion) = RString()

partial_function(f, I, O) = RInt32()
partial_invoke(f, I, O, xs...) = rvtype(O)

partial_jsalloc() = RBool(options().jsalloc)

pack_method = RMethod(tag"common.core.pack", lowerpattern(rvx"args"), args -> pack(parts(args)...), true)
part_method = RMethod(tag"common.core.part", lowerpattern(rvx"[data, i]"), partial_part, true)
nparts_method = RMethod(tag"common.core.nparts", lowerpattern(rvx"[x]"), partial_nparts, true)
packcat_method = RMethod(tag"common.core.packcat", lowerpattern(rvx"args"), args -> packcat(parts(args)...), true)
widen_method = RMethod(tag"common.core.widen", lowerpattern(rvx"[x]"), partial_widen, true)
shortcutEquals_method = RMethod(tag"common.core.shortcutEquals", lowerpattern(rvx"[a, b]"), partial_shortcutEquals, true)

bitsize_method = RMethod(tag"common.core.bitsize", lowerpattern(rvx"[x]"), partial_bitsize, true)
bitcast_method = RMethod(tag"common.core.bitcast", lowerpattern(rvx"[x, y]"), partial_bitcast, true)
bitcast_s_method = RMethod(tag"common.core.bitcast_s", lowerpattern(rvx"[x, y]"), partial_bitcast_s, true)

bitops = [:shl, :shr_u, :shr_s, :and, :or, :xor, :add, :sub, :mul, :div_u, :div_s, :rem_u, :rem_s]
for op in bitops
  @eval $(Symbol(:bit, op, :_method)) =
    RMethod(Tag(:common, :core, $(QuoteNode(Symbol(:bit, op)))),
            lowerpattern(rvx"[x, y]"), Partial($op, partial_bitop), true)
end
bitop_methods = @eval [$([Symbol(:bit, op, :_method) for op in bitops]...)]

bitcmps = [:eq, :ne, :gt_u, :ge_u, :lt_u, :le_u, :gt_s, :ge_s, :lt_s, :le_s]
for op in bitcmps
  @eval $(Symbol(:bit, op, :_method)) =
    RMethod(Tag(:common, :core, $(QuoteNode(Symbol(:bit, op)))),
            lowerpattern(rvx"[x, y]"), Partial($op, partial_bitcmp), true)
end
bitcmp_methods = @eval [$([Symbol(:bit, op, :_method) for op in bitcmps]...)]

biteqz_method = RMethod(tag"common.core.biteqz", lowerpattern(rvx"[x]"), partial_biteqz, true)

isnil_method = RMethod(tag"common.core.isnil", lowerpattern(rvx"[x]"), partial_isnil, true)
notnil_method = RMethod(tag"common.core.notnil", lowerpattern(rvx"[x]"), partial_notnil, true)
tagcast_method = RMethod(tag"common.core.tagcast", lowerpattern(rvx"[x, t]"), partial_tagcast, true)
tagstring_method = RMethod(tag"common.core.tagstring", lowerpattern(rvx"[x]"), partial_tagstring, true)

function_method = RMethod(tag"common.core.function", lowerpattern(rvx"[f, I, O]"), partial_function, true)
invoke_method = RMethod(tag"common.core.invoke", lowerpattern(rvx"[f, I, O, xs...]"), partial_invoke, true)

jsalloc_method = RMethod(tag"common.core.jsalloc", lowerpattern(rvx"[]"), partial_jsalloc, true)

primitives() = [
  pack_method,
  part_method,
  nparts_method,
  packcat_method,
  widen_method,
  shortcutEquals_method,
  bitsize_method,
  bitcast_method,
  bitcast_s_method,
  bitop_methods...,
  bitcmp_methods...,
  biteqz_method,
  isnil_method,
  notnil_method,
  tagcast_method,
  tagstring_method,
  function_method,
  invoke_method,
  jsalloc_method,
]

# Primitive implementations
# Invoked from middle-end lowering. `inline` primitives replace the call with a
# definition. `outline` ones return an expanded IR fragment, to be called as a
# normal function.

const inlinePrimitive = IdDict{RMethod,Any}()
const outlinePrimitive = IdDict{RMethod,Any}()

inlinePrimitive[widen_method] = function (pr, ir, v)
  x = ir[v].expr.args[2]
  T = exprtype(ir, x)
  if T isa String
    id = insert!(pr, v, stmt(Expr(:ref, T), type = rlist(RInt32())))
    pr[v] = stmt(xcall(tag"common.JSObject", id), type = RString())
  elseif T isa Number
    pr[v] = T
  elseif T isa Bits
    pr[v] = data(T)
  elseif tag(T) == tag"common.Int"
    pr[v] = data(part(T, 1))
  else
    pr[v] = x
  end
end

inlinePrimitive[bitsize_method] = function (pr, ir, v)
  pr[v] = xtuple()
end

function mask(T, x)
  m = (one(unsigned(layout(T))) << nbits(T)) - 0x01
  stmt(xcall(WType(layout(T)).and, x, m), type = layout(T))
end

function extend!(pr, v, T, x)
  shift = layout(T)(64 - nbits(T))
  x = insert!(pr, v, stmt(xcall(WType(layout(T)).shl, x, shift), type = layout(T)))
  x = insert!(pr, v, stmt(xcall(WType(layout(T)).shr_s, x, shift), type = layout(T)))
end

inlinePrimitive[bitcast_method] = function (pr, ir, v)
  if isvalue(ir[v].type)
    pr[v] = xtuple()
    return
  end
  x = ir[v].expr.args[3]
  F = exprtype(ir, x)
  T = exprtype(ir, v)
  if (layout(T), layout(F)) == (Int32, Int64)
    x = insert!(pr, v, stmt(xcall(i32.wrap_i64, x), type = Int32))
  elseif (layout(T), layout(F)) == (Int64, Int32)
    x = insert!(pr, v, stmt(xcall(i64.extend_i32_u, x), type = Int64))
  end
  if nbits(T) < nbits(F) && nbits(T) < nbits(layout(T))
    x = insert!(pr, v, mask(T, x))
  end
  pr[v] = x
  return
end

inlinePrimitive[bitcast_s_method] = function (pr, ir, v)
  if isvalue(ir[v].type)
    pr[v] = xtuple()
    return
  end
  x = ir[v].expr.args[3]
  F = exprtype(ir, x)
  T = exprtype(ir, v)
  nbits(T) <= nbits(F) && return inlinePrimitive[bitcast_method](pr, ir, v)
  if nbits(F) < nbits(layout(F))
    x = extend!(pr, v, F, x)
  end
  if (nbits(layout(T)), nbits(layout(F))) == (64, 32)
    x = insert!(pr, v, stmt(xcall(i64.extend_i32_s, x), type = Int64))
  end
  if nbits(T) < nbits(layout(T))
    x = insert!(pr, v, mask(T, x))
  end
  pr[v] = x
  return
end

for op in bitops
  @eval inlinePrimitive[$(Symbol(:bit, op, :_method))] = function (pr, ir, v)
    T = exprtype(ir, v)
    if isvalue(T)
      pr[v] = xtuple()
      return
    end
    x, y = ir[v].expr.args[2:end]
    sz = nbits(layout(T))
    if $(endswith(string(op), "_s")) && nbits(T) < nbits(layout(T))
      x = extend!(pr, v, T, x)
      y = extend!(pr, v, T, y)
    end
    r = insert!(pr, v, stmt(xcall(WType(layout(T)).$op, x, y), type = layout(T)))
    if nbits(T) < nbits(layout(T))
      r = insert!(pr, v, mask(T, r))
    end
    pr[v] = r
  end
end

for op in bitcmps
  @eval inlinePrimitive[$(Symbol(:bit, op, :_method))] = function (pr, ir, v)
    if isvalue(exprtype(ir, v))
      pr[v] = xtuple()
      return
    end
    x, y = ir[v].expr.args[2:end]
    T = union(exprtype(ir, [x, y])...)
    if $(endswith(string(op), "_s")) && nbits(T) < nbits(layout(T))
      x = extend!(pr, v, T, x)
      y = extend!(pr, v, T, y)
    end
    pr[v] = xcall(WType(layout(T)).$op, x, y)
  end
end

inlinePrimitive[biteqz_method] = function (pr, ir, v)
  x = ir[v].expr.args[2]
  T = exprtype(ir, x)
  if isvalue(ir[v].type)
    pr[v] = xtuple()
  elseif nbits(layout(T)) == 64
    pr[v] = xcall(i64.eqz, x)
  elseif nbits(layout(T)) == 32
    pr[v] = xcall(i32.eqz, x)
  end
end

symoverlap(x::Tag, ys::Onion) = [i for (i, y) in enumerate(ys.types) if x == y]
symoverlap(xs::Onion, y::Tag) = symoverlap(y, xs)

inlinePrimitive[shortcutEquals_method] = function (pr, ir, v)
  if isvalue(ir[v].type)
    pr[v] = Expr(:tuple)
  else # symbol case
    a, b = ir[v].expr.args[2:3]
    Ta, Tb = exprtype(ir, [a, b])
    Tb isa Onion && ((a, Ta, b, Tb) = (b, Tb, a, Ta))
    ov = symoverlap(Ta, Tb)
    length(ov) == 1 || error("not implemented")
    i = insert!(pr, v, Expr(:ref, a, 1))
    pr[v] = xcall(i32.eq, i, Int32(ov[1]))
  end
end

inlinePrimitive[isnil_method] = function (pr, ir, v)
  x = ir[v].expr.args[2]
  T = exprtype(ir, x)
  if isvalue(ir[v].type)
    pr[v] = xtuple()
  else
    i = findfirst(==(nil), T.types)
    j = insert!(pr, v, Expr(:ref, x, 1))
    pr[v] = xcall(i32.eq, j, Int32(i))
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
    replace!(pr, v, abort!(pr, "notnil(nil)"))
  else
    @assert T isa Onion && !(ir[v].type isa Onion)
    i = findfirst(x -> x != nil, T.types)
    delete!(pr, v)
    replace!(pr, v, union_downcast!(pr, T, i, x))
  end
end

inlinePrimitive[tagcast_method] = function (pr, ir, v)
  x, tag = ir[v].expr.args[2:3]
  T = exprtype(ir, x)
  tag = exprtype(ir, tag)
  if T == ir[v].type
    pr[v] = x
  elseif ir[v].type == ⊥
    delete!(pr, v)
    replace!(pr, v, abort!(pr, "tagcast"))
  else
    delete!(pr, v)
    if T isa Recursive
      T = unroll(T)
      x = unbox!(pr, T, x)
    end
    i = findfirst(x -> Raven.tag(x) == tag, (T::Onion).types)
    replace!(pr, v, union_downcast!(pr, T, i, x))
  end
end

inlinePrimitive[tagstring_method] = function (pr, ir, v)
  if ir[v].type isa String
    pr[v] = Expr(:tuple)
  end
end

function string!(ir, s)
  s = push!(ir, stmt(Expr(:tuple), type = s))
  push!(ir, stmt(xcall(part_method, s, 1), type = RString()))
end

outlinePrimitive[tagstring_method] = function (T::Onion)
  ir = IR(meta = FuncInfo(tag"common.core.tagstring"))
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

inlinePrimitive[jsalloc_method] = function (pr, ir, v)
  pr[v] = xtuple()
end

# Core module

function core()
  mod = RModule(tag"common.core")
  foreach(meth -> method!(mod, meth), primitives())
  return mod
end
