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
  @eval $op(x::RType, y::RType) = RType($op(atom(x), atom(y)))
end

for (op, f) in [(:eq, ==), (:ne, !=), (:gt_u, >), (:lt_u, <),
                (:ge_u, >=), (:le_u, <=), (:gt_s, >), (:lt_s, <),
                (:ge_s, >=), (:le_s, <=)]
  T = endswith(string(op), "_s") ? Int64 : UInt64
  @eval $op(x::Bits{N}, y::Bits{N}) where N = Bits{1}($f($T(x), $T(y)))
  @eval $op(x::RType, y::RType) = RType($op(atom(x), atom(y)))
end

# Core primitives – pack, packcat, part and nparts – are dealt with in
# IR expansion, but we define type inference here, and implement some
# simple built-in functions.

function Int64(x::RType)
  @assert atom(tag(x)) == tag"common.Int"
  isvalue(x) ? Int64(atom(part(x, 1))) : Int64
end

partial_part(data::RType, i::RType) =
  isfield(data, :union) ? reduce(union, partial_part.(data.union, (i,))) :
  isfield(data, :recursive) ? partial_part(unroll(data), i) :
  partial_part(data, Int64(i))

function partial_part(data::RType, i::Integer)
  if isatom(data) || isfield(data, :pack)
    0 <= i <= nparts(data) ? part(data, i) : ⊥
  elseif isfield(data, :vpack)
    i == 0 ? tag(data) : partial_eltype(data)
  else
    error("unimplemented")
  end
end

# TODO: HACK: we assume index != 0 when indexing dynamically.
# Should instead have a seperate `index` function that enforces this.
partial_part(data::RType, i::Type{Int64}) = partial_eltype(data)

partial_nparts(x::RType) =
  isfield(x, :union) ? reduce(union, RType.(partial_nparts.(x.union))) :
  isfield(x, :recursive) ? partial_nparts(unroll(x)) :
  isfield(x, :vpack) ? RType(Int64) :
  RType(nparts(x))

partial_widen(x::RType) =
  isatom(x) ? abstract(x) :
  isfield(x, :pack) && tag(x) in RType.((tag"common.Int", tag"common.Bool")) ? pack(tag(x), abstract(part(x, 1))) :
  error("unimplemented")

symbolValues(x::RType) =
  isfield(x, :recursive) ? symbolValues(unroll(x)) :
  isfield(x, :tag) ? Set([x]) :
  isfield(x, :union) ? reduce(union, symbolValues.(x.union)) :
  Set{Tag}()

# Fast, approximate equality check; basically a stand-in for pointer equality.
# TODO extend to handle VPack
partial_shortcutEquals(a, b) =
  isvalue(a) && isvalue(b) ? RType(a == b) :
  !isempty(intersect(symbolValues(a), symbolValues(b))) ? RType(Bool) :
  RType(false)

partial_bitsize(x::RType) = RType(Int64(x.bits[1]))

partial_bitcast(::ValOrType{Bits{N}}, x::Bits) where N = Bits{N}(UInt64(x))
partial_bitcast(::ValOrType{Bits{N}}, x::Type{<:Bits}) where N = Bits{N}
partial_bitcast(a::RType, b::RType) = RType(partial_bitcast(atom(a), atom(b)))

partial_bitcast_s(::ValOrType{Bits{N}}, x::Bits) where N = Bits{N}(Int64(x))
partial_bitcast_s(::ValOrType{Bits{N}}, x::Type{<:Bits}) where N = Bits{N}
partial_bitcast_s(a::RType, b::RType) = RType(partial_bitcast_s(atom(a), atom(b)))

partial_bitop(::ValOrType{Bits{N}}, x::ValOrType{Bits{N}}) where N = Bits{N}
partial_bitop(x::RType, y::RType) = RType(partial_bitop(atom(x), atom(y)))

partial_bitcmp(::ValOrType{Bits{N}}, x::ValOrType{Bits{N}}) where N = Bits{1}
partial_bitcmp(x::RType, y::RType) = RType(partial_bitcmp(atom(x), atom(y)))

partial_biteqz(x::Bits) = Bits{1}(x.value == 0)
partial_biteqz(::Type{<:Bits}) = Bits{1}
partial_biteqz(x::RType) = RType(partial_biteqz(atom(x)))

# Needed by dispatchers, since a user-defined method would need runtime matching
# to deal with unions.
partial_isnil(x::RType) =
  x == nil ? RType(true) :
  issubset(nil, x) ? RType(Bool) :
  RType(false)

function partial_notnil(x::RType)
  !issubset(nil, x) ? x :
  isfield(x, :pack) ? (x == nil ? ⊥ : x) :
  isfield(x, :union) ? onion(filter(x -> x != nil, x.union)...) :
  isfield(x, :recursive) ? recursive(partial_notnil(unroll(x))) :
  @assert false
end

function partial_tagcast(x::RType, t::RType)
  @assert isvalue(t)
  x = unroll(x)
  isfield(x, :union) || return (tag(x) == t ? x : ⊥)
  ps = filter(x -> tag(x) == t, x.union)
  return isempty(ps) ? ⊥ : only(ps)
end

partial_tagstring(x::RType) = RString()

function rvtype(x::RType)
  @assert isvalue(x)
  if isfield(x, :tag)
    error("unimplemented")
  elseif isatom(x)
    abstract(x)
  elseif tag(x) == RType(tag"common.List")
    pack(tag(x), rvtype.(parts(x))...)
  elseif tag(x) == RType(tag"common.Pack")
    pack(rvtype.(parts(x))...)
  elseif tag(x) == RType(tag"common.Literal")
    return part(x, 1)
  else
    error("Unrecognised type $x")
  end
end

partial_function(f, I, O) = RType(Int32)
partial_invoke(f, I, O, xs...) = rvtype(O)

partial_jsalloc() = RType(options().jsalloc)

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

isnil_method = RMethod(tag"common.core.nil?", lowerpattern(rvx"[x]"), partial_isnil, true)
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
  if isatom(T) && isvalue(T)
    pr[v] = atom(T)
  elseif tag(T) in RType.((tag"common.Int", tag"common.Bool"))
    pr[v] = atom(part(T, 1))
  else
    pr[v] = x
  end
end

inlinePrimitive[bitsize_method] = function (pr, ir, v)
  pr[v] = xtuple()
end

function mask(T, x)
  m = bits(layout(T))((1 << nbits(T)) - 1)
  stmt(xcall(WType(layout(T)).and, x, m), type = layout(T))
end

function extend!(pr, v, T, x)
  S = bits(layout(T))
  shift = S(nbits(S) - nbits(T))
  x = insert!(pr, v, stmt(xcall(WType(layout(T)).shl, x, shift), type = RType(S)))
  x = insert!(pr, v, stmt(xcall(WType(layout(T)).shr_s, x, shift), type = RType(S)))
end

inlinePrimitive[bitcast_method] = function (pr, ir, v)
  if isvalue(ir[v].type)
    pr[v] = xtuple()
    return
  end
  x = ir[v].expr.args[3]
  F = atom(exprtype(ir, x))
  T = atom(exprtype(ir, v))
  if (layout(T), layout(F)) == (Int32, Int64)
    x = insert!(pr, v, stmt(xcall(i32.wrap_i64, x), type = RType(Bits{32})))
  elseif (layout(T), layout(F)) == (Int64, Int32)
    x = insert!(pr, v, stmt(xcall(i64.extend_i32_u, x), type = RType(Bits{64})))
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
  F = atom(exprtype(ir, x))
  T = atom(exprtype(ir, v))
  nbits(T) <= nbits(F) && return inlinePrimitive[bitcast_method](pr, ir, v)
  if nbits(F) < nbits(layout(F))
    x = extend!(pr, v, F, x)
  end
  if (nbits(layout(T)), nbits(layout(F))) == (64, 32)
    x = insert!(pr, v, stmt(xcall(i64.extend_i32_s, x), type = RType(Bits{64})))
  end
  if nbits(T) < nbits(layout(T))
    x = insert!(pr, v, mask(T, x))
  end
  pr[v] = x
  return
end

for op in bitops
  @eval inlinePrimitive[$(Symbol(:bit, op, :_method))] = function (pr, ir, v)
    T = atom(exprtype(ir, v))
    if T isa Bits
      pr[v] = xtuple()
      return
    end
    x, y = ir[v].expr.args[2:end]
    sz = nbits(layout(T))
    if $(endswith(string(op), "_s")) && nbits(T) < nbits(layout(T))
      x = extend!(pr, v, T, x)
      y = extend!(pr, v, T, y)
    end
    r = insert!(pr, v, stmt(xcall(WType(layout(T)).$op, x, y), type = RType(T)))
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
    T = atom(union(exprtype(ir, [x, y])...))
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

symoverlap(x::RType, y::RType) =
  isfield(x, :tag) && isfield(y, :union) ? [i for (i, y) in enumerate(y.union) if x == y] :
  isfield(x, :union) && isfield(y, :tag) ? symoverlap(y, x) :
  error("unimplemented")

inlinePrimitive[shortcutEquals_method] = function (pr, ir, v)
  if isvalue(ir[v].type)
    pr[v] = Expr(:tuple)
  else # symbol case
    a, b = ir[v].expr.args[2:3]
    A, B = exprtype(ir, [a, b])
    isfield(B, :union) && ((a, A, b, B) = (b, B, a, A))
    ov = symoverlap(A, B)
    length(ov) == 1 || error("not implemented")
    i = insert!(pr, v, stmt(Expr(:ref, a, 1), type = RType(Bits{32})))
    pr[v] = xcall(i32.eq, i, Bits{32}(ov[1]))
  end
end

inlinePrimitive[isnil_method] = function (pr, ir, v)
  x = ir[v].expr.args[2]
  T = exprtype(ir, x)
  if isvalue(ir[v].type)
    pr[v] = xtuple()
  else
    i = findfirst(==(nil), T.union)
    j = insert!(pr, v, stmt(Expr(:ref, x, 1), type = RType(Bits{32})))
    pr[v] = xcall(i32.eq, j, Bits{32}(i))
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
    @assert isfield(T, :union) && !isfield(ir[v].type, :union)
    i = findfirst(x -> x != nil, T.union)
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
    if isfield(T, :recursive)
      T = unroll(T)
      x = unbox!(pr, T, x)
    end
    i = findfirst(x -> Raven.tag(x) == tag, T.union)
    replace!(pr, v, union_downcast!(pr, T, i, x))
  end
end

inlinePrimitive[tagstring_method] = function (pr, ir, v)
  if isvalue(ir[v].type)
    pr[v] = Expr(:tuple)
  end
end

function string!(ir, s::String)
  id = push!(ir, stmt(Expr(:ref, s), type = rlist(Int32)))
  push!(ir, stmt(xcall(tag"common.JSObject", id), type = RString()))
end

inlinePrimitive[tagstring_method] = function (pr, ir, v)
  T = exprtype(ir, ir[v].expr.args[2])
  if isfield(T, :tag)
    delete!(pr, v)
    s = string!(pr, string(atom(T)::Tag))
    replace!(pr, v, s)
  end
end

outlinePrimitive[tagstring_method] = function (T)
  @assert isfield(T, :union)
  ir = IR(meta = FuncInfo(tag"common.core.tagstring"))
  x = argument!(ir, type = T)
  union_cases!(ir, T, x) do S, _
    string!(ir, string(atom(S)::Tag))
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
