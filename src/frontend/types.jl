# Bottom

struct Unreachable end
const ⊥ = Unreachable()

Base.show(io::IO, ::Unreachable) = print(io, "⊥")

# Tags

struct Tag
  path::NTuple{N,Symbol} where N
end

Tag(t::Tag) = t
Tag(parts::Symbol...) = Tag((parts...,))
Tag(a::Tag, b::Tag) = Tag((a.path..., b.path...))
Tag(a, b) = Tag(Tag(a), Tag(b))

Base.string(id::Tag) = join(id.path, ".")

Base.Symbol(id::Tag) = Symbol(string(id))

function Base.show(io::IO, id::Tag)
  print(io, "tag\"")
  join(io, id.path, ".")
  print(io, "\"")
end

Tag(s::String) = Tag(Symbol.(split(s, ".", keepempty=false))...)

path(t::Tag) = Tag(t.path[1:end-1]...)
name(t::Tag) = t.path[end]

macro tag_str(ex)
  Tag(ex)
end

function modtag(mod::Tag, tag::String)
  prefix = startswith(tag, ".") ? mod : tag""
  return Tag(prefix, tag)
end

# Bits

struct Bits{N}
  value::UInt64
  Bits{N}(x::UInt64) where N = new(x & (UInt64(1)<<N-1))
end

Bits{N}(x::Int64) where N = Bits{N}(reinterpret(UInt64, x))
Bits{N}(x::Int32) where N = Bits{N}(Int64(x))
Bits{N}(x::Bool) where N = Bits{N}(UInt64(x))

ValOrType{T} = Union{T,Type{<:T}}

nbits(::ValOrType{Bits{N}}) where N = N

Base.UInt64(x::Bits) = x.value
Base.UInt32(x::Bits) = UInt32(x.value)
Base.Int64(x::Bits{N}) where N = reinterpret(Int64, x.value << (64 - N)) >> (64 - N)

Base.bitstring(x::Bits) = bitstring(UInt64(x))[end-nbits(x)+1:end]

function Base.show(io::IO, bs::Bits{N}) where N
  print(io, "Bits{$N}($(Int64(bs)))")
end

bits(x::ValOrType{Bits}) = x
bits(x::Type{Float32}) = Bits{32}
bits(x::Type{Float64}) = Bits{64}
bits(x::Float32) = Bits{32}(UInt64(reinterpret(UInt32, x)))
bits(x::Float64) = Bits{64}(reinterpret(UInt64, x))
bits(x::Type{Int32}) = Bits{32}
bits(x::Type{Int64}) = Bits{64}
bits(x::Union{Int32,Int64}) = bits(typeof(x))(x)

nbits(::Type{Int32}) = 32
nbits(::Type{Int64}) = 64

WebAssembly.WType(::Type{Bits{64}}) = i64
WebAssembly.WType(::Type{Bits{32}}) = i32

WebAssembly.Const(x::Bits) =
  nbits(x) <= 32 ? WebAssembly.Const(UInt32(x)) : WebAssembly.Const(UInt64(x))

# Types

@union struct RType
  tag::String
  bits::Tuple{UInt8,Union{UInt64,Nothing}}
  f32::Union{Float32,Nothing}
  f64::Union{Float64,Nothing}
  pack::Vector{RType}
  vpack::Tuple{RType,RType}
  union::Vector{RType}
  recurrence::Nothing
  recursive::RType
end

const RAnno = Union{RType,Unreachable}

function Base.hash(T::RType, h::UInt64)
  Unions.dispatch(T) do f
    hash((f, Unions.unsafe_getproperty(T, f)), h ⊻ 0x4bd213eedb18202d)
  end
end

RType(x::RType) = x

RType(x::Tag) = RType(tag = string(x))
RType(x::Bits) = RType(bits = (nbits(x),UInt64(x)))
RType(x::Float32) = RType(f32 = x)
RType(x::Float64) = RType(f64 = x)

RType(::Type{Float32}) = RType(f32 = nothing)
RType(::Type{Float64}) = RType(f64 = nothing)
RType(::Type{Bits{N}}) where N = RType(bits = (N,nothing))

pack(parts::Vector{RType}) = RType(pack = parts)
pack(xs...) = pack(RType[RType(x) for x in xs])
vpack(t, xs) = RType(vpack = (RType(t), RType(xs)))
recursive(x) = RType(recursive = RType(x))
recurrence = RType(recurrence = nothing)

pack(T::RType, x::RType) =
  T == RType(tag"common.core.Float32") && isfield(x, :bits) && x.bits[1] == 32 ?
    RType(isvalue(x) ? reinterpret(Float32, x.bits[2] % UInt32) : Float32) :
  T == RType(tag"common.core.Float64") && isfield(x, :bits) && x.bits[1] == 64 ?
    RType(isvalue(x) ? reinterpret(Float64, x.bits[2]) : Float64) :
  pack([T, x])

function onion(xs::Vector{RType})
  @assert !isempty(xs)
  length(xs) == 1 && return xs[1]
  @assert !any(x -> isfield(x, :union), xs)
  RType(union = sort!(xs, by = sortkey))
end

onion(xs...) = onion([RType(x) for x in xs])

isatom(x::RType) = field(x) in (:tag, :bits, :f32, :f64)

atom(x::RType) =
  isfield(x, :tag) ? Tag(x.tag) :
  isfield(x, :bits) ? (isnothing(x.bits[2]) ? Bits{Int(x.bits[1])} : Bits{Int(x.bits[1])}(x.bits[2])) :
  isfield(x, :f32) ? (isnothing(x.f32) ? Float32 : x.f32) :
  isfield(x, :f64) ? (isnothing(x.f64) ? Float64 : x.f64) :
  error("not an atom type")

bits(x::RType) = RType(bits(atom(x)))

abstract(x::RType) =
  isfield(x, :bits) ? RType(bits = (x.bits[1], nothing)) :
  isfield(x, :f32) ? RType(Float32) :
  isfield(x, :f64) ? RType(Float64) :
  error("not an atom type")

isvalue(x::RType) =
  isfield(x, :tag) ||
  (isfield(x, :bits) && !isnothing(x.bits[2])) ||
  (isfield(x, :f32) && !isnothing(x.f32)) ||
  (isfield(x, :f64) && !isnothing(x.f64)) ||
  (isfield(x, :pack)) && all(isvalue, x.pack)

tag(x::RType)::RType =
  isfield(x, :tag) ? RType(tag"common.core.Tag") :
  isfield(x, :bits) ? RType(tag"common.core.Bits") :
  isfield(x, :f32) ?  RType(tag"common.core.Float32") :
  isfield(x, :f64) ?  RType(tag"common.core.Float64") :
  isfield(x, :pack) ? x.pack[1] :
  isfield(x, :vpack) ? x.vpack[1] :
  isfield(x, :recursive) ? tag(x.recursive) :
  error("No fixed tag for $(field(x)) type")

allparts(x::RType) =
  isfield(x, :tag, :bits) ? [tag(x), x] :
  isfield(x, :f32, :f64) ? [tag(x), bits(x)] :
  isfield(x, :pack) ? x.pack :
  error("No fixed parts for $(field(x)) type")

part(x::RType, i::Integer) = allparts(x)[i+1]

nparts(x::RType) = length(allparts(x))-1

parts(x::RType) = allparts(x)[2:end]

packcat(x) = x

function packcat(x, y)
  x, y = unroll.((x, y))
  z = isfield(x, :vpack) || isfield(y, :vpack) ?
    vpack(tag(x), union(partial_eltype(x), partial_eltype(y))) :
  pack(tag(x), parts(x)..., parts(y)...)
  return recur(z)
end

packcat(x, y, z...) = packcat(packcat(x, y), z...)

function sortkey(x::RType)
  if isfield(x, :tag)
    (tag(x).tag, x.tag)
  elseif isfield(x, :bits)
    (tag(x).tag, x.bits[1])
  elseif isfield(x, :union)
    sortkey(x.union[1])
  elseif isfield(x, :recursive)
    sortkey(x.recursive)
  else
    key = tag(x)
    isfield(key, :tag) ? (key.tag,) : ()
  end
end

function typekey(x::RType, r = Set{Any}())
  if isfield(x, :union)
    foreach(x -> typekey(x, r), x.union)
  elseif isfield(x, :recursive)
    typekey(x.recursive, r)
  else
    push!(r, sortkey(x))
  end
  return r
end

overlapping(a::RType, b::RType) = !Base.isdisjoint(typekey(a), typekey(b))

# Common types

nil = pack(tag"common.Nil")

RType(::Type{Bool}) = pack(tag"common.Bool", Bits{1})
RType(x::Bool) = pack(tag"common.Bool", Bits{1}(x))

RType(::Type{Int32}) = pack(tag"common.Int", Bits{32})
RType(x::Int32) = pack(tag"common.Int", Bits{32}(x))

RType(::Type{Int64}) = pack(tag"common.Int", Bits{64})
RType(x::Int64) = pack(tag"common.Int", Bits{64}(x))

RPtr() = pack(tag"common.Ptr", Int32)

JSObject() =
  options().jsalloc ?
    pack(tag"common.JSObject", pack(tag"common.Ref", RPtr())) :
    pack(tag"common.JSObject", Int32)

RString() = pack(tag"common.String", JSObject())
RType(::Type{String}) = RString()

rlist(xs...) = pack(tag"common.List", xs...)

# Subset

function postwalk(f::F, x::RType) where F
  inner(x) = postwalk(f, x)
  y = if isatom(x) || isfield(x, :recursive, :recurrence)
    x
  elseif isfield(x, :pack)
    parts = RType[]
    for p in x.pack
      q = inner(p)
      q === ⊥ && return ⊥
      push!(parts, q)
    end
    pack(parts)
  elseif isfield(x, :vpack)
    tag = inner(x.vpack[1])
    parts = inner(x.vpack[2])
    parts == ⊥ ? pack(tag) : vpack(tag, parts::RType)
  elseif isfield(x, :union)
    ts = RType[]
    for t in x.union
      s = inner(t)
      s === ⊥ && continue
      isfield(s, :union) ? append!(ts, s.union) : push!(ts, s)
    end
    onion(ts)
  else
    @assert false
  end
  return f(y)::RAnno
end

function _unroll(T::RType, S::RAnno = T)
  if isfield(T, :recursive)
    postwalk(T.recursive) do x
      isfield(x, :recurrence) ? S : x
    end
  elseif isfield(T, :union)
    onion([x for S in T.union for x in disjuncts(_unroll(S))])
  else
    T
  end
end

function _issubset(self::T, x::RType, y::RType)::Bool where T
  issubset(x, y) = _issubset(self, x, y)
  if isfield(x, :recursive) || isfield(y, :recursive)
    self[(_unroll(x), _unroll(y))]
  elseif isfield(x, :union)
    all(issubset(T, y) for T in x.union)
  elseif isfield(y, :union)
    any(issubset(x, T) for T in y.union)
  elseif isatom(x) && isatom(y)
    x == y ||
    field(x) == field(y) && !isvalue(y) &&
      (!isfield(x, :bits) || x.bits[1] == y.bits[1])
  elseif isfield(x, :pack) && isfield(y, :pack)
    nparts(x) == nparts(y) && all(issubset.(x.pack, y.pack))
  elseif isfield(x, :pack) && isfield(y, :vpack)
    issubset(tag(x), tag(y)) && all(issubset.(x.pack[2:end], (y.vpack[2],)))
  elseif isfield(x, :vpack) && isfield(y, :vpack)
    issubset(tag(x), tag(y)) && issubset(x.vpack[2], y.vpack[2])
  else
    false
  end
end

function subsetter()
  fp = Fixpoint{Tuple{RType,RType},Bool}(_ -> true) do self, (x, y)
    _issubset(self, x, y)
  end
  (x, y) -> fp[(x, y)]
end

issubset(x::RType, y::RType) = subsetter()(x, y)
issubset(::RType, ::Unreachable) = false
issubset(::Unreachable, ::Unreachable) = true

# Disjoint

disjuncts(x::RType) = isfield(x, :union) ? x.union : RType[x]

function _isdisjoint(self::T, x::RType, y::RType)::Bool where T
  isdisjoint(x, y) = _isdisjoint(self, x, y)
  if isfield(x, :recursive) || isfield(y, :recursive)
    self[(_unroll(x), _unroll(y))]
  elseif isfield(x, :union) || isfield(y, :union)
    all(isdisjoint(x, y) for x in disjuncts(x) for y in disjuncts(y))
  elseif isatom(x) && isatom(y)
    field(x) != field(y) ||
    isvalue(x) && isvalue(y) && x != y ||
    isfield(x, :bits) && x.bits[1] != y.bits[1]
  elseif isfield(x, :pack) && isfield(y, :pack)
    nparts(x) != nparts(y) || any(isdisjoint.(x.pack, y.pack))
  elseif isfield(x, :pack) && isfield(y, :vpack)
    isdisjoint(tag(x), tag(y)) || any(isdisjoint.(x.pack[2:end], (y.vpack[2],)))
  elseif isfield(x, :vpack) && isfield(y, :pack)
    isdisjoint(y, x)
  elseif isfield(x, :vpack) && isfield(y, :vpack)
    isdisjoint(tag(x), tag(y))
  else
    true
  end
end

function disjointer()
  fp = Fixpoint{Tuple{RType,RType},Bool}(_ -> false) do self, (x, y)
    _isdisjoint(self, x, y)
  end
  (x, y) -> fp[(x, y)]
end

isdisjoint(x, y) = disjointer()(x, y)

function _isdistinct(self, x, y; isdisjoint)
  isdistinct(x, y) = _isdistinct(self, x, y; isdisjoint)
  result = if isfield(x, :recursive) || isfield(y, :recursive)
    self[(_unroll(x), _unroll(y))]
  elseif isfield(x, :union) || isfield(y, :union)
    all(isdistinct(x, y) for x in disjuncts(x) for y in disjuncts(y)) &&
    count(!isdisjoint(x, y) for x in disjuncts(x) for y in disjuncts(y)) < 2
  elseif isfield(x, :pack) && isfield(y, :pack)
    nparts(x) < 1 || isdisjoint(x, y) || all(isdistinct.(x.pack, y.pack))
  elseif isfield(x, :pack) && isfield(y, :vpack)
    nparts(x) < 1 || isdisjoint(x, y) ||
      (isdistinct(tag(x), tag(y)) && all(isdistinct(x, partial_eltype(y)) for x in x.pack))
  elseif isfield(x, :vpack) && isfield(y, :pack)
    isdistinct(y, x)
  elseif isfield(x, :vpack) && isfield(y, :vpack)
    isdisjoint(tag(x), tag(y)) || isdisjoint(x.vpack[2], y.vpack[2])
  else
    true
  end
  return result
end

function distincter()
  isdisjoint = disjointer()
  fp = Fixpoint{Tuple{RType,RType},Bool}(_ -> false) do self, (x, y)
    _isdistinct(self, x, y; isdisjoint)
  end
  (x, y) -> fp[(x, y)]
end

isdistinct(x, y) = distincter()(x, y)

# Union

function finite(T::RType, depth = 1)
  if isfield(T, :union)
    onion([d for x in T.union for d in disjuncts(finite(x, depth))])
  elseif isfield(T, :recursive)
    term = _unroll(T, ⊥)
    isdistinct(T, term) && (term = _unroll(T, term))
    for i = 1:depth
      term = _unroll(T, term)
    end
    term
  else
    T
  end
end

finite(::Unreachable) = ⊥

partial_eltype(x::RType; union = union) =
  isfield(x, :pack) ? reduce(union, x.pack[2:end], init = ⊥) :
  isfield(x, :vpack) ? x.vpack[2] :
  error("No eltype for $(field(x)) type")

function splitby(f, xs)
  ts = empty(xs)
  fs = empty(xs)
  for x in xs
    push!(f(x) ? ts : fs, x)
  end
  return ts, fs
end

function _union(x::RAnno, y::RAnno; self = _union)::RAnno
  x, y = finite.((x, y))
  if x === ⊥
    return y
  elseif y === ⊥
    return x
  elseif isfield(x, :union) || isfield(y, :union)
    ys = RType[]
    for x in vcat(disjuncts(y), disjuncts(x))
      while true
        xs, ys = splitby(y -> overlapping(x, y), ys)
        isempty(xs) && break
        x = reduce((x, y) -> _union(x, y; self), xs, init = x)
      end
      append!(ys, disjuncts(x))
    end
    return onion(ys)
  elseif sortkey(x) != sortkey(y)
    return onion(x, y)
  elseif isatom(x) && isatom(y)
    return x == y ? x : abstract(x)
  elseif isfield(x, :vpack) || isfield(y, :vpack) || nparts(x) != nparts(y)
    t = self(tag(x), tag(y))
    t === ⊥ && return ⊥
    parts = self(partial_eltype(x; union = self),
                 partial_eltype(y; union = self))
    parts === ⊥ && return pack(t)
    z = vpack(t, parts)
    return z
  else
    return pack([self(x.pack[i], y.pack[i]) for i = 1:length(x.pack)]...)
  end
end

# Reroll

function reconstruct(x::RType)
  parts::Vector{RType} =
    isfield(x, :pack) ? x.pack :
    isfield(x, :vpack) ? [x.vpack...] :
    isfield(x, :union) ? x.union :
    RType[]
  re(xs::Vector{RType}) =
    isfield(x, :pack) ? pack(xs) :
    isfield(x, :vpack) ? vpack(xs[1], xs[2]) :
    isfield(x, :union) ? onion(xs) :
    x
  return parts, re
end

Base.occursin(x::RType, y::RType) =
  x == y ||
  !isfield(y, :recursive, :recurrence) &&
    any(y -> occursin(x, y), reconstruct(y)[1])

isrecursive(x) =
  isfield(x, :recursive) || isfield(x, :union) && any(isrecursive, x.union)

function rfinite(T)
  postwalk(T) do t
    isfield(t, :recursive) ? finite(t) : t
  end
end

isrecur(x, T) = !isdistinct(x, T) && issubset(rfinite(x), T)

function reroll_inner(T, x; self = reroll, seen)::Tuple{RType,Set{Any}}
  if isfield(x, :recursive)
    x in seen && return x, Set()
    y, ks = reroll_outer(T, _unroll(x); seen = Set([seen..., x]))
    isempty(ks) && return x, Set()
    occursin(x, y) && return x, Set()
    return y, ks
  else
    xs, re = reconstruct(x)
    ys = self.((T,), xs; seen)
    ch = isfield(x, :union) ? [d for y in first.(ys) for d in disjuncts(y)] :
      first.(ys)
    return re(ch), reduce(Base.union!, second.(ys), init = Set())
  end
end

function reroll_outer(T, x; seen)::Tuple{RType,Any}
  if isfield(x, :union)
    reroll_inner(T, x, self = reroll_inner; seen)
  else
    reroll_inner(T, x; seen)
  end
end

reroll(T, x; seen)::Tuple{RType,Any} =
  isrecur(x, T) ? (recurrence, typekey(x)) :
  reroll_outer(T, x; seen)

function reroll(T)
  isrecursive(T) && return T
  xs = disjuncts(T)
  ys = reroll_inner.((T,), xs; seen = Set())
  ys = [(x, Base.union(k1, k2)) for ((x, k1), k2) in zip(ys, typekey.(xs))]
  xs = Tuple{RType,Set{Any}}[]
  # Group by typekey in common
  for (x, ks) in ys
    as, bs = splitby(((_, k),) -> !Base.isdisjoint(ks, k), xs)
    x = onion(x, [d for a in first.(as) for d in disjuncts(a)]...)
    ks = reduce(Base.union, second.(as), init = ks)
    xs = [bs..., (x, ks)]
  end
  xs = [occursin(recurrence, x) ? recursive(x) : x for x in first.(xs)]
  return onion(xs...)
end

function unroll_inner(S, T)
  if isfield(T, :recursive)
    T
  elseif isfield(T, :recurrence)
    S
  elseif isfield(T, :union)
    onion([unroll_inner(S, d) for d in disjuncts(T)])
  else
    xs, re = reconstruct(T)
    re(unroll.((S,), xs))
  end
end

unroll(S, T) = reroll(unroll_inner(S, T))

function unroll(T)
  if isfield(T, :recursive)
    unroll_inner(T, T.recursive)
  elseif isfield(T, :union)
    onion([x for S in disjuncts(T) for x in disjuncts(unroll(S))])
  else
    T
  end
end

# Lift
# (type to merge, subset present, recursion present)

function lift_inner(T, x; self = lift, seen)
  if isfield(x, :recursive)
    x in seen && return ⊥, false, true
    inner, s, r = lift_outer(T, _unroll(x); seen = Set([seen..., x]))
    s && r ? (x, true, false) : (inner, s, false)
  else
    xs, _ = reconstruct(x)
    ys = self.((T,), xs; seen)
    reduce(_union, first.(ys), init = ⊥), any(second.(ys)), any(third.(ys))
  end
end

function lift_outer(T, x; seen)
  if isfield(x, :union)
    lift_inner(T, x; self = lift_inner, seen)
  else
    lift_inner(T, x; seen)
  end
end

lift(T, x; seen) =
  !isdistinct(x, T) ? (issubset(x, T) ? ⊥ : x, true, false) :
  lift_outer(T, x; seen)

lift(T) = lift_outer(T, T; seen = Set())[1]

# Recursive

function recur_inner(T; self)
  if isfield(T, :union)
    onion([recur_inner(x; self) for x in disjuncts(T)])
  else
    xs, re = reconstruct(T)
    return re(self.(xs))
  end
end

function _recur(T; self = identity)
  if isfield(T, :vpack) || isfield(T, :union) || isfield(T, :recursive)
    R = reroll(recur_inner(_union(_unroll(T), lift(T)); self))
    issubset(R, T) ? R : self(R)
  else
    recur_inner(_unroll(T); self)
  end
end

struct TypeError
  name::String
end

function recurser()
  check(old, new) = issubset(old, new) || throw(TypeError("subset"))
  fp = Fixpoint{RType,RType}(identity; check) do self, T
    _recur(T, self = T -> self[T])
  end
  return T -> fp[T]
end

recur(T) = recurser()(T)

const union_cache = Dict{Tuple{RType,RType},RType}()

function Base.union(x::RType, y::RType)
  sortkey(x) <= sortkey(y) || ((x, y) = (y, x))
  get!(() -> recur(_union(x, y)), union_cache, (x, y))
end

Base.union(x::RType, ::Unreachable) = x
Base.union(::Unreachable, y::RType) = y
Base.union(::Unreachable, ::Unreachable) = ⊥

# Display

function Base.show(io::IO, x::RType)
  if isfield(x, :tag)
    print(io, "tag")
    show(io, x.tag)
  elseif isfield(x, :bits)
    N, x = x.bits
    print(io, "bits")
    if isnothing(x)
      print(io, " ", Int64(N))
    else
      print(io, "\"$(bitstring(Bits{N}(x)))\"")
    end
  elseif isfield(x, :f32)
    print(io, isnothing(x.f32) ? "Float32" : "$(x.f32)f0")
  elseif isfield(x, :f64)
    print(io, isnothing(x.f64) ? "Float64" : x.f64)
  elseif isfield(x, :pack)
    if tag(x) == RType(tag"common.Int")
      N = nbits(atom(part(x, 1)))
      if isvalue(x)
        val = Int64(x)
        nbits(atom(part(x, 1))) == 64 ? print(io, val) : print(io, "oftype(int $N, $val)")
      else
        print(io, "int ", N)
      end
    elseif tag(x) == RType(tag"common.List")
      print(io, "[")
      join(io, x.pack[2:end], ", ")
      print(io, "]")
    else
      print(io, "pack(")
      join(io, x.pack, ", ")
      print(io, ")")
    end
  elseif isfield(x, :vpack)
    if tag(x) == RType(tag"common.List")
      print(io, "[", x.vpack[2], " ...]")
    else
      print(io, "vpack(", x.vpack[1], ", ", x.vpack[2], " ...)")
    end
  elseif isfield(x, :union)
    join(io, x.union, " | ")
  elseif isfield(x, :recurrence)
    print(io, "T")
  elseif isfield(x, :recursive)
    print(io, "(T = ", x.recursive, ")")
  else
    @assert false
  end
end
