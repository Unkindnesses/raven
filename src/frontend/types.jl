# Types

struct Pack{N}
  parts::NTuple{N,Any}
  Pack(x...) = new{length(x)}(x)
end

pack(x...) = Pack(x...)

nil = pack(tag"common.Nil")

nparts(x::Pack) = length(x.parts)-1
part(x::Pack, i) = x.parts[i+1]
parts(x) = x.parts[2:end]
allparts(x::Pack) = x.parts

Base.getindex(x::Pack, i::Integer) = x.parts[i+1]
Base.getindex(x::Pack, i::AbstractVector) = pack(x.parts[i.+1]...)
Base.lastindex(x::Pack) = lastindex(x.parts)
Base.iterate(x::Pack, st...) = iterate(x.parts, st...)
Base.:(==)(a::Pack, b::Pack) = a.parts == b.parts
Base.hash(x::Pack, h::UInt) = hash((Pack, x.parts), h)

tag(x) = partial_part(x, 0)

struct VPack # variable width
  tag::Any
  parts::Any
end

vpack(tag, parts) = VPack(tag, parts)

tag(x::VPack) = x.tag

rlist(xs...) = pack(tag"common.List", xs...)

packcat(x) = x
packcat(x, y) = pack(tag(x), parts(x)..., parts(y)...)
packcat(x, y, z, zs...) = packcat(packcat(x, y), z, zs...)

packcat(x::Pack, y::Pack) = invoke(packcat, Tuple{Any,Any}, x, y)

packcat(x::Union{VPack,Pack}, y::Union{VPack,Pack}) =
  VPack(tag(x), union(partial_eltype(x), partial_eltype(y)))

struct Recursive
  type::Any
end

struct Recur end

Base.show(io::IO, T::Recursive) = print(io, "T = ", T.type)
Base.show(io::IO, ::Recur) = print(io, "T")

Base.:(==)(a::Recursive, b::Recursive) = a.type == b.type

# Abstract Types

struct Unreachable end
const ⊥ = Unreachable()

Base.show(io::IO, ::Unreachable) = print(io, "⊥")

struct Onion
  types::NTuple{N,Any} where N
  function Onion(xs)
    length(xs) == 1 && return only(xs)
    @assert !isempty(xs) && !any(x -> x isa Onion, xs)
    return new((sort(collect(xs), lt = t_isless)...,))
  end
end

function Base.show(io::IO, or::Onion)
  for i = 1:length(or.types)
    i == 1 || print(io, " | ")
    show(io, or.types[i])
  end
end

function splittags(f, x::Onion)
  tags = filter(t -> t isa Tag, x.types)
  t = filter(t -> !(t isa Tag), x.types)
  result = Any[f(t) for t in tags]
  isempty(t) || push!(result, f(Onion(t)))
  return Onion(result)
end

pack(ts::Onion, xs...) = splittags(tag -> Pack(tag, xs...), ts)
vpack(ts::Onion, xs) = splittags(tag -> VPack(tag, xs), ts)

# Primitive Types

Primitive = Union{Int64,Int32,Float64,Float32,Tag,String}

JSObject() =
  options().jsalloc ?
    pack(tag"common.JSObject", pack(tag"common.Ref", pack(tag"common.Ptr", Int32))) :
    pack(tag"common.JSObject", Int32)

RString() = pack(tag"common.String", JSObject())

const fromSymbol = Dict{Tag,Type}()

for T in :[Int64, Int32, Float64, Float32, Tag].args
  local tag = Tag(tag"common.core", T)
  @eval fromSymbol[$tag] = $T
  @eval part(x::Union{$T,Type{$T}}, i::Integer) =
          i == 0 ? $tag :
          i == 1 ? x :
          error("Tried to access part $i of 1")
  @eval nparts(x::Union{$T,Type{$T}}) = 1
  @eval allparts(x::Union{$T,Type{$T}}) = ($tag, x)
end

part(s::String, i::Integer) =
  i == 0 ? tag"common.String" :
  i == 1 ? JSObject() :
  error("Tried to access part $i of 1")

allparts(s::String) = (tag"common.String", JSObject())

nparts(s::String) = 1

isvalue(x) = false
isvalue(x::Primitive) = true
isvalue(xs::Pack) = all(isvalue, parts(xs))

const SimpleType = Union{Primitive,Type{<:Primitive},Pack,VPack}

# Depth

typedepth(::Unreachable) = 0
typedepth(::Union{Primitive,Type{<:Primitive}}) = 1
typedepth(x::Pack) = 1 + maximum(typedepth.(x.parts), init = 0)
typedepth(x::VPack) = 1 + typedepth(x.parts)
typedepth(x::Onion) = 1 + maximum(typedepth.(x.types), init = 0)
typedepth(x::Recursive) = 1 + typedepth(x.type)
typedepth(x::Recur) = 1

# Order

_repr(x) = repr(x)
_repr(x::Int32) = "$x::Int32"

t_isless(x, y) = _repr(x) < _repr(y)

# Subset

function issubset(x, y)
  if x == ⊥
    true
  elseif y == ⊥
    false
  elseif x === y
    true
  elseif x isa Primitive && y isa Type{<:Primitive}
    x isa y
  elseif x isa Pack && y isa Pack
    nparts(x) == nparts(y) && all(issubset.(x.parts, y.parts))
  elseif x isa Pack && y isa VPack
    issubset(tag(x), tag(y)) && all(issubset.(parts(x), (y.parts,)))
  elseif x isa VPack && y isa VPack
    issubset(tag(x), tag(y)) && issubset(x.parts, y.parts)
  elseif x isa Recursive && y isa Recursive
    issubset(x.type, y)
  elseif x isa Recur && y isa Recursive
    true
  elseif x isa Recursive
    issubset(unroll(x), y)
  elseif y isa Recursive
    issubset(x, unroll(y))
  elseif x isa Onion
    all(issubset.(x.types, (y,)))
  elseif y isa Onion
    any(issubset.((x,), y.types))
  else
    false
  end
end

# Recursion widening
# This has two passes, checking for candidacy and then converting internal
# subtypes to `Recur`. (Some simple internal subtypes don't trigger widening.)

_recursion_candidate(T, x::Union{Primitive,Type{<:Primitive}}) = false

_recursion_candidate(T, x::Pack) = any(_recursion_candidate.((T,), parts(x)))

_recursion_candidate(T, x::Onion) =
  issubset(x, T) || any(_recursion_candidate.((T,), x.types))

# TODO: unhack
_recursion_candidate(T, x::Recursive) = false

_recursion_candidate(T, x::VPack) =
  (T isa VPack && issubset(x, T)) || _recursion_candidate(T, x.parts)

recursion_candidate(T) = false

recursion_candidate(T::Onion) = any(_recursion_candidate.((T,), T.types))

recursion_candidate(T::VPack) = _recursion_candidate(T, T.parts)

_makerecursive(T, x::Onion) =
  issubset(x, T) ? Recur() : Onion(_makerecursive.((T,), x.types))

_makerecursive(T, x::Pack) =
  issubset(x, T) ? Recur() : pack(_makerecursive.((T,), x.parts)...)

_makerecursive(T, x::VPack) =
  issubset(x, T) ? Recur() : VPack(x.tag, _makerecursive(T, x.parts))

_makerecursive(T, x::Union{Primitive,Type{<:Primitive}}) =
  issubset(x, T) ? Recur() : x

makerecursive(T::Onion) =
  Recursive(Onion([x isa Pack ? pack(tag(x), _makerecursive.((T,), parts(x))...) :
                   x isa VPack ? VPack(tag(x), _makerecursive(T, x.parts)) :
                   x
                   for x in T.types]))

makerecursive(T::VPack) = Recursive(VPack(T.tag, _makerecursive(T, T.parts)))

const enable_recursion = Ref(true)

recursive(T) =
  enable_recursion[] && recursion_candidate(T) ? makerecursive(T) : T

# Recursion unrolling

unroll(S, T::Union{Primitive,Type{<:Primitive}}) = T

unroll(S, T::Onion) = Onion(unroll.((S,), T.types))
unroll(S, T::Pack) = Pack(unroll.((S,), T.parts)...)
unroll(S, T::VPack) = VPack(tag(T), unroll(S, T.parts))
unroll(S, T::Recur) = S

unroll(T::Recursive) = unroll(T, T.type)

# Union

tokey(x::Tag) = x
tokey(x) = nothing

typekey(x) = tokey(tag(x))
typekey(x::Tag) = (tag(x), x)

partial_eltype(x::Pack) = reduce(union, parts(x), init = ⊥)
partial_eltype(x::VPack) = x.parts

union(x::T, y::T) where T<:Number = x == y ? x : T
union(x::T, y::Type{T}) where T<:Number = T
union(x::Type{T}, y::T) where T<:Number = T
union(x::Type{T}, y::Type{T}) where T<:Number = T
union(x::String, y::String) = x == y ? x : RString()

function union(x, y)
  if x == ⊥
    return y
  elseif y == ⊥
    return x
  elseif x isa Recursive
    @assert issubset(y, x)
    return x
  elseif y isa Recursive
    @assert issubset(x, y)
    return y
  elseif y isa Onion
    return reduce(union, y.types, init = x)
  elseif x isa Onion
    ps = x.types
    i = findfirst(x -> typekey(x) === typekey(y), ps)
    i == nothing && return Onion((ps..., y))
    T = Onion(j == i ? union(y, ps[j]) : ps[j] for j = 1:length(ps))
    return T == x ? T : recursive(T)
  elseif typekey(x) === typekey(y)
    if x isa Tag && y isa Tag
      return x
    elseif x isa VPack || y isa VPack || nparts(x) != nparts(y)
      return recursive(VPack(union(tag(x), tag(y)), union(partial_eltype(x), partial_eltype(y))))
    else
      return pack(union(tag(x), tag(y)), (union(part(x, i), part(y, i)) for i = 1:nparts(x))...)
    end
  else
    return recursive(Onion((x, y)))
  end
end

# Internal symbols

symbolValues(x::Union{Primitive,Type{<:Primitive},Pack}) = []
symbolValues(x::Tag) = [x]
symbolValues(x::Onion) = reduce(vcat, map(symbolValues, x.types))

# Raven value -> compiler type

rvtype(x::Tag) = fromSymbol[x]

function rvtype(x::Pack)
  if tag(x) == tag"common.List"
    pack(tag(x), rvtype.(parts(x))...)
  elseif tag(x) == tag"common.Pack"
    pack(rvtype.(parts(x))...)
  elseif tag(x) == tag"common.Literal"
    return part(x, 1)
  else
    error("Unrecognised type $x")
  end
end

# Printing

vprint(io::IO, x) = show(io, x)

const printers = Dict{Symbol,Any}()

printers[:List] = function (io::IO, s::Pack)
  print(io, "[")
  join(io, [sprint(vprint, x) for x in parts(s)], ", ")
  print(io, "]")
end

printers[:Pair] = function (io, s)
  vprint(io, part(s, 1))
  print(io, " => ")
  vprint(io, part(s, 2))
end

function vprint(io::IO, s::Pack)
  isempty(s.parts) && return print(io, "pack()")
  haskey(printers, tag(s)) && return printers[tag(s)](io, s)
  print(io, "pack(")
  join(io, [sprint(vprint, x) for x in s.parts], ", ")
  print(io, ")")
end

function vprint(io::IO, s::VPack)
  print(io, "pack(")
  vprint(io, s.tag)
  print(io, ", ")
  vprint(io, s.parts)
  print(io, " ...)")
end

vprint(io::IO, ::Type{T}) where T = print(io, "_: $T")

vprint(io::IO, x::AST.Expr) = print(io, "`", x, "`")

Base.show(io::IO, d::Pack) = vprint(io, d)
Base.show(io::IO, d::VPack) = vprint(io, d)

# Show inside AST
AST._show(io::AST.Ctx, x::Pack) = show(io.io, x)
