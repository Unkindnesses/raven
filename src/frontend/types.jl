# Symbolic identifiers

struct Id
  path::NTuple{N,Symbol} where N
end

Id(parts::Symbol...) = Id((parts...,))

Base.string(id::Id) = join(id.path, ".")

Base.Symbol(id::Id) = Symbol(string(id))

function Base.show(io::IO, id::Id)
  print(io, "id\"")
  join(io, id.path, ".")
  print(io, "\"")
end

Id(s::String) = Id(Symbol.(split(s, "."))...)

macro id_str(ex)
  Id(ex)
end

# Types

struct Pack{N}
  parts::NTuple{N,Any}
end

pack(x...) = Pack((x...,))

nil = pack(id"Nil")

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

tag(x) = part(x, 0)

struct VPack # variable width
  tag::Any
  parts::Any
end

tag(x::VPack) = x.tag

rlist(xs...) = pack(id"List", xs...)

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

struct Or
  patterns::NTuple{N,Any} where N
  Or(xs) = new((sort(collect(xs), lt = t_isless)...,))
end

# Primitive Types

Primitive = Union{Int64,Int32,Float64,Float32,Id,String}

const JSObject = pack(id"JSObject", pack(id"Ref", pack(id"Ptr", Int32)))
const RString = pack(id"String", JSObject)

const fromSymbol = Dict{Id,Type}()

for T in :[Int64, Int32, Float64, Float32, Id].args
  @eval fromSymbol[$(Id(T))] = $T
  @eval part(x::Union{$T,Type{$T}}, i::Integer) =
          i == 0 ? $(Id(T)) :
          i == 1 ? x :
          error("Tried to access part $i of 1")
  @eval nparts(x::Union{$T,Type{$T}}) = 1
  @eval allparts(x::Union{$T,Type{$T}}) = ($(QuoteNode(T)),x)
end

part(s::String, i::Integer) =
  i == 0 ? id"String" :
  i == 1 ? JSObject :
  error("Tried to access part $i of 1")

allparts(s::String) = (:String, JSObject)

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
typedepth(x::Or) = 1 + maximum(typedepth.(x.patterns), init = 0)
typedepth(x::Recursive) = 1 + typedepth(x.type)
typedepth(x::Recur) = 1

# Order

t_isless(x, y) = repr(x) < repr(y)

# Subset

function issubset(x, y)
  if x == ⊥
    true
  elseif y == ⊥
    false
  elseif x == y
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
  elseif x isa Or
    all(issubset.(x.patterns, (y,)))
  elseif y isa Or
    any(issubset.((x,), y.patterns))
  else
    false
  end
end

# Recursion widening
# This has two passes, checking for candidacy and then converting internal
# subtypes to `Recur`. (Some simple internal subtypes don't trigger widening.)

_recursion_candidate(T, x::Union{Primitive,Type{<:Primitive}}) = false

_recursion_candidate(T, x::Pack) = any(_recursion_candidate.((T,), x.parts))

_recursion_candidate(T, x::Or) =
  issubset(x, T) || any(_recursion_candidate.((T,), x.patterns))

recursion_candidate(T) = false

recursion_candidate(T::Or) = any(_recursion_candidate.((T,), T.patterns))

_makerecursive(T, x::Or) =
  issubset(x, T) ? Recur() : Or(_makerecursive.((T,), x.patterns))

_makerecursive(T, x::Pack) =
  issubset(x, T) ? Recur() : pack(_makerecursive.((T,), x.parts)...)

_makerecursive(T, x::Union{Primitive,Type{<:Primitive}}) =
  issubset(x, T) ? Recur() : x

makerecursive(T::Or) =
  Recursive(Or([pack(tag(x), _makerecursive.((T,), parts(x))...)
                for x in T.patterns]))

recursive(T) = recursion_candidate(T) ? makerecursive(T) : T

# Recursion unrolling

unroll(S, T::Union{Primitive,Type{<:Primitive}}) = T

unroll(S, T::Or) = Or(unroll.((S,), T.patterns))
unroll(S, T::Pack) = Pack(unroll.((S,), T.parts))
unroll(S, T::Recur) = S

unroll(T::Recursive) = unroll(T, T.type)

# Union

typekey(x) = tag(x)
typekey(x::Id) = (id"Id", x)

partial_eltype(x::Pack) = reduce(union, parts(x), init = ⊥)
partial_eltype(x::VPack) = x.parts

union(x::T, y::T) where T<:Number = x == y ? x : T
union(x::T, y::Type{T}) where T<:Number = T
union(x::Type{T}, y::T) where T<:Number = T
union(x::Type{T}, y::Type{T}) where T<:Number = T

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
  elseif y isa Or
    return reduce(union, y.patterns, init = x)
  elseif x isa Or
    typedepth(x) > 10 && error("exploding type: $y")
    ps = x.patterns
    i = findfirst(x -> typekey(x) == typekey(y), ps)
    i == nothing && return Or([ps..., x])
    T = Or([j == i ? union(y, ps[j]) : ps[j] for j = 1:length(ps)])
    return T == x ? T : recursive(T)
  elseif typekey(x) == typekey(y)
    if x isa Id && y isa Id
      return x
    elseif x isa VPack || y isa VPack || nparts(x) != nparts(y)
      return VPack(tag(x), union(partial_eltype(x), partial_eltype(y)))
    else
      return pack(tag(x), [union(part(x, i), part(y, i)) for i = 1:nparts(x)]...)
    end
  else
    return Or([x, y])
  end
end

# Internal symbols

symbolValues(x::Union{Primitive,Type{<:Primitive},Pack}) = []
symbolValues(x::Id) = [x]
symbolValues(x::Or) = reduce(vcat, map(symbolValues, x.patterns))

# Raven value -> compiler type

rvtype(x::Id) = fromSymbol[x]

function rvtype(x::Pack)
  if tag(x) == id"List"
    pack(tag(x), rvtype.(parts(x))...)
  elseif tag(x) == id"Pack"
    pack(rvtype.(parts(x))...)
  elseif tag(x) == id"Literal"
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
