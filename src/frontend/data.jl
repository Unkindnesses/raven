# Types

struct Pack{N}
  parts::NTuple{N,Any}
end

pack(x...) = Pack((x...,))

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

rtuple(xs...) = pack(:List, xs...)

packcat(x) = x
packcat(x::Pack, y::Pack) = pack(tag(x), parts(x)..., parts(y)...)
packcat(x, y, z, zs...) = packcat(packcat(x, y), z, zs...)

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
  patterns::Vector{Any}
end

# Primitive Types

Primitive = Union{Int64,Int32,Float64,Float32,Symbol,String}

for T in :[Int64, Int32, Float64, Float32, Symbol].args
  @eval part(x::Union{$T,Type{$T}}, i::Integer) =
          i == 0 ? $(QuoteNode(T)) :
          i == 1 ? x :
          error("Tried to access part $i of 1")
  @eval nparts(x::Union{$T,Type{$T}}) = 1
  @eval allparts(x::Union{$T,Type{$T}}) = ($(QuoteNode(T)),x)
end

part(s::Union{String,Type{String}}, i::Integer) =
  i == 0 ? :String :
  i == 1 ? pack(:JSObject, Int32) :
  error("Tried to access part $i of 1")

allparts(s::Union{String,Type{String}}) = (:String, pack(:JSObject, Int32))

nparts(s::Union{String,Type{String}}) = 1

isvalue(x) = false
isvalue(x::Primitive) = true
isvalue(xs::Pack) = all(isvalue, parts(xs))

const SimpleType = Union{Primitive,Type{<:Primitive},Pack,VPack}

# Depth

typedepth(::Unreachable) = 0
typedepth(::Union{Primitive,Type{<:Primitive}}) = 1
typedepth(x::Pack) = 1 + maximum(typedepth.(x.parts), init = 0)
typedepth(x::Or) = 1 + maximum(typedepth.(x.patterns), init = 0)

# Subset

issubset(x, ::Unreachable) = false
issubset(::Unreachable, x) = true

issubset(x::Primitive, y::Primitive) = x == y
issubset(x::Primitive, T::Type{<:Primitive}) = x isa T
issubset(x::Type{<:Primitive}, y::Type{<:Primitive}) = x <: y
issubset(x::Union{Primitive,Type{<:Primitive}}, y::Union{Primitive,Pack}) = false

issubset(x::Pack, y::Pack) = nparts(x) == nparts(y) && all(issubset.(x.parts, y.parts))

issubset(x::VPack, y::VPack) = issubset(tag(x), tag(y)) && issubset(x.parts, y.parts)

issubset(x::Pack, y::VPack) = issubset(tag(x), tag(y)) && all(issubset.(parts(x), (y.parts,)))
issubset(x::VPack, y::Pack) = false

issubset(x::Or, y) = all(issubset.(x.patterns, (y,)))
issubset(x, y::Or) = any(issubset.((x,), y.patterns))

issubset(x::Or, ::Unreachable) = false
issubset(::Unreachable, x::Or) = true

issubset(x::Or, y::Or) = invoke(issubset, Tuple{Or,Any}, x, y)

function issubset(x::SimpleType, y::Recursive)
  withrecur(y.type) do
    issubset(x, y.type)
  end
end

function issubset(x::Recursive, y::Recursive)
  withrecur(y.type) do
    issubset(x.type, y.type)
  end
end

issubset(x::Recursive, y::Recur) = issubset(x, Recursive(recur()))

issubset(x::Union{Recursive,Recur}, y::SimpleType) = false

issubset(x::SimpleType, y::Recur) = issubset(x, recur())

issubset(::Recur, ::Recur) = true

# Recursion widening
# This has two passes, checking for candidacy and then converting internal
# subtypes to `Recur`. (Some simple internal subtypes don't trigger widening.)

_recursion_candidate(x::Union{Primitive,Type{<:Primitive}}) = false

_recursion_candidate(x::Pack) = any(_recursion_candidate.(x.parts))

_recursion_candidate(x::Or) =
  issubset(x, recur()) || any(_recursion_candidate.(x.patterns))

recursion_candidate(T) = false

function recursion_candidate(T::Or)
  withrecur(T) do
    any(_recursion_candidate.(T.patterns))
  end
end

_makerecursive(x::Or) =
  issubset(x, recur()) ? Recur() : Or(_makerecursive.(x.patterns))

_makerecursive(x::Pack) =
  issubset(x, recur()) ? Recur() : pack(_makerecursive.(x.parts)...)

_makerecursive(x::Union{Primitive,Type{<:Primitive}}) =
  issubset(x, recur()) ? Recur() : x

function makerecursive(T::Or)
  withrecur(T) do
    Recursive(Or([pack(tag(x), _makerecursive.(parts(x))...) for x in T.patterns]))
  end
end

recursive(T) = recursion_candidate(T) ? makerecursive(T) : T

# Recursion unrolling

function unroll(T::Recursive, n = 1)
  n == 0 && return T
  withrecur(T) do
    unroll(T.type, n)
  end
end

unroll(T::Union{Primitive,Type{<:Primitive}}, n) = T

unroll(T::Or, n) = typeof(T)(unroll.(T.patterns, n))
unroll(T::Pack, n) = Pack(unroll.(T.parts, n))
unroll(T::Recur, n) = unroll(recur(), n-1)

# Printing

vprint(io::IO, x) = show(io, x)

const printers = Dict{Symbol,Any}()

printers[:List] = function (io::IO, s::Pack)
  print(io, "[")
  join(io, [sprint(vprint, x) for x in parts(s)], ", ")
  print(io, "]")
end

function printList(io::IO, s::Pack)
  print(io, "list(")
  while tag(s) == :Prepend
    vprint(io, part(s, 2))
    tag(part(s, 1)) == :Prepend && print(io, ", ")
    s = part(s, 1)
  end
  print(io, ")")
end

printers[:Prepend] = printList
printers[:Empty] = printList

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

vprint(io::IO, x::Symbol) = print(io, "`", x, "`")

vprint(io::IO, x::AST.Expr) = print(io, "`", x, "`")

Base.show(io::IO, d::Pack) = vprint(io, d)
Base.show(io::IO, d::VPack) = vprint(io, d)

# Show inside AST
AST._show(io::AST.Ctx, x::Pack) = show(io.io, x)
