# Types

struct Data{N}
  parts::NTuple{N,Any}
end

data(x...) = Data((x...,))

nparts(x::Data) = length(x.parts)-1
part(x::Data, i) = x.parts[i+1]
parts(x) = x.parts[2:end]
allparts(x::Data) = x.parts

Base.getindex(x::Data, i::Integer) = x.parts[i+1]
Base.getindex(x::Data, i::AbstractVector) = data(x.parts[i.+1]...)
Base.lastindex(x::Data) = lastindex(x.parts)
Base.iterate(x::Data, st...) = iterate(x.parts, st...)
Base.:(==)(a::Data, b::Data) = a.parts == b.parts
Base.hash(x::Data, h::UInt) = hash((Data, x.parts), h)

tag(x) = part(x, 0)

struct VData # variable width
  tag::Any
  parts::Any
end

tag(x::VData) = x.tag

rtuple(xs...) = data(:List, xs...)

datacat(x) = x
datacat(x::Data, y::Data) = data(tag(x), parts(x)..., parts(y)...)
datacat(x, y, z, zs...) = datacat(datacat(x, y), z, zs...)

datacat(x::Union{VData,Data}, y::Union{VData,Data}) =
  VData(tag(x), union(partial_eltype(x), partial_eltype(y)))

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
  i == 1 ? data(:JSObject, Int32) :
  error("Tried to access part $i of 1")

allparts(s::Union{String,Type{String}}) = (:String, data(:JSObject, Int32))

nparts(s::Union{String,Type{String}}) = 1

isvalue(x) = false
isvalue(x::Primitive) = true
isvalue(xs::Data) = all(isvalue, parts(xs))

const SimpleType = Union{Primitive,Type{<:Primitive},Data,VData}

# Depth

typedepth(::Unreachable) = 0
typedepth(::Union{Primitive,Type{<:Primitive}}) = 1
typedepth(x::Data) = 1 + maximum(typedepth.(x.parts), init = 0)
typedepth(x::Or) = 1 + maximum(typedepth.(x.patterns), init = 0)

# Subset

issubset(x, ::Unreachable) = false
issubset(::Unreachable, x) = true

issubset(x::Primitive, y::Primitive) = x == y
issubset(x::Primitive, T::Type{<:Primitive}) = x isa T
issubset(x::Type{<:Primitive}, y::Type{<:Primitive}) = x <: y
issubset(x::Union{Primitive,Type{<:Primitive}}, y::Union{Primitive,Data}) = false

issubset(x::Data, y::Data) = nparts(x) == nparts(y) && all(issubset.(x.parts, y.parts))

issubset(x::VData, y::VData) = issubset(tag(x), tag(y)) && issubset(x.parts, y.parts)

issubset(x::Data, y::VData) = issubset(tag(x), tag(y)) && all(issubset.(parts(x), (y.parts,)))
issubset(x::VData, y::Data) = false

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

_recursion_candidate(x::Data) = any(_recursion_candidate.(x.parts))

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

_makerecursive(x::Data) =
  issubset(x, recur()) ? Recur() : data(_makerecursive.(x.parts)...)

_makerecursive(x::Union{Primitive,Type{<:Primitive}}) =
  issubset(x, recur()) ? Recur() : x

function makerecursive(T::Or)
  withrecur(T) do
    Recursive(Or([data(tag(x), _makerecursive.(parts(x))...) for x in T.patterns]))
  end
end

recursive(T) = recursion_candidate(T) ? makerecursive(T) : T

# Printing

vprint(io::IO, x) = show(io, x)

const printers = Dict{Symbol,Any}()

printers[:List] = function (io::IO, s::Data)
  print(io, "[")
  join(io, [sprint(vprint, x) for x in parts(s)], ", ")
  print(io, "]")
end

function printList(io::IO, s::Data)
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

function vprint(io::IO, s::Data)
  isempty(s.parts) && return print(io, "data()")
  haskey(printers, tag(s)) && return printers[tag(s)](io, s)
  print(io, "data(")
  join(io, [sprint(vprint, x) for x in s.parts], ", ")
  print(io, ")")
end

function vprint(io::IO, s::VData)
  print(io, "data(")
  vprint(io, s.tag)
  print(io, ", ")
  vprint(io, s.parts)
  print(io, " ...)")
end

vprint(io::IO, ::Type{T}) where T = print(io, "_: $T")

vprint(io::IO, x::Symbol) = print(io, "`", x, "`")

vprint(io::IO, x::AST.Expr) = print(io, "`", x, "`")

Base.show(io::IO, d::Data) = vprint(io, d)
Base.show(io::IO, d::VData) = vprint(io, d)

# Show inside AST
AST._show(io::AST.Ctx, x::Data) = show(io.io, x)
