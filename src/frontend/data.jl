# Types

struct Data{N}
  parts::NTuple{N,Any}
end

data(x...) = Data((x...,))

nparts(x::Data) = length(x.parts)-1
part(x::Data, i) = x.parts[i+1]
parts(x) = x.parts[2:end]

Base.getindex(x::Data, i::Integer) = x.parts[i+1]
Base.getindex(x::Data, i::AbstractVector) = data(x.parts[i.+1]...)
Base.lastindex(x::Data) = lastindex(x.parts)
Base.iterate(x::Data, st...) = iterate(x.parts, st...)
Base.:(==)(a::Data, b::Data) = a.parts == b.parts
Base.hash(x::Data, h::UInt) = hash((Data, x.parts), h)

tag(x) = part(x, 0)

const rnothing = data(:Nothing)

rtuple(xs...) = data(:Tuple, xs...)

datacat(x) = x
datacat(x, xs...) = data(x.parts..., reduce(vcat, collect.(parts.(xs)))...)

# Abstract Types

struct Unreachable end
const ⊥ = Unreachable()

Base.show(io::IO, ::Unreachable) = print(io, "⊥")

Primitive = Union{Int64,Int32,Float64,Float32,Symbol,String}

# Primitive Types
for T in :[Int64, Int32, Float64, Float32, Symbol].args
  @eval part(x::Union{$T,Type{$T}}, i::Integer) =
          i == 0 ? $(QuoteNode(T)) :
          i == 1 ? x :
          error("Tried to access part $i of 2")
  @eval nparts(x::$T) = 2
end

part(s::String, i::Integer) =
  i == 0 ? :String :
  i == 1 ? data(:JSObject, Int32) :
  error("Tried to access part $i of 2")

isvalue(x) = false
isvalue(x::Primitive) = true
isvalue(xs::Data) = all(isvalue, parts(xs))

# Printing

vprint(io::IO, x) = show(io, x)

function vprint(io::IO, s::Data)
  print(io, "data(")
  join(io, [sprint(vprint, x) for x in s.parts], ", ")
  print(io, ")")
end

vprint(io::IO, ::Type{T}) where T = print(io, "::$T")

vprint(io::IO, x::Symbol) = print(io, "`", x, "`")

vprint(io::IO, x::Expr) = print(io, "`", x, "`")

Base.show(io::IO, d::Data) = vprint(io, d)

# Show inside AST
_show(io::Ctx, x::Data) = show(io.io, x)
