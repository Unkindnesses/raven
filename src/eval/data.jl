# Types

struct Data{N}
  parts::NTuple{N,Any}
end

data(x...) = Data((x...,))

nparts(x::Data) = length(x.parts)-1
part(x::Data, i) = x.parts[i+1]

Base.getindex(x::Data, i::Integer) = x.parts[i+1]
Base.getindex(x::Data, i::AbstractVector) = data(x.parts[i.+1]...)
Base.lastindex(x::Data) = lastindex(x.parts)
Base.iterate(x::Data, st...) = iterate(x.parts, st...)
Base.:(==)(a::Data, b::Data) = a.parts == b.parts
Base.hash(x::Data, h::UInt) = hash((Data, x.parts), h)

tag(x) = part(x, 0)

const rnothing = data(:Nothing)

rtuple(xs...) = data(:Tuple, xs...)

# Abstract Types

struct Hole{T} end

isprimitive(x::Hole{T}, ::Type{T}) where T = true

struct Bottom end
const ⊥ = Bottom()

jtype(x::Hole{T}) where T = T

Primitive = Union{Int64,Int32,Float64,Float32,Symbol,String}
jtype(x::Primitive) = typeof(x)

# Primitive Types
for T in :[Int64, Int32, Float64, Float32, Symbol, String].args
  @eval part(x::Union{$T,Hole{$T}}, i::Integer) =
          i == 0 ? $(QuoteNode(T)) :
          i == 1 ? x :
          error("Tried to access part $i of 2")
  @eval nparts(x::$T) = 2
end

# Printing

vprint(io::IO, x) = show(io, x)

function vprint(io::IO, s::Data)
  print(io, "data(")
  join(io, [sprint(vprint, x) for x in s.parts], ", ")
  print(io, ")")
end

vprint(io::IO, ::Hole{T}) where T = print(io, "::$T")

vprint(io::IO, x::Symbol) = print(io, "`", x, "`")

vprint(io::IO, x::Expr) = print(io, "`", x, "`")

Base.show(io::IO, d::Data) = vprint(io, d)
