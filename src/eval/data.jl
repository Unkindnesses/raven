# Types

struct Data{N}
  parts::NTuple{N,Any}
end

data(x...) = Data((x...,))

nparts(x::Data) = length(x.parts)-1
part(x::Data, i) = x.parts[i+1]

Base.:(==)(a::Data, b::Data) = a.parts == b.parts
Base.hash(x::Data, h::UInt) = hash((Data, x.parts), h)

tag(x) = part(x, 0)

const rnothing = data(:Nothing)

rtuple(xs...) = data(:Tuple, xs...)

Primitive = Union{Int64,Int32,Float64,Float32,Symbol,String}

# Primitive Types
for T in :[Int64, Int32, Float64, Float32, Symbol, String].args
  @eval part(x::$T, i::Integer) =
          i == 0 ? $(QuoteNode(T)) :
          i == 1 ? x :
          error("Tried to access part $i of 2")
  @eval nparts(x::$T) = 2
end

# Abstract Types

struct Hole end

struct PrimitiveHole{T} end

isprimitive(x::PrimitiveHole{T}, ::Type{T}) where T = true

struct Bottom end
const ⊥ = Bottom()

# Printing

function vprint(io::IO, s::Data)
  print(io, "data(")
  join(io, [sprint(vprint, x) for x in s.parts], ", ")
  print(io, ")")
end

vprint(io, x::Symbol) = print(io, "`", x, "`")
vprint(io::IO, x::Union{Int64, Int32, Float64, Float32, String}) = show(io, x)

vprint(io::IO, x::Expr) = print(io, "`", x, "`")
