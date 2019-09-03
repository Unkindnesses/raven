# Types

struct Struct
  data::Vector{Any}
end

vstruct(x...) = Struct(Any[x...])

nparts(x::Struct) = length(x.data)-1
part(x::Struct, i) = x.data[i+1]

Base.:(==)(a::Struct, b::Struct) = a.data == b.data

tag(x) = part(x, 0)

const vnothing = vstruct(:Nothing)

Primitive = Union{Int,Float64,Symbol,String}

# Primitive Types
for T in :[Int64, Float64, Symbol, String].args
  @eval part(x::$T, i::Integer) =
          i == 0 ? $(QuoteNode(T)) :
          i == 1 ? x :
          error("Tried to access part $i of 2")
  @eval nparts(x::$T) = 2
end

# Printing

function vprint(io::IO, s::Struct)
  print(io, "struct(")
  join(io, [sprint(vprint, x) for x in s.data], ", ")
  print(io, ")")
end

vprint(io, x::Symbol) = print(io, "`", x, "`")
vprint(io::IO, x::Union{Int64, Float64}) = print(io, x)

vprint(io::IO, x::Expr) = print(io, "`", x, "`")
