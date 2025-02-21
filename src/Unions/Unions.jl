module Unions

export @union

isunion(::Type) = false

include("union.jl")

end
