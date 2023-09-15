module Caches

import Base: getindex, setindex!, delete!, haskey, get, get!, keytype, valtype

export Cache, EagerCache

include("utils.jl")
include("basic.jl")
include("cache.jl")

end
