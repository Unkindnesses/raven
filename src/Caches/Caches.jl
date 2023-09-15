module Caches

import Base: getindex, setindex!, delete!, haskey, get, get!, keytype, valtype

export Cache, EagerCache, Pipeline

include("utils.jl")
include("basic.jl")
include("cache.jl")
include("pipeline.jl")

end
