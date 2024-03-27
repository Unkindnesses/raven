module Caches

import Base: getindex, setindex!, delete!, empty!, keys, haskey, get, get!, keytype, valtype
using MacroTools

export @Pipeline, Cache, EagerCache, CycleCache, DualCache, Pipeline

include("utils.jl")
include("basic.jl")
include("cache.jl")
include("pipeline.jl")

end
