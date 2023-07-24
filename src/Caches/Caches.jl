module Caches

import Base: getindex, setindex!, delete!, haskey, get

export Cache

include("utils.jl")
include("basic.jl")
include("cache.jl")

end
