module IRTools

using MacroTools
using MacroTools: prewalk, postwalk

include("ir/ir.jl")
include("ir/utils.jl")
include("ir/print.jl")

include("passes.jl")

end # module
