module IRTools

using MacroTools
using MacroTools: @q, prewalk, postwalk
import ..IRTools

export @code_ir

include("ir/ir.jl")
include("ir/utils.jl")
include("ir/print.jl")

include("passes.jl")

end # module
