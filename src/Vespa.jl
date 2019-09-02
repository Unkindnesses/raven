module Vespa

using MacroTools: @forward, isexpr
using FunctionalCollections

using IRTools
using IRTools: IR, Slot, Variable, argument!, block, blocks, arguments

include("frontend/ast.jl")
include("frontend/parse.jl")
include("frontend/lower.jl")

include("eval/interpreter.jl")
include("eval/runtime.jl")
include("eval/patterns.jl")

include("repl.jl")

end # module
