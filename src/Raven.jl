module Raven

using MacroTools: @forward, isexpr
using FunctionalCollections

using IRTools, WebAssembly
using IRTools: IR, Variable, Statement, Variable, argument!, block, blocks, arguments, argtypes,
  block, isreturn, branches, prune!, return!
using IRTools.Inner: Slot

include("frontend/ast.jl")
include("frontend/parse.jl")
include("frontend/data.jl")
include("frontend/lower.jl")
include("frontend/modules.jl")
include("frontend/load.jl")

include("eval/patterns.jl")
include("eval/interpreter.jl")
include("eval/abstract.jl")

include("backend/passes.jl")
include("backend/wasm.jl")
include("backend/js.jl")

include("repl.jl")

end # module
