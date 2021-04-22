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

include("middle/load.jl")
include("middle/patterns.jl")
include("middle/interpreter.jl")
include("middle/abstract.jl")

include("backend/passes.jl")
include("backend/wasm.jl")
include("backend/js.jl")

end # module
