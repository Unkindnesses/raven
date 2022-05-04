module Raven

using MacroTools: @forward, isexpr
using FunctionalCollections

using IRTools, WebAssembly
using IRTools: IR, Variable, Statement, Variable, argument!, block, blocks, arguments, argtypes,
  block, isreturn, branches, prune!, return!, branch!, block!
using IRTools.Inner: Slot

include("frontend/ast.jl")
include("frontend/parse.jl")
include("frontend/data.jl")
include("frontend/modules.jl")
include("frontend/lower.jl")

include("middle/primitives.jl")
include("middle/patterns.jl")
include("middle/abstract.jl")
include("middle/lower.jl")
include("middle/load.jl")

include("backend/wasm.jl")
include("backend/js.jl")

end # module
