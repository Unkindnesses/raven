module Raven

include("Dwarf/Dwarf.jl")
include("WebAssembly/WebAssembly.jl")

using MacroTools: @q, @forward, isexpr

using IRTools, .Dwarf, .WebAssembly
using IRTools: IR, Variable, Statement, Variable, argument!, block, blocks, arguments, argtypes,
  block, isreturn, branches, prune!, return!, branch!, block!, stmt, predecessors, successors
using IRTools.Inner: Slot

include("frontend/ast.jl")
include("frontend/parse.jl")
include("frontend/types.jl")
include("frontend/modules.jl")
include("frontend/lower.jl")

include("match.jl")

include("middle/utils.jl")
include("middle/loop.jl")
include("middle/primitives.jl")
include("middle/patterns.jl")
include("middle/abstract.jl")
include("middle/lower.jl")
include("middle/refcount.jl")
include("middle/load.jl")

include("backend/wasm.jl")
include("backend/js.jl")

include("reflection.jl")

end # module
