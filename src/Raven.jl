module Raven

include("Caches/Caches.jl")
include("IRTools/IRTools.jl")
include("Dwarf/Dwarf.jl")
include("WebAssembly/WebAssembly.jl")

using MacroTools: @q, @forward, isexpr

using .Caches, .IRTools, .Dwarf, .WebAssembly
using .IRTools: IR, Variable, Statement, Variable, argument!, block, blocks, arguments, argtypes,
  block, isreturn, isunreachable, branches, prune!, return!, unreachable!, branch!, block!, stmt, predecessors, successors,
  Slot, isvariable, Source
using .Caches: reset!

include("frontend/tag.jl")
include("frontend/ast.jl")
include("frontend/parse.jl")
include("frontend/types.jl")
include("frontend/modules.jl")
include("frontend/lower.jl")

include("middle/utils.jl")
include("middle/loop.jl")
include("middle/primitives.jl")
include("middle/patterns.jl")
include("middle/abstract.jl")
include("middle/expand.jl")
include("middle/inline.jl")
include("middle/refcount.jl")
include("middle/load.jl")

include("backend/wasm.jl")
include("backend/compiler.jl")

include("reflection.jl")
include("test.jl")

end # module
