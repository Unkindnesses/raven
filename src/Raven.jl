module Raven

include("Unions/Unions.jl")
include("IRTools/IRTools.jl")
include("Caches/Caches.jl")
include("Dwarf/Dwarf.jl")
include("WebAssembly/WebAssembly.jl")

using Base64, JSON, Sockets
using MacroTools: @q, @forward, isexpr

using .Unions, .Caches, .IRTools, .Dwarf, .WebAssembly
using .Unions: isfield, field
using .IRTools: IR, Variable, Statement, Variable, argument!, block, blocks,
  arguments, argtypes, block, isreturn, isunreachable, branches, prune!,
  return!, unreachable!, branch!, block!, stmt, predecessors, successors, Slot,
  isvariable, Source, liveness, liveness_after, WorkQueue
using .Caches: reset!, iscached, cached, fingerprint

import Base: ==, hash

include("utils.jl")

include("frontend/types.jl")
include("frontend/ast.jl")
include("frontend/parse.jl")
include("frontend/modules.jl")
include("frontend/lower.jl")

include("middle/utils.jl")
include("middle/loop.jl")
include("middle/primitives.jl")
include("middle/patterns.jl")
include("middle/interpret.jl")
include("middle/abstract.jl")
include("middle/inference.jl")
include("middle/expand.jl")
include("middle/inline.jl")
include("middle/refcount.jl")
include("middle/load.jl")

include("backend/wasm.jl")
include("backend/compiler.jl")
include("backend/repl.jl")

include("test.jl")

include("repl-mode.jl")

function __init__()
  name = "raven-repl-$(rand(UInt64))"
  global addr = Sys.iswindows() ? "\\\\.\\pipe\\$name" : joinpath(tempdir(), name)
  global server = listen(addr)
end

end # module
