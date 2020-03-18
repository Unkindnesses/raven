module Raven

using MacroTools: @forward, isexpr
using FunctionalCollections

using IRTools, WebAssembly
using IRTools: IR, Variable, Statement, Variable, argument!, block, blocks, arguments, argtypes,
  block, isreturn, branches
using IRTools.Inner: Slot

include("frontend/ast.jl")
include("eval/data.jl")

include("frontend/parse.jl")
include("frontend/lower.jl")

include("eval/interpreter.jl")
include("eval/patterns.jl")
include("eval/runtime.jl")
include("eval/abstract.jl")
include("eval/compiler.jl")

include("backend/passes.jl")
include("backend/wasm.jl")

evalfile(joinpath(@__DIR__, "..", "base", "base.rv"))

include("repl.jl")

end # module
