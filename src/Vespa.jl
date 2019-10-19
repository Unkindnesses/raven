module Vespa

using MacroTools: @forward, isexpr
using FunctionalCollections

using IRTools
using IRTools: IR, Variable, Statement, Variable, argument!, block, blocks, arguments, argtypes,
  block, isreturn
using IRTools.Inner: Slot

include("utils.jl")

include("frontend/ast.jl")
include("frontend/parse.jl")
include("frontend/lower.jl")

include("eval/data.jl")
include("eval/interpreter.jl")
include("eval/patterns.jl")
include("eval/abstract.jl")
include("eval/runtime.jl")
include("eval/compiler.jl")

evalfile(joinpath(@__DIR__, "..", "base", "base.vs"))

include("repl.jl")

end # module
