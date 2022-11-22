module WebAssembly

include("wasm.jl")
include("ir.jl")
include("passes.jl")
include("io.jl")

include("passes/multi.jl")

module Instructions

using ..WebAssembly: Instruction, Const, Nop, Local, SetLocal, Op, Select, Convert,
  Block, If, Loop, Branch, Call, Return, Unreachable, nop, unreachable

export Instruction, Const, Nop, Local, SetLocal, Op, Select, Convert,
  Block, If, Loop, Branch, Call, Return, Unreachable, nop, unreachable

end

end # module
