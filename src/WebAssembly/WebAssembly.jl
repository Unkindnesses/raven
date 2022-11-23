module WebAssembly

include("wasm.jl")
include("ir.jl")
include("binary.jl")

module Instructions

using ..WebAssembly: Instruction, Const, Nop, Local, SetLocal, Op, Select, Convert,
  Block, If, Loop, Branch, Call, Return, Unreachable, nop, unreachable

export Instruction, Const, Nop, Local, SetLocal, Op, Select, Convert,
  Block, If, Loop, Branch, Call, Return, Unreachable, nop, unreachable

end

"""
    write_wat(filename, m::Module)

Write the WebAssembly module `m` to WebAssembly text format in `filename`.
"""
function write_wat(filename, m::Module)
  open(filename, "w") do f
    show(f, m)
  end
end

end # module
