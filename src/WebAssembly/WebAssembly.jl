module WebAssembly

include("wasm.jl")
include("ir.jl")
include("binary.jl")

module Instructions

for x in :[
  Instruction, Const, Nop, Local, SetLocal, Op, Select, Convert, Block,
  If, Loop, Branch, Call, Return, Unreachable, nop, unreachable,
  i32, i64, f32, f64
].args
  @eval using ..WebAssembly: $x
  @eval export $x
end

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
