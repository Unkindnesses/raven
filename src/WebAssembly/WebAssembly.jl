module WebAssembly

using ..Dwarf
using MacroTools: @forward
using ..Dwarf: LineInfo

include("wasm.jl")
include("wat.jl")
include("binary.jl")
include("ir.jl")

module Instructions

for x in :[
  Instruction, Const, Nop, Local, SetLocal, Op, Select, Convert, Block,
  Loop, Branch, Call, Return, Unreachable, nop, unreachable,
  i32, i64, f32, f64
].args
  @eval using ..WebAssembly: $x
  @eval export $x
end

end

end # module
