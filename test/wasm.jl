module WASMTest

using Raven, Raven.WebAssembly, Test
using Raven.WebAssembly.Instructions
import Raven.WebAssembly: Func, Mem, Import, Export, Global, leb128, binary
import Raven.WebAssembly: Variable, Locals, Local, SetLocal, Drop, stackshuffle
using Raven: FuncInfo, pscmd, @id_str

function leb128(x)
  io = IOBuffer()
  leb128(io, x)
  seek(io, 0)
  return read(io)
end

@testset "leb128" begin
  @test leb128(UInt32(0)) == [0x00]
  @test leb128(UInt32(1)) == [0x01]
  @test leb128(UInt32(624485)) == [0xE5, 0x8E, 0x26]

  @test leb128(typemax(UInt32)) == [0xff, 0xff, 0xff, 0xff, 0x0f]

  @test leb128(-1) == [0x7f]
  @test leb128(-123456) == [0xC0, 0xBB, 0x78]
end

@testset "stack shuffling" begin
  state = Locals([Variable(6), Variable(3), Variable(2)])
  target = Locals([Variable(3), Variable(4)], Set([Variable(3)]))
  @test stackshuffle(state, target)[1] == [Drop(), SetLocal(true, 3), Local(4)]
  @test stackshuffle(state, target, strict = true)[1] ==
    [Drop(), SetLocal(false, 3), Drop(), Local(3), Local(4)]

  state = Locals([Variable(3)])
  target = Locals([Variable(3), Variable(3)])
  @test stackshuffle(state, target)[1] == [SetLocal(true, 3), Local(3)]

  state = Locals([Variable(1), Variable(2)])
  target = Locals([])
  @test stackshuffle(state, target)[1] == []
  stackshuffle(state, target, strict = true) == [Drop(), Drop()]
end

function compiled_wat(m)
  tmp = Raven.tmp() * ".wasm"
  try
    open(tmp, "w") do io
      binary(io, m, path = "")
    end
    return String(read(pscmd(`wasm2wat $tmp`)))
  finally
    rm(tmp)
  end
end

@testset "binary" begin
  m = WebAssembly.Module()
  @test compiled_wat(m) == "(module)\n"

  m = WebAssembly.Module(mems = [Mem(0)])
  @test occursin("(memory" , compiled_wat(m))

  m = WebAssembly.Module(globals = [Global(0)])
  @test occursin("(global (;0;) (mut i64) (i64.const 0))", compiled_wat(m))

  m = WebAssembly.Module(
    funcs = [Func(:add, [] => [i32], [], Block([Const(Int32(5))]), FuncInfo(id"add"))])
  s = compiled_wat(m)
  @test occursin("func \$add (type 0) (result i32)", s)
  @test occursin("i32.const 5", s)

  m = WebAssembly.Module(
    funcs = [Func(:add, [] => [f64], [], Block([Const(1.0)]), FuncInfo(id"add"))])
  @test occursin("f64.const 0x1p+0", compiled_wat(m))

  m = WebAssembly.Module(
    imports = [Import(:support, :global, :jsglobal, [f32] => [i32])])
  s = compiled_wat(m)
  @test occursin("import \"support\" \"global\"", s)
  @test occursin("(param f32) (result i32)", s)

  m = WebAssembly.Module(
    exports = [Export(:wasmAdd, :add)],
    funcs = [Func(:add, [i32] => [i32], [], Block([Const(Int32(5))]), FuncInfo(id"add"))])
  @test occursin("(export \"wasmAdd\" (func \$add))", compiled_wat(m))
end

end
