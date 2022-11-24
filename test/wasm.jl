module WASMTest

using Raven, Raven.WebAssembly, Test
using Raven.WebAssembly.Instructions
import Raven.WebAssembly: Func, Mem, Import, leb128, binary
using Raven: pscmd

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

function compiled_wat(m)
  tmp = Raven.tmp() * ".wasm"
  try
    open(tmp, "w") do io
      binary(io, m)
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

  m = WebAssembly.Module(
    funcs = [Func(:add, [], [i32], [], Block([Const(Int32(5))]))])
  s = compiled_wat(m)
  @test occursin("(func (result i32))", s)
  @test occursin("i32.const 5", s)

  m = WebAssembly.Module(
    funcs = [Func(:add, [], [f64], [], Block([Const(1.0)]))])
  @test occursin("f64.const 0x1p+0", compiled_wat(m))

  m = WebAssembly.Module(
    imports = [Import(:support, :global, :jsglobal, :func, [f32], [i32])])
  s = compiled_wat(m)
  @test occursin("import \"support\" \"global\"", s)
  @test occursin("(param f32) (result i32)", s)
end

end
