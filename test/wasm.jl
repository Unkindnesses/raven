using Test
import Raven.WebAssembly: leb128

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
