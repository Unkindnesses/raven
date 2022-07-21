using Raven: @rvx_str, data, rtuple, match, lowerpattern
using Test

let
  pat = lowerpattern(rvx"_").pattern
  @test match(pat, 1) == Dict()
  @test match(pat, :Foo) == Dict()
end

let
  pat = lowerpattern(rvx"1").pattern
  @test match(pat, 1) == Dict()
  @test match(pat, 2) == nothing
end
