using Raven, Test

f = joinpath(@__DIR__, "raven/pow.rv")

let
  ci = Raven.code_lowered(f, tag"pow")
  @test ci isa AbstractDict
  @test length(ci) == 1
end

let
  ci = Raven.code_typed(f, tag"pow")
  @test ci isa AbstractDict
  @test length(ci) == 2
end

let
  ci = Raven.code_final(f, tag"pow")
  @test ci isa AbstractDict
  @test length(ci) == 2
end

let
  ci = Raven.code_wasm(f, tag"pow")
  @test ci isa AbstractDict
  @test length(ci) == 2
end
