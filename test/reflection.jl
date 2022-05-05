using Raven, Test

f = joinpath(@__DIR__, "raven/pow.rv")

let
  ci = Raven.code_lowered(f, :pow)
  @test ci isa AbstractDict
  @test length(ci) == 1
end

let
  ci = Raven.code_typed(f, :pow)
  @test ci isa AbstractDict
  @test length(ci) == 2
end

let
  ci = Raven.code_final(f, :pow)
  @test ci isa AbstractDict
  @test length(ci) == 2
end

let
  ci = Raven.code_wasm(f, :pow)
  @test ci isa AbstractDict
  @test length(ci) == 2
end
