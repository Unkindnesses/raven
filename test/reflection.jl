using Raven, Test
using Raven: @tag_str, @src_str

f = src"""
  fn pow(x, n: Int64) {
    r = one(x)
    while true {
      if n == 0 {
        return r
      }
      n = n - one(n)
      r = r * x
    }
  }

  pow(widen(2), widen(3))
  """

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
  @test length(ci) == 1
end

let
  ci = Raven.code_wasm(f, tag"pow")
  @test ci isa AbstractDict
  @test length(ci) == 1
end
