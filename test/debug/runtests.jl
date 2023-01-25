using Raven, Test

include("utils.jl")

let
  out = dwarfdump("$(@__DIR__)/../compiled/structures.wasm")
  @test occursin("DW_AT_producer\t(\"raven version 0.0.0\")", out)
  @test occursin("DW_AT_language\t(DW_LANG_C99)", out)

  out = dwarf_verify("$(@__DIR__)/../compiled/structures.wasm")
  @test !occursin("warning", out)
  @test occursin("No errors.", out)
end
