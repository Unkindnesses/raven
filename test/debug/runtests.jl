using Raven, Test

include("utils.jl")

Raven.compile("$(@__DIR__)/src/pow.rv")

pow_wasm = "$(@__DIR__)/src/pow.wasm"

@testset "Basic info" begin
  out = dwarfdump(pow_wasm)
  @test occursin("DW_AT_producer\t(\"raven version 0.0.0\")", out)
  @test occursin("DW_AT_language\t(DW_LANG_C99)", out)
end

@testset "Verify" begin
  out = dwarf_verify(pow_wasm)
  @test !occursin("warning", out)
  @test occursin("No errors.", out)
  out = dwarf_verify_lines(pow_wasm)
  @test !occursin("warning", out)
  @test occursin("No errors.", out)
end

@testset "Line info" begin
  i = only(callsites(pow_wasm, "pow:1")) - code_offset(pow_wasm)
  lt = linetable(pow_wasm)
  li = lineinfo(lt, i)
  @test endswith(li.file, "test/debug/src/pow.rv")
  @test li.line == 10
  @test li.column == 12
end
