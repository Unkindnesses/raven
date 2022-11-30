using Raven, Test

@testset "Raven" begin

@testset "WebAssembly" begin
  include("wasm.jl")
end

@testset "Parser" begin
  include("parser.jl")
end

@testset "Types" begin
  include("types.jl")
end

@testset "Compiler" begin
  include("compiler.jl")
end

@testset "Dwarf" begin
  include("dwarf.jl")
end

@testset "Inference" begin
  include("inference.jl")
end

@testset "Reflection" begin
  include("reflection.jl")
end

@testset "Matching" begin
  include("match.jl")
end

end
