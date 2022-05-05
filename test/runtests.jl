using Raven, Test

@testset "Raven" begin

@testset "Parser" begin
  include("parser.jl")
end

@testset "Compiler" begin
  include("compiler.jl")
end

@testset "Reflection" begin
  include("reflection.jl")
end

end
