using Vespa, Test

@testset "Vespa" begin

@testset "Parser" begin
  include("parser.jl")
end

@testset "Interpreter" begin
  include("interpret.jl")
end

end
