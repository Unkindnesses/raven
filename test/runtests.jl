using Raven, Test

@testset "Raven" begin

@testset "Parser" begin
  include("parser.jl")
end

# @testset "Interpreter" begin
#   include("interpret.jl")
# end

@testset "Patterns" begin
  include("patterns.jl")
end

end
