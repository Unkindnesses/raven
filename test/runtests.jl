using Raven, Test

@testset "Raven" begin

@testset "WebAssembly" begin
  include("wasm.jl")
end

@testset "Types" begin
  include("types.jl")
end

@testset "Parser" begin
  include("parse.jl")
end

@testset "Compiler" begin
  include("compiler.jl")
end

if !Sys.iswindows()
  @testset "Debug" begin
    include("debug/runtests.jl")
  end
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
