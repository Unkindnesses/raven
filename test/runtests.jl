using Raven, Test

@testset "Raven" begin

@testset "Caches" begin
  include("caches.jl")
end

@testset "WebAssembly" begin
  include("wasm.jl")
end

@testset "Types" begin
  include("types.jl")
end

@testset "Parser" begin
  include("parse.jl")
end

@testset "Caching" begin
  include("caching.jl")
end

@testset "Inference" begin
  include("inference.jl")
end

@testset "Language" begin
  include("language/language.jl")
end

@testset "REPL" begin
  include("repl.jl")
end

if !Sys.iswindows()
  @testset "Debug" begin
    include("debug/runtests.jl")
  end
end

end
