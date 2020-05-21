using Raven, Test

cd(@__DIR__)

function result(test)
  Raven.includerv("raven/$test.rv") |> Bool
end

@testset for test in [:pow, :ptr, :relu, :complex]
  @test result(test)
end
