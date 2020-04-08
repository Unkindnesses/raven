using Raven, Test

cd(@__DIR__)

function result(test)
  Raven.compile("raven/$test.rv")
  String(read(`node raven/$test.js`))
end

passes(test) = parse(Bool, result(test))

@testset for test in [:pow, :ptr, :relu, :complex, :memory]
  @test passes(test)
end
