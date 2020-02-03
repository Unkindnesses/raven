using Raven, Test

cd(@__DIR__)

function result(test)
  Raven.compile("raven/$test.rv", "raven/$test.wasm")
  String(read(`node runner.js raven/$test.wasm`))
end

passes(test) = parse(Bool, result(test))

for test in [:pow, :ptr]
  @test passes(test)
end
