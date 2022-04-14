using Raven, Test

cd(@__DIR__)

function result(test)
  Raven.compile("raven/$test.rv", "compiled")
  String(read(`node compiled/$test.js`))
end

passes(test) = parse(Bool, result(test))

@testset for test in [:pow, :ptr, :relu, :complex, :memory, :structures, :splat, :scope]
  @test passes(test)
end

for f in ["error-global", "methoderror"]
  let
    Raven.compile("raven/$f.rv", "compiled")
    p = run(`node compiled/$f.js`, wait = false)
    wait(p)
    @test p.exitcode == 1
  end
end
