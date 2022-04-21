using Raven, Test

cd(@__DIR__)

function result(test)
  Raven.compile("raven/$test.rv", "compiled")
  String(read(`node compiled/$test.js`))
end

passes(test) = parse(Bool, result(test))

function result_code(test)
  Raven.compile("raven/$test.rv", "compiled")
  p = run(`node compiled/$test.js`, wait = false)
  wait(p); p.exitcode
end

fails(test) = result_code(test) == 1

@testset for test in [:pow, :ptr, :relu, :complex, :structures,
                      :splat, :scope, :malloc0, :malloc1, :malloc2]
  @test passes(test)
end

# Test that the code compiles successfully, failing at runtime
@testset for test in [:global, :method, :condition]
  @test fails("error-$test")
end
