using Raven, Test

cd(@__DIR__)

function output(test)
  Raven.compile("raven/$test.rv", dir = "compiled")
  String(read(`node compiled/$test.js`))
end

result(test) = parse.(Bool, split(output(test)))

passes(test) = parse(Bool, result(test))

function result_code(test)
  Raven.compile("raven/$test.rv", dir = "compiled")
  p = run(`node compiled/$test.js`, wait = false)
  wait(p); p.exitcode
end

fails(test) = result_code(test) == 1

@testset for test in [:pow, :ptr, :relu, :complex, :structures, :swap,
                      :splat, :scope, :malloc0, :malloc1, :malloc2,
                      :patterns, :string]
  for r in result(test)
    @test r
  end
end

# Test that the code compiles successfully, failing at runtime
@testset for test in [:global, :function, :method, :condition, :index, :match]
  @test fails("error-$test")
end

# Test `println` output
@test output("print-list") == string([1:10...])
@test output("print-seq") == "seq(1, 2, 3)"
