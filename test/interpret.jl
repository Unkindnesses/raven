using Raven, Test
using Raven: interpret

cd(@__DIR__)

function stdout_string(f)
  _stdout = stdout
  try
    rd, = redirect_stdout()
    f()
    return String(readavailable(rd))
  finally
    redirect_stdout(_stdout)
  end
end

function result(test)
  stdout_string(() -> interpret("raven/$test.rv"))
end

passes(test) = parse(Bool, result(test))

@testset for test in [:pow, :ptr, :relu, :complex]
  @test passes(test)
end

@test_broken passes(:structures)
