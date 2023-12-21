using Raven, Test

@testset "Basic eval" begin
  out = Pipe()
  repl = Raven.REPL(stdout = out)

  Raven.eval!(repl, "2+2")
  @test readline(out) == "4"

  Raven.eval!(repl, "xs = [1, 2, 3]")
  @test readline(out) == "[1, 2, 3]"

  Raven.eval!(repl, "append(&xs, 4)")
  @test readline(out) == "[1, 2, 3, 4]"

  Raven.eval!(repl, "xs")
  @test readline(out) == "[1, 2, 3, 4]"

  Raven.eval!(repl, "x = widen(5)")
  @test readline(out) == "5"

  Raven.eval!(repl, "x + 2")
  @test readline(out) == "7"

  close(repl)
end
