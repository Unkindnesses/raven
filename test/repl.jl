using Raven, Test

let
  out = Pipe()
  repl = Raven.REPL(stdout = out)
  em = Raven.eval!(repl, "2+2")
  @test readline(out) == "4"
  close(repl)
end
