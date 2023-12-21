using Raven, Test

let
  out = Pipe()
  repl = Raven.REPL(stdout = out)
  em = Raven.reload!(repl.compiler, src"println(2+2)")
  for mod in em.emitter.queue
    Raven.runWasm(repl.conn, mod)
  end
  @test readline(out) == "4"
  close(repl)
end
