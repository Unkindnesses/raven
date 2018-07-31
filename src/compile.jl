baremodule __main__
import Base: =>
end

vseval(ex) = eval(__main__, ex)

macro vs_str(s)
  vseval(parse(s))
end

parse("""
  def foo(a, b):
    {a: a, b: b}
  """)

vs"""
  def foo(a, b):
    {a: a, b: b}
  """

__main__.foo(1, 2)
