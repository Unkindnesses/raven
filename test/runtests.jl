using Vespa, Test

@testset "Vespa" begin

vs"""
  def foo(a):
    add(a, 1)
  """

@test vs"foo(2)" == 3

vs"""
  def foo(a, b):
    {a: a, b: b}
  """

@test vs"foo(1, 2)" == Dict("a"=>1,"b"=>2)

end
