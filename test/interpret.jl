using Vespa, Test
using Vespa: @vs_str, lowerfn, interpret

vs"""
  fn relu(x):
    if x > 0:
      x
    else:
      0
  """

@test interpret(Vespa.main[:relu], 5) == 5
@test interpret(Vespa.main[:relu], -5) == 0

vs"""
  fn relu2(x): relu(x)
  """

@test interpret(Vespa.main[:relu2], 5) == 5

vs"""
  fn pow(x, n):
    r := 1
    while n > 0:
      r := r * x
      n := n - 1
    return r
  """

@test interpret(Vespa.main[:pow], 2, 3) == 8
