using Vespa, Test
using Vespa: @vs_str, lowerfn, interpret

ir = lowerfn(vs"""
  fn relu(x):
    if x > 0:
      x
    else:
      0
  """)

@test interpret(ir, 5) == 5
@test interpret(ir, -5) == 0
