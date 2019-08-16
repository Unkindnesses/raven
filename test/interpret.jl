using Vespa, Test
using Vespa: @vsx_str, lowerfn, interpret

ir = lowerfn(vsx"""
  fn relu(x):
    if x > 0:
      x
    else:
      0
  """)

@test interpret(ir, 5) == 5
@test interpret(ir, -5) == 0
