using Vespa, Test
using Vespa: Call, If, Block, Operator, @vsx_str

@test vsx"foo(x, y)" == Call(:foo, [:x, :y])

@test vsx"fn f(x): add(x, 1)" ==
  Block(:fn, [vsx"f(x)"], [vsx"add(x,1)"], true)

@test vsx"""
  fn f(x):
    add(x, 1)
  """ == Block(:fn, [vsx"f(x)"], [vsx"add(x,1)"])

@test vsx"""
  fn f(x): # comment
    add(x, 1)
  """ == Block(:fn, [vsx"f(x)"], [vsx"add(x,1)"])

@test vsx"""
  fn f(x):
    add(x, 1)
    add(x, 2)
  """ == Block(:fn, [vsx"f(x)"], [vsx"add(x, 1)", vsx"add(x, 2)"])

@test vsx"2 + 3" == Operator(:+, [2, 3])

@test vsx"x := 2+3" == Operator(:(:=), [:x, vsx"2+3"])

@test vsx"""
  if x > 0:
    1
  elseif x < 0:
    2
  else:
    3
  """ == If([vsx"x>0", vsx"x<0",true],[[vsx"1"],[vsx"2"],[vsx"3"]])
