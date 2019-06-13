using Vespa, Test
using Vespa: Call, If, Block, Operator, @vs_str

@test vs"foo(x, y)" == Call(:foo, [:x, :y])

@test vs"fn f(x): add(x, 1)" ==
  Block(:fn, [vs"f(x)"], [vs"add(x,1)"], true)

@test vs"""
  fn f(x):
    add(x, 1)
  """ == Block(:fn, [vs"f(x)"], [vs"add(x,1)"])

@test vs"""
  fn f(x):
    add(x, 1)
    add(x, 2)
  """ == Block(:fn, [vs"f(x)"], [vs"add(x, 1)", vs"add(x, 2)"])

@test vs"2 + 3" == Operator(:+, [2, 3])

@test vs"x := 2+3" == Operator(:(:=), [:x, vs"2+3"])

@test vs"""
  if x > 0:
    1
  elseif x < 0:
    2
  else:
    3
  """ == If([vs"x>0", vs"x<0",true],[[vs"1"],[vs"2"],[vs"3"]])
