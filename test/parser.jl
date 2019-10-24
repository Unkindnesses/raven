using Raven, Test
using Raven: Call, If, Block, Operator, @rvx_str

@test rvx"foo(x, y)" == Call(:foo, [:x, :y])

@test rvx"fn f(x): add(x, 1)" ==
  Block(:fn, [rvx"f(x)"], [rvx"add(x,1)"], true)

@test rvx"""
  fn f(x):
    add(x, 1)
  """ == Block(:fn, [rvx"f(x)"], [rvx"add(x,1)"])

@test rvx"""
  fn f(x): # comment
    add(x, 1)
  """ == Block(:fn, [rvx"f(x)"], [rvx"add(x,1)"])

@test rvx"""
  fn f(x):
    add(x, 1)
    add(x, 2)
  """ == Block(:fn, [rvx"f(x)"], [rvx"add(x, 1)", rvx"add(x, 2)"])

@test rvx"2 + 3" == Operator(:+, [2, 3])

@test rvx"x = 2+3" == Operator(:(=), [:x, rvx"2+3"])

@test rvx"""
  if x > 0:
    1
  elseif x < 0:
    2
  else:
    3
  """ == If([rvx"x>0", rvx"x<0",true],[[rvx"1"],[rvx"2"],[rvx"3"]])
