using Vespa, Test
using Vespa: Call, Block

const vespa = Vespa.parse

macro vs_str(x)
  vespa(x)
end

@test vs"foo(x, y)" == Call(:foo, [:x, :y])

@test vs"fn f(x): add(x, 1)" ==
  Block(:fn, [vs"f(x)"], [vs"add(x,1)"])

@test vs"""
  fn f(x):
    add(x, 1)
  """ == vs"fn f(x): add(x, 1)"

@test vs"""
  fn f(x):
    add(x, 1)
    add(x, 2)
  """ == Block(:fn, [vs"f(x)"], [vs"add(x, 1)", vs"add(x, 2)"])
