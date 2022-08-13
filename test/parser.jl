using Raven, Test
using Raven: @rvx_str
using Raven.AST: Call, Syntax, Block, Operator, Splat

@test rvx"foo(x, y)" == Call(:foo, [:x, :y])

@test rvx"fn f(x) { add(x, 1) }" ==
  Syntax(:fn, [rvx"f(x)", Block([rvx"add(x,1)"])])

@test rvx"""
  fn f(x) { # comment
    add(x, 1)
  }
  """ == Syntax(:fn, [rvx"f(x)", Block([rvx"add(x,1)"])])

@test rvx"""
  fn f(x) {
    add(x, 1)
    add(x, 2)
  }
  """ == Syntax(:fn, [rvx"f(x)", Block([rvx"add(x, 1)", rvx"add(x, 2)"])])

@test rvx"2 + 3" == Operator(:+, [2, 3])

@test rvx"x = 2+3" == Operator(:(=), [:x, rvx"2+3"])

@test rvx"""
  if x > 0 {
    1
  } else if x < 0 {
    2
  } else {
    3
  }
  """ == Syntax(:if, [rvx"x>0", Block([1]),
                      :else, :if, rvx"x<0", Block([2]),
                      :else, Block([3])])

@test rvx"foo(x, y...)" == Call(:foo, [:x, Splat(:y)])

let
  ex = rvx"fn add(a, b) { a+b }"
  @test Raven.fnsig(ex) isa Raven.Signature
end
