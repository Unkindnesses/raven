using Raven, Test
using Raven: @rv_str, lowerfn, interpret

rv"""
  fn relu(x) {
    if x > 0 {
      x
    } else {
      0
    }
  }
  """

@test rv"relu(5)" == 5
@test rv"relu(0-5)" == 0

rv"""
  fn relu2(x) { relu(x) }
  """

@test rv"relu2(5)" == 5

rv"""
  fn pow(x, n) {
    r = 1
    while n > 0 {
      r = r * x
      n = n - 1
    }
    return r
  }
  """

@test rv"pow(2, 3)" == 8

@test rv"data(1, 2)" isa Raven.Data
