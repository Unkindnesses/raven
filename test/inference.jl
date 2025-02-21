using Raven, Test
using Raven: @tag_str, @src_str, RInt64, reload!, rlist

compiler = Raven.global_compiler()
inf = compiler.pipe.inferred;

reload!(compiler, src"""
  fn pow(x, n: Int64) {
    r = one(x)
    while n > 0 {
      n = n - one(n)
      r = r * x
    }
    return r
  }
  """)

ir, T = inf[(tag"pow", rlist(RInt64(2), RInt64(3)))]
@test T == rlist(RInt64(8))

reload!(compiler, src"""
  fn fib(n) {
    if widen(n <= 1) {
      return n
    } else {
      return fib(n-1) + fib(n-2)
    }
  }
  """)

ir, T = inf[(tag"fib", rlist(RInt64(20)))]
@test T == rlist(RInt64())

reload!(compiler, src"""
  fn fib(n) { fib(n-1) + fib(n-2) }
  fn fib(1) { 1 }
  fn fib(0) { 0 }

  fn fibSequence(n) {
    xs = []
    for i in range(1, n) {
      append(&xs, fib(i))
    }
    return xs
  }
  """)

ir, T = inf[(tag"fibSequence", rlist(RInt64(5)))]
@test T == rlist(rlist(RInt64.([1, 1, 2, 3, 5])...))
