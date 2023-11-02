using Raven, Test
using Raven: @src_str, rlist

let
  inf = Raven.code_typed(src"""
    fn pow(x, n: Int64) {
      r = one(x)
      while n > 0 {
        n = n - one(n)
        r = r * x
      }
      return r
    }
    pow(2, 3)
    """, tag"pow")
  fs = filter(inf) do (sig, fr)
    sig[1] isa Raven.RMethod
  end |> collect
  @test only(fs)[2][2] == 8
end

let
  inf = Raven.code_typed(src"""
    fn fib(n) {
      if widen(n <= 1) {
        return n
      } else {
        return fib(n-1) + fib(n-2)
      }
    }
    fn main() { fib(20) }
    main()
    """, tag"main")
  fs = filter(inf) do (sig, fr)
    sig[1] isa Raven.RMethod
  end |> collect
  @test fs[1][2][2] == Int64
end
