using Raven, Test
using Raven: @tag_str

function loadsrc(src)
  tmp = tempname() * ".rv"
  open(io -> print(io, src), tmp, "w")
  try
    mod = Raven.load(tmp)
    inf = Raven.infer(mod)
    inf[(Raven.startmethod(mod),)]
    return inf
  finally
    rm(tmp)
  end
end

let
  inf = loadsrc("""
    fn pow(x, n: Int64) {
      r = one(x)
      while n > 0 {
        n = n - one(n)
        r = r * x
      }
      return r
    }
    pow(2, 3)
    """)
  fs = filter(Dict(inf)) do (sig, fr)
    sig[1] isa Raven.RMethod && sig[1].name == tag"pow"
  end |> collect
  @test fs[1][2][2] == 8
end

let
  inf = loadsrc("""
    fn fib(n) {
      if widen(n <= 1) {
        return n
      } else {
        return fib(n-1) + fib(n-2)
      }
    }
    fn main() { fib(20) }
    main()
    """)
  fs = filter(Dict(inf)) do (sig, fr)
    sig[1] isa Raven.RMethod && sig[1].name == tag"main"
  end |> collect
  @test fs[1][2][2] == Int64
end
