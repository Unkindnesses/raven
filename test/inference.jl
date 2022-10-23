using Raven, Test

function loadsrc(src)
  tmp = tempname() * ".rv"
  open(io -> print(io, src), tmp, "w")
  try
    Raven.loadfile(tmp)
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
  fs = filter(inf.frames) do (sig, fr)
    sig[1] isa Raven.RMethod && sig[1].name == :pow
  end |> collect
  @test fs[1][2].rettype == 8
end
