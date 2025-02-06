using Raven, Test
using Raven: @test_rv, @test_rv_str

@test_rv("show 2+2", output = "(2 + 2) = 4")

@test_rv("println(js(1))", output = "1")

@test_rv("println(2.0)", output = "2")

@test_rv("println(x)", error = true, output = "x is not defined")

@test_rv("foo()", error = true, output = "foo is not defined")

@test_rv("""
  if "true" { println("hi") }
  """,
  error = true,
  output = "Non-boolean used as a condition")

@test_rv("""
  fn foo(a, b) {
    return a+b
  }

  foo(1, 2, 3)
  """,
  error = true,
  output = "No matching method")

test_rv"""
  fn relu(x) {
    if x > 0 {
      x
    } else {
      0
    }
  }

  test relu(widen(5)) == 5
  test relu(widen(-5)) == 0
  """

test_rv"""
  fn foo(x) { x + 1 }

  test foo(widen(5)) == 6

  fn foo(x) { x + 2 }

  test foo(widen(5)) == 7
  """

test_rv"""
  x = widen(2)
  n = widen(3)
  r = x^n

  test r == 8

  fn pow(x, n: Int64) {
    r = one(x)
    while true {
      if n == 0 {
        return r
      }
      n = n - one(n)
      r = r * x
    }
  }

  test pow(2, 3) == 8
  """

test_rv"""
  z = Complex(widen(5), widen(6))

  test abs2(z) == 61
  test (z*z) == Complex(widen(-11), widen(60))
  test z == z
  test z != (z*z)
  """

test_rv"""
  fn fib(n) {
    if n <= 1 {
      return n
    } else {
      return fib(n-1) + fib(n-2)
    }
  }

  test fib(20) == 6765
  """

test_rv"""
  fn fib(n) { fib(n-1) + fib(n-2) }
  fn fib(0) { 0 }
  fn fib(1) { 1 }

  test fib(widen(20)) == 6765
  """

@testset "Memory" begin
  include("memory.jl")
end

# Return-value casting
test_rv"""
  fn foo(c) {
    if c {
      return "true"
    } else {
      return "false"
    }
  }

  test foo(widen(true)) == "true"
  test foo(widen(true)) != "false"
  test foo(widen(false)) == "false"
  """

test_rv"""
  test tag"foo" == tag"foo"
  test tag"foo" != tag"bar"
  """

test_rv"""
  x = if widen(true) { tag"foo" } else { tag"bar" }
  test x == tag"foo"
  test x != tag"bar"
  test tag"foo" == x
  test tag"bar" != x
  """

test_rv"""
  xs = seq(widen(1), widen(2), widen(3))
  test sum(xs) == 6
  """

test_rv"""
  xs = [4, widen(5)]
  test (part(xs, widen(1)) + part(xs, widen(2))) == (4+5)
  """

@test_rv("""
  xs = [3, 5, 7]
  test part(xs, 5)
  """,
  error = true,
  output = "Invalid index 5 for [3, 5, 7]")

@test_rv("""
  xs = [3, 5, 7]
  test part(xs, widen(5))
  """,
  error = true,
  output = "Invalid index for [3, 5, 7]")

test_rv"""
  test tag(widen(5)) == Int64
  test part(widen(5), 1) == 5
  test tag("foo") == String
  """

test_rv"""
  fn myrange(n) {
    xs = []
    for i in range(1, n) {
      append(&xs, i)
    }
    return xs
  }

  {
    xs = myrange(widen(10))
    test nparts(xs) == 10
    test part(xs, 1) == 1
    test part(xs, 10) == 10
    test part(xs, widen(5)) == 5
  }

  test allocationCount() == 0
  """

@test_rv("""
  fn myrange(n) {
    xs = []
    for i in range(1, n) {
      append(&xs, i)
    }
    return xs
  }

  println(myrange(widen(10)))
  """, output = string([1:10...]))

test_rv"""
  fn rangeReverse(n) {
    xs = []
    i = 1
    while (i <= n) {
      xs = [i, xs...]
      i = i + 1
    }
    return xs
  }

  {
    xs = rangeReverse(10)
    test nparts(xs) == 10
    test part(xs, 1) == 10
    test part(xs, 10) == 1
    test part(xs, widen(5)) == 6
  }

  test allocationCount() == 0
  """

test_rv"""
  {
    xs = range(widen(5), widen(6))
    itr = iterator(xs)

    val = iterate(&itr)
    test not(isnil(val))
    test part(val, 1) == 5

    val = iterate(&itr)
    test not(isnil(val))
    test part(val, 1) == 6

    val = iterate(&itr)
    test isnil(val)
  }
  """

test_rv"""
  {
    d = simpledict()
    setkey(&d, tag"a", 7)
    setkey(&d, tag"b", 5)
    setkey(&d, tag"b", "foo")

    test length(d) == 2

    test getkey(d, tag"a") == 7
    test getkey(d, tag"b") == "foo"
    test haskey(d, tag"b")
    test not(haskey(d, tag"c"))

    test not(isnil(merge(d, tag"b", "foo")))
    test isnil(merge(d, tag"b", "bar"))
    test not(isnil(merge(d, tag"c", 9)))

    test not(isnil(merge(d, tag"a", widen(7))))
    test isnil(merge(d, tag"a", widen(8)))
  }
  """

test_rv"""
  {
    b1 = seq(Pair(tag"a", 1), Pair(tag"b", 2))
    b2 = seq(Pair(tag"b", widen(2)), Pair(tag"d", 3))
    test not(isnil(merge(&b1, b2)))

    b1 = seq(Pair(tag"a", 1), Pair(tag"b", 2))
    b2 = seq(Pair(tag"b", widen(3)), Pair(tag"d", 3))
    test isnil(merge(&b1, b2))
  }
  """

test_rv"""
  fn foo(n) { prepend(foo(n-1), n) }
  fn foo(0) { seq() }

  {
    xs = foo(widen(0))
    test string(tag(xs)) == "common.Empty"
    test nparts(xs) == 0
    test isempty(xs)
    test length(xs) == 0
    test allocationCount() == 0

    xs = foo(widen(5))
    test string(tag(xs)) == "common.Prepend"
    test nparts(xs) == 2
    test part(xs, 2) == 5
    test part(part(xs, 1), 2) == 4
    test not(isempty(xs))
    test length(xs) == 5
    test allocationCount() == 0
  }
  """

test_rv"""
  {
    xs = seqRange(widen(1), widen(10))
    test sum(xs) == 55
    xs = repeat(widen(3), widen(5))
    test sum(xs) == 15
  }
  """

@test_rv("""
  println(seqRange(widen(1), widen(3)))
  """, output = "seq(1, 2, 3)")

test_rv"""
  {
    xs = collect(range(1, 5))
    test length(rest(xs)) == 4

    xs = collect(range(1, widen(5)))
    test length(rest(xs)) == 4
  }
  """

test_rv"""
  fn swap(&x, &y) {
    [x, y] = [y, x]
    return x+y
  }

  a = widen(3)
  b = widen(5)

  test swap(&a, &b) == 8
  test a == 5
  test b == 3
  """

test_rv"""
  fn copy(x: Int64, &y: Int64) {
    y = x
    return
  }

  {
    c = 3
    d = 5
    copy(&c, &d) # TODO disallow this
    test c == nil
    test d == 3
  }
  """

test_rv"""
  fn add(a, b) {
    a + b
  }

  test add([widen(2), widen(3)]...) == 5
  test add([widen(2)]..., [widen(3)]...) == 5
  """

test_rv"""
  fn add(args...) {
    part(args, 1) + part(args, 2)
  }

  test add(widen(5), widen(3)) == 8
  """

test_rv"""
  n = widen(5)

  fn foo(m) { return n + m }

  test foo(3) == 8
  """

test_rv"""
  x = 1, { x = 2 }
  test x == 2
  """

test_rv"""
  x = widen(1), { x = 2 }
  test x == 2
  """

test_rv"""
  x = 1
  y = widen(1)
  let x = x {
    x = x + 1
    y = y + 1
    test x == 2
    test y == 2
  }

  test x == 1
  test y == 2
  """

test_rv"""
  test not(isnil(match(widen(1), Literal(1))))
  test isnil(match(widen(2), Literal(1)))
  """

test_rv"""
  fn test(1, x) { x }
  fn test(2, x) { x + 1 }

  test test(widen(1), widen(3)) == 3
  test test(widen(2), widen(3)) == 4

  fn test(Complex(a, b)) { a + b }
  fn test(Complex(a, a)) { a }

  test test(Complex(1, widen(2))) == 3
  test test(Complex(2, widen(2))) == 2
  """

test_rv"""
  {
    xs = [widen(2), widen(3)]
    [a, b] = xs
    test (a + b) == 5
  }
  """

test_rv"""
  {
    xs = [widen(2), widen(2)]
    [a, a] = xs
    test (a + a) == 4
  }
  """

@test_rv("""
  {
    xs = [widen(2), widen(3)]
    [a, a] = xs
  }
  """,
  error = true,
  output = "match failed")

test_rv"""
  {
    xs = Complex(widen(2), widen(3))
    Complex(a, b) = xs
    test (a + b) == 5
  }
  """

test_rv"""
  test concat("a", "b") == "ab"
  """

test_rv"""
  fn eitherSym(x) {
    if x {
      return tag"foo"
    } else {
      return tag"bar"
    }
  }

  test string(eitherSym(widen(true))) == "foo"
  test string(eitherSym(widen(false))) == "bar"
  """

test_rv"""
  fn isPrime(n, primes) {
    for prime in primes {
      if rem(n, prime) == 0 {
        return false
      }
    }
    return true
  }

  fn sieve(n) {
    primes = []
    for i in range(2, n) {
      if isPrime(i, primes) {
        append(&primes, i)
      }
    }
    return primes
  }

  {
    primes = sieve(widen(100))
    test part(primes, 1) == 2
    test part(primes, length(primes)) == 97
  }
  """

test_rv"""
  {
    obj = await(call(js(), "dummyPromise", 5))
    test Float64(obj) == Float64(5)
  }
  """

@test_rv("""
  x = 1
  clear x
  println(x)
  """, error = true, output = "x is not defined")

@test_rv("""
  fn square(x) { x * x }
  clear square
  tag"square"(5)
  """, error = true, output = "No matching method")

# Test compiling without JS refcounting
@test_rv("""
  fn pow(x, n: Int64) {
    r = one(x)
    while true {
      if n == 0 {
        return r
      }
      n = n - one(n)
      r = r * x
    }
  }

  test pow(2, 3) == 8
  """,
  options = Raven.Options(jsalloc=false))

@test_rv("""
  fn either(x) {
    if x {
      return widen(5)
    } else {
      return widen("foo")
    }
  }

  # TODO can't yet compile dynamic `show` fallback
  fn prn(x: Int64) { println(x) }
  fn prn(x: String) { println(x) }

  prn(either(widen(true)))
  prn(either(widen(false)))
  """, output = "5\nfoo")

@testset "WASI" begin
  Raven.compile(joinpath(@__DIR__, "wasi.rv"), Raven.Options(memcheck=false))
  run(`wasm-tools component embed $(@__DIR__)/../../wasi-cli/wit --world command $(@__DIR__)/wasi.wasm -o $(@__DIR__)/wasi.cli.wasm`)
  run(`wasm-tools component new $(@__DIR__)/wasi.cli.wasm -o $(@__DIR__)/wasi.cli.wasm`)
  @test String(read(`wasmtime $(@__DIR__)/wasi.cli.wasm`)) == "hello!\n"
end

test_rv"""
  {
    Ok(x) = errcall(js(), "dummyPromise", 5)
    test Float64(x) == 5.0
  }
  """

# Currently only works because of dispatcher trimming; we can't compile the
# generic fallback `show` method.
@test_rv("""
  {
    x = errcall(js(), "dummyPromise", 7)
    show x
  }
  """, output = "x = Ok(7)")

test_rv"""
  {
    x = unwrap(errcall(js(), "dummyPromise", 5))
    test Float64(x) == 5.0
  }
  """

@test_rv("""
  unwrap(errcall(js(), "dummyErr"))
  """,
  error = true,
  output = ["unwrap Err", "dummy error"])

@test_rv("""
  for arg in args() {
    println(arg)
  }
  """, output = "node")

test_rv"""
  {
    xs = []
    if widen(true) {
      append(&xs, "foo")
      append(&xs, "bar")
    }
    test part(xs, 2) == "bar"
  }
  """

test_rv"""
  bundle Expr { Add(left, right), Lit(value) }

  fn eval(Add(left, right)) { eval(left) + eval(right) }

  fn eval(Lit(value)) { value }

  test Lit(1) == Lit(1)
  test eval(Add(Lit(1), Lit(2))) == 3
  """

@test_rv("""
  bundle Expr { Add(left, right), Lit(value) }

  show Add(Lit(1), Lit(2))
  """,
  output = "Add(Lit(1), Lit(2))")

test_rv"""
  test round(1.3) == 1
  test round(1.7) == 2
  test round(-1.3) == -1
  test round(-1.7) == -2
  """
