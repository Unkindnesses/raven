using Raven, Test
using Raven: @test_rv, @test_rv_str

@test_rv("println(x)", error = true, output = "x is not defined")

@test_rv("foo()", error = true, output = "foo is not defined")

@test_rv("show 2+2", output = "(2 + 2) = 4")

@test_rv("""
  if "true" { println("hi") }
  """,
  error = true,
  output = "Non-boolean used as a condition")

@test_rv("""
  fn foo(a, b) {
    return a+b
  }

  println(foo(1, 2, 3))
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

  println(relu(widen(5)) == 5)
  println(relu(widen(-5)) == 0)
  """

test_rv"""
  x = widen(2)
  n = widen(3)
  r = x^n

  println(r == 8)

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

  println(pow(2, 3) == 8)
  """

test_rv"""
  z = Complex(widen(5), widen(6))

  println(abs2(z) == 61)
  println((z*z) == Complex(widen(-11), widen(60)))
  println(z == z)
  println(z != (z*z))
  """

test_rv"""
  fn fib(n) {
    if n <= 1 {
      return n
    } else {
      return fib(n-1) + fib(n-2)
    }
  }

  println(fib(20) == 6765)
  """

test_rv"""
  fn fib(n) { fib(n-1) + fib(n-2) }
  fn fib(0) { 0 }
  fn fib(1) { 1 }

  println(fib(widen(20)) == 6765)
  """

@testset "Memory" begin
  include("memory.jl")
end

# Return-value casting
test_rv"""
  fn test(c) {
    if c {
      return "true"
    } else {
      return "false"
    }
  }

  println(test(widen(true)) == "true")
  println(test(widen(true)) != "false")
  println(test(widen(false)) == "false")
  """

test_rv"""
  println(tag"foo" == tag"foo")
  println(tag"foo" != tag"bar")
  """

test_rv"""
  x = if widen(true) { tag"foo" } else { tag"bar" }
  println(x == tag"foo")
  println(x != tag"bar")
  println(tag"foo" == x)
  println(tag"bar" != x)
  """

test_rv"""
  xs = seq(widen(1), widen(2), widen(3))
  println(sum(xs) == 6)
  """

test_rv"""
  xs = [4, widen(5)]
  println((part(xs, widen(1)) + part(xs, widen(2))) == (4+5))
  """

@test_rv("""
  xs = [3, 5, 7]
  println(part(xs, 5))
  """,
  error = true,
  output = "Invalid index 5 for pack")

@test_rv("""
  xs = [3, 5, 7]
  println(part(xs, widen(5)))
  """,
  error = true,
  output = "Invalid index for pack")

test_rv"""
  println(tag(widen(5)) == Int64)
  println(part(widen(5), 1) == 5)
  println(tag("foo") == String)
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
    println(nparts(xs) == 10)
    println(part(xs, 1) == 1)
    println(part(xs, 10) == 10)
    println(part(xs, widen(5)) == 5)
  }

  println(allocationCount() == 0)
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
    println(nparts(xs) == 10)
    println(part(xs, 1) == 10)
    println(part(xs, 10) == 1)
    println(part(xs, widen(5)) == 6)
  }

  println(allocationCount() == 0)
  """

test_rv"""
  {
    xs = range(widen(5), widen(6))
    itr = iterator(xs)

    val = iterate(&itr)
    println(not(isnil(val)))
    println(part(val, 1) == 5)

    val = iterate(&itr)
    println(not(isnil(val)))
    println(part(val, 1) == 6)

    val = iterate(&itr)
    println(isnil(val))
  }
  """

test_rv"""
  {
    d = simpledict()
    setkey(&d, tag"a", 7)
    setkey(&d, tag"b", 5)
    setkey(&d, tag"b", "foo")

    println(length(d) == 2)

    println(getkey(d, tag"a") == 7)
    println(getkey(d, tag"b") == "foo")
    println(haskey(d, tag"b"))
    println(not(haskey(d, tag"c")))

    println(not(isnil(merge(d, tag"b", "foo"))))
    println(isnil(merge(d, tag"b", "bar")))
    println(not(isnil(merge(d, tag"c", 9))))

    println(not(isnil(merge(d, tag"a", widen(7)))))
    println(isnil(merge(d, tag"a", widen(8))))
  }
  """

test_rv"""
  {
    b1 = seq(Pair(tag"a", 1), Pair(tag"b", 2))
    b2 = seq(Pair(tag"b", widen(2)), Pair(tag"d", 3))
    println(not(isnil(merge(&b1, b2))))

    b1 = seq(Pair(tag"a", 1), Pair(tag"b", 2))
    b2 = seq(Pair(tag"b", widen(3)), Pair(tag"d", 3))
    println(isnil(merge(&b1, b2)))
  }
  """

test_rv"""
  fn foo(n) { prepend(foo(n-1), n) }
  fn foo(0) { seq() }

  {
    xs = foo(widen(0))
    println(string(tag(xs)) == "common.Empty")
    println(nparts(xs) == 0)
    println(isempty(xs))
    println(length(xs) == 0)
    println(allocationCount() == 0)

    xs = foo(widen(5))
    println(string(tag(xs)) == "common.Prepend")
    println(nparts(xs) == 2)
    println(part(xs, 2) == 5)
    println(part(part(xs, 1), 2) == 4)
    println(not(isempty(xs)))
    println(length(xs) == 5)
    println(allocationCount() == 0)
  }
  """

test_rv"""
  {
    xs = seqRange(widen(1), widen(10))
    println(sum(xs) == 55)
    xs = repeat(widen(3), widen(5))
    println(sum(xs) == 15)
  }
  """

@test_rv("""
  print(seqRange(widen(1), widen(3)))
  """, output = "seq(1, 2, 3)")

test_rv"""
  {
    xs = collect(range(1, 5))
    println(length(rest(xs)) == 4)

    xs = collect(range(1, widen(5)))
    println(length(rest(xs)) == 4)
  }
  """

test_rv"""
  fn swap(&x, &y) {
    tmp = x
    x = y
    y = tmp
    x+y
  }

  a = widen(3)
  b = widen(5)

  println(swap(&a, &b) == 8)
  println(a == 5)
  println(b == 3)
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
    println(c == nil)
    println(d == 3)
  }
  """

test_rv"""
  fn add(a, b) {
    a + b
  }

  println(add([widen(2), widen(3)]...) == 5)
  println(add([widen(2)]..., [widen(3)]...) == 5)
  """

test_rv"""
  fn add(args...) {
    part(args, 1) + part(args, 2)
  }

  println(add(widen(5), widen(3)) == 8)
  """

test_rv"""
  n = widen(5)

  fn foo(m) { return n + m }

  println(foo(3) == 8)
  """

test_rv"""
  x = 1, { x = 2 }
  println(x == 2)
  """

test_rv"""
  x = widen(1), { x = 2 }
  println(x == 2)
  """

test_rv"""
  x = 1
  y = widen(1)
  let x = x {
    x = x + 1
    y = y + 1
    println(x == 2)
    println(y == 2)
  }

  println(x == 1)
  println(y == 2)
  """

test_rv"""
  println(not(isnil(match(widen(1), Literal(1)))))
  println(isnil(match(widen(2), Literal(1))))
  """

test_rv"""
  fn test(1, x) { x }
  fn test(2, x) { x + 1 }

  println(test(widen(1), widen(3)) == 3)
  println(test(widen(2), widen(3)) == 4)

  fn test(Complex(a, b)) { a + b }
  fn test(Complex(a, a)) { a }

  println(test(Complex(1, widen(2))) == 3)
  println(test(Complex(2, widen(2))) == 2)
  """

test_rv"""
  {
    xs = [widen(2), widen(3)]
    [a, b] = xs
    println((a + b) == 5)
  }
  """

test_rv"""
  {
    xs = [widen(2), widen(2)]
    [a, a] = xs
    println((a + a) == 4)
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
    println((a + b) == 5)
  }
  """

test_rv"""
  println(concat("a", "b") == "ab")
  """

test_rv"""
  fn eitherSym(x) {
    if x {
      return tag"foo"
    } else {
      return tag"bar"
    }
  }

  println(string(eitherSym(widen(true))) == "foo")
  println(string(eitherSym(widen(false))) == "bar")
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
    println(part(primes, 1) == 2)
    println(part(primes, length(primes)) == 97)
  }
  """

test_rv"""
  {
    obj = await(call(global(), "dummyPromise", 5))
    println(Float64(obj) == Float64(5))
  }
  """

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

  println(pow(2, 3) == 8)
  """,
  options = Raven.Options(jsalloc=false))
