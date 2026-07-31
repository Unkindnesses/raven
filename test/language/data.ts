import { test } from 'vitest'
import { test as rv } from '../../src/cli/test.js'

test('tag equality', async () => {
  await rv(`
    test tag"foo" == tag"foo"
    test tag"foo" != tag"bar"
  `)
})

test('dynamic tag equality', async () => {
  await rv(`
    x = if widen(true) { tag"foo" } else { tag"bar" }
    test x == tag"foo"
    test x != tag"bar"
    test tag"foo" == x
    test tag"bar" != x
  `)
})

test('sum sequence', async () => {
  await rv(`
    xs = seq(widen(1), widen(2), widen(3))
    test sum(xs) == 6
  `)
})

test('dynamic part', async () => {
  await rv(`
    xs = [4, widen(5)]
    test part(xs, widen(1)) + part(xs, widen(2)) == 4+5
  `)
})

test('invalid static index', async () => {
  await rv(`
    xs = [3, 5, 7]
    part(xs, 5)
  `, { error: true, output: 'Invalid index 5 for [3, 5, 7]' })
})

test('invalid dynamic index', async () => {
  await rv(`
    xs = [3, 5, 7]
    test part(xs, widen(5))
  `, { error: true, output: 'Invalid index for [3, 5, 7]' })
})

test('tag builtin', async () => {
  await rv(`
    test tag(widen(5)) == Int
    test tag("foo") == String
  `)
})

test('vpack allocation', async () => {
  await rv(`
    fn myrange(n) {
      xs = []
      for i = range(1, n) {
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
  `)
})

test('vpack print', async () => {
  await rv(`
    fn myrange(n) {
      xs = []
      for i = range(1, n) {
        append(&xs, i)
      }
      return xs
    }

    println(myrange(widen(10)))
  `, { output: '[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]' })
})

test('dynamic packcat', async () => {
  await rv(`
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
  `)
})

test('iterator protocol', async () => {
  await rv(`
    {
      xs = range(widen(5), widen(6))
      itr = iterate(xs)

      val = next(&itr)
      test !nil?(val)
      test part(val, 1) == 5

      val = next(&itr)
      test !nil?(val)
      test part(val, 1) == 6

      val = next(&itr)
      test nil?(val)
    }
  `)
})

test('record', async () => {
  await rv(`
    {
      d = record()
      d = notnil(merge(d, tag"a", 7))
      d = notnil(merge(d, tag"b", 5))
      setkey(&d, tag"b", "foo")

      test length(d) == 2

      test getkey(d, tag"a") == 7
      test getkey(d, tag"b") == "foo"
      test haskey(d, tag"b")
      test !haskey(d, tag"c")

      test !nil?(merge(d, tag"b", "foo"))
      test nil?(merge(d, tag"b", "bar"))
      test !nil?(merge(d, tag"c", 9))

      test !nil?(merge(d, tag"a", widen(7)))
      test nil?(merge(d, tag"a", widen(8)))
    }
  `)
})

test('record setkey fails for new key', async () => {
  await rv(`
    {
      d = record()
      setkey(&d, tag"a", 7)
    }
  `, { error: true, output: 'No such key: a' })
})

test('record upsertkey inserts new key', async () => {
  await rv(`
    {
      d = record()
      upsertkey(&d, tag"a", 7)
      test length(d) == 1
      test getkey(d, tag"a") == 7
    }
  `)
})

test('merge records', async () => {
  await rv(`
    {
      b1 = record(Pair(tag"a", 1), Pair(tag"b", 2))
      b2 = record(Pair(tag"b", widen(2)), Pair(tag"d", 3))
      test !nil?(merge(&b1, b2))

      b1 = record(Pair(tag"a", 1), Pair(tag"b", 2))
      b2 = record(Pair(tag"b", widen(3)), Pair(tag"d", 3))
      test nil?(merge(&b1, b2))
    }
  `)
})

test('recursive type', async () => {
  await rv(`
    fn foo(n) { prepend(foo(n-1), n) }
    fn foo(0) { seq() }

    {
      xs = foo(widen(0))
      test string(tag(xs)) == "common.sequence/Empty"
      test nparts(xs) == 0
      test empty?(xs)
      test length(xs) == 0
      test allocationCount() == 0

      xs = foo(widen(5))
      test string(tag(xs)) == "common.sequence/Prepend"
      test nparts(xs) == 2
      test part(xs, 2) == 5
      test part(part(xs, 1), 2) == 4
      test !empty?(xs)
      test length(xs) == 5
      test allocationCount() == 0
    }
  `)
})

test('seq helpers', async () => {
  await rv(`
    {
      xs = seqRange(widen(1), widen(10))
      test sum(xs) == 55
      xs = repeat(widen(3), widen(5))
      test sum(xs) == 15
    }
  `)
})

test('print recursion', async () => {
  await rv(`
    println(seqRange(widen(1), widen(3)))
  `, { output: 'seq(1, 2, 3)' })
})

test('collect range', async () => {
  await rv(`
    {
      xs = collect(range(1, 5))
      test rest(xs) == [2, 3, 4, 5]

      xs = collect(range(1, widen(5)))
      test rest(xs) == [2, 3, 4, 5]
    }
  `)
})

test('for expression collects values', async () => {
  await rv(`
    ys = (for x = range(1, 5) { x })
    test ys == [1, 2, 3, 4, 5]
  `)
})

test('swap method', async () => {
  await rv(`
    fn swap(&x, &y) {
      [x, y] = [y, x]
      return x+y
    }

    a = widen(3)
    b = widen(5)

    test swap(&a, &b) == 8
    test a == 5
    test b == 3
  `)
})

test('copy method', async () => {
  await rv(`
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
  `)
})

test('splat arguments', async () => {
  await rv(`
    fn add(a, b) {
      a + b
    }

    test add([widen(2), widen(3)]...) == 5
    test add([widen(2)]..., [widen(3)]...) == 5
  `)
})

test('variadic args', async () => {
  await rv(`
    fn add(args...) {
      part(args, 1) + part(args, 2)
    }

    test add(widen(5), widen(3)) == 8
  `)
})

test('array destructuring', async () => {
  await rv(`
    {
      xs = [widen(2), widen(3)]
      [a, b] = xs
      test a + b == 5
    }
  `)
})

test('array destructuring with match', async () => {
  await rv(`
    {
      xs = [widen(2), widen(2)]
      [a, a] = xs
      test a + a == 4
    }
  `)
})

test('array destructuring failure', async () => {
  await rv(`
    {
      xs = [widen(2), widen(3)]
      [a, a] = xs
    }
  `, { error: true, output: 'match failed' })
})

test('complex destructuring', async () => {
  await rv(`
    {
      xs = Complex(widen(2), widen(3))
      Complex(a, b) = xs
      test a + b == 5
    }
  `)
})

test('prime sieve', async () => {
  await rv(`
    fn isPrime(n, primes) {
      for prime = primes {
        if rem(n, prime) == 0 {
          return false
        }
      }
      return true
    }

    fn sieve(n) {
      primes = []
      for i = range(2, n) {
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
  `)
})

test('bundle eval', async () => {
  await rv(`
    bundle Expr { Add(left, right), Literal(value) }

    fn eval(Add(left, right)) { eval(left) + eval(right) }

    fn eval(Literal(value)) { value }

    test Literal(1) == Literal(1)
    test eval(Add(Literal(1), Literal(2))) == 3
  `)
})

test('bundle show', async () => {
  await rv(`
    bundle Expr { Add(left, right), Literal(value) }

    show Add(Literal(1), Literal(2))
  `, { output: 'Add(Literal(1), Literal(2))' })
})

test('array values', async () => {
  await rv(`
    fn group(n) {
      xs = []
      result = []
      for i = range(1, n) {
        append(&xs, i)
        append(&result, xs)
      }
      return result
    }

    test group(widen(3)) == [[1], [1, 2], [1, 2, 3]]
  `)
})

test('global list release', async () => {
  await rv(`
    xs = collect(range(1, widen(3)))
    xs = collect(range(1, widen(3)))
    n = length(xs) # no global release
    test n == 3
  `)
})

test('cell', async () => {
  await rv(`
    buf = Cell(Float64, 5)
    test length(buf) == 5
    buf[1] = widen(3.5)
    test buf[1] == 3.5
  `)
})

test('circular buffer', async () => {
  await rv(`
    buf = RingBuffer(Float64, 3)

    put!(buf, 10.0)
    put!(buf, 20.0)
    put!(buf, 30.0)

    test length(buf) == 3
    test buf[1] == 10.0
    test buf[2] == 20.0
    test buf[3] == 30.0

    test take!(buf) == 10.0
    put!(buf, 40.0)

    test buf[1] == 20.0
    test buf[2] == 30.0
    test buf[3] == 40.0

    buf[2] = 35.0
    test take!(buf) == 20.0
    test take!(buf) == 35.0
    test take!(buf) == 40.0
    test length(buf) == 0
  `)
})
