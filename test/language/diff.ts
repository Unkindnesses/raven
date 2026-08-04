import { test } from 'vitest'
import { test as rv } from '../../src/cli/test.js'

const close = `fn close?(x, y) { d = x - y, -0.000000001 < d && d < 0.000000001 }`

test('derivative rules', async () => {
  await rv(`
    ${close}

    test diff(sin, 0.0) == 1.0
    test diff(exp, 0.0) == 1.0
    test diff(sqrt, 4.0) == 0.25
    test close?(diff(log, 2.0), 0.5)

    test grad(*, 2.0, 3.0) == [3.0, 2.0]
    test grad(sin, 0.0) == [1.0]
    test grad(exp, 0.0) == [1.0]
    test grad(sqrt, 4.0) == [0.25]
    test close?(grad(log, 2.0)[1], 0.5)
  `)
})

test('source transform', async () => {
  await rv(`
    ${close}

    fn f(x) { sin(sin(x)) }
    fn square(x) { x * x }
    fn translate(x) { x + 2.0 }
    fn quotient(x) { (x - 1.0) / (x + 1.0) }

    test diff(f, 0.0) == 1.0
    test diff(square, 3.0) == 6.0
    test diff(translate, 3.0) == 1.0

    test grad(f, 0.0) == [1.0]
    test grad(square, 3.0) == [6.0]
    test grad(translate, 3.0) == [1.0]

    # dy = 2/(x+1)^2
    test close?(diff(quotient, 1.0), 0.5)
    test close?(grad(quotient, 1.0)[1], 0.5)
  `)
})

test('control flow', async () => {
  await rv(`
    fn signed(x) { if x > 0.0 { x * x } else { -(x * x) } }

    test diff(signed, 3.0) == 6.0
    test diff(signed, -3.0) == 6.0

    test grad(signed, 3.0) == [6.0]
    test grad(signed, -3.0) == [6.0]

    # only one branch uses each argument, so the other gets a zero
    fn pick(x, y) { if x > 0.0 { x * x } else { y * y } }

    test grad(pick, 2.0, 3.0) == [4.0, 0.0]
    test grad(pick, -2.0, 3.0) == [0.0, 6.0]
  `)
})

test('loops', async () => {
  await rv(`
    fn power(x, n: Int) {
      r = one(x)
      while n > 0 {
        n = n - one(n)
        r = r * x
      }
      return r
    }
    fn cube(x) { power(x, 3) }

    test diff(cube, 2.0) == 12.0
    test grad(cube, 2.0) == [12.0]
  `)
})

test('recursion', async () => {
  await rv(`
    # Halving until it stops, so shrink(x) is 10x/8 over (8, 16].
    fn shrink(x) { if x > 1.0 { shrink(x * 0.5) } else { x * 10.0 } }

    test diff(shrink, 8.0) == 1.25

    fn power(x, n: Int) { if n == 0 { one(x) } else { x * power(x, n - 1) } }
    fn cube(x) { power(x, 3) }

    test diff(cube, 2.0) == 12.0

    # ping(x) counts x down to zero, tripling and then doubling five times.
    fn ping(x) { if x < 1.0 { x } else { pong(x - 1.0) * 2.0 } }
    fn pong(x) { if x < 1.0 { x * 3.0 } else { ping(x - 1.0) } }

    test diff(ping, 5.0) == 24.0
  `)
})

test('several arguments', async () => {
  await rv(`
    fn poly(x, y) { x * x * y + y }

    # 15, then the partials 2xy and x^2 + 1
    [z, dz] = forward(poly, [2.0, 3.0], [1.0, 0.0])
    test z == 15.0
    test dz == 12.0

    [z, dz] = forward(poly, [2.0, 3.0], [0.0, 1.0])
    test z == 15.0
    test dz == 5.0

    # x is used twice, so its cotangents accumulate
    test grad(poly, 2.0, 3.0) == [12.0, 5.0]
  `)
})

test('swaps', async () => {
  await rv(`
    fn rotate(&x, &y) {
      z = x + y
      t = x # TODO support match
      x = y
      y = t
      return z
    }

    fn f(x) {
      y = 3.0
      z = rotate(&x, &y)
      return z * x + y
    }

    test f(2.0) == 17.0
    test diff(f, 2.0) == 4.0
    test grad(f, 2.0) == [4.0]
  `)
})

test('user rules take precedence', async () => {
  await rv(`
    fn double(x) { x + x }

    @extend
    fn forward($double, [x], [dx]) { [double(x), dx * 100.0] }

    @extend
    fn reverse($double, x) { [double(x), fn (d) { [d * 100.0] }] }

    test diff(double, 1.0) == 100.0
    test grad(double, 1.0) == [100.0]
  `)
})

test('nested differentiation', async () => {
  await rv(`
    ${close}

    fn f(x) { x * x * x }
    fn df(x) { diff(f, x) }
    fn ddf(x) { diff(df, x) }

    test f(2.0) == 8.0
    test df(2.0) == 12.0
    test ddf(2.0) == 12.0

    fn dsin(x) { diff(sin, x) }
    test close?(dsin(0.0), 1.0)
    test close?(diff(dsin, 0.0), 0.0)
  `)
})

test('reverse over structures', async () => {
  await rv(`
    fn sumsq(xs) { xs[1] * xs[1] + xs[2] * xs[2] }
    test grad(sumsq, [1.0, 2.0]) == [[2.0, 4.0]]

    # inactive arguments still get a cotangent
    fn mix(x, n: Int) { x * x }
    test grad(mix, 3.0, 2) == [6.0, 0]
  `)
})

test('pullbacks', async () => {
  await rv(`
    fn square(x) { x * x }

    [y, back] = reverse(square, 3.0)
    test y == 9.0
    test back(1.0) == [6.0]
    test back(2.0) == [12.0]

    # a gradient can be taken inside another function
    fn dsquare(x) { grad(square, x)[1] }
    test dsquare(4.0) == 8.0
  `)
})
