import { test } from 'vitest'
import { test as rv } from '../../src/cli/test.js'

test('show', async () => {
  await rv('show 2+2', { output: '(2 + 2) = 4' })
})

test('print float', async () => {
  await rv('println(2.0)', { output: '2' })
})

test('float multiply', async () => {
  await rv(`
    test 1.5 * 2.0 == 3.0
  `)
})

test('mixed int/float +', async () => {
  await rv(`
    test 1.0 + 2 == 3.0
    test 2 + 1.0 == 3.0
  `)
})

test('bigint arithmetic and promotion', async () => {
  await rv(`
    x = BigInt("9223372036854775808")
    test string(x + 2) == "9223372036854775810"
    test x + 0.5 == 9223372036854775808.0 + 0.5
    test string(BigInt(UInt64(0xFFFFFFFFFFFFFFFF))) == "18446744073709551615"
    test string(big(-1)) == "-1"
  `)
})

test('hex literals', async () => {
  await rv(`
    test bitsize(0x1) == 8
    test bitsize(0xFF) == 8
    test bitsize(0x001) == 16
  `)
})

test('char literals', async () => {
  await rv(`
    test c"🔥" == Char(0x1F525)
    test string(c"🔥") == "🔥"
  `)
})

test('global invalidation', async () => {
  await rv(`
    x = 1
    fn test() { x + 1 }
    x = test()
    test x == 2
  `, { options: { inline: false } })
})

test('undefined variable errors', async () => {
  await rv('println(x)', { error: true, output: 'x is not defined' })
})

test('undefined function errors', async () => {
  await rv('foo()', { error: true, output: 'foo is not defined' })
})

test('non-tag call errors', async () => {
  await rv(`
    x = 1, x()
  `, { error: true, output: 'No matching method: 1: []' })
})

test('non boolean condition fails', async () => {
  await rv(`if "true" { println("hi") }`,
    { error: true, output: 'Non-boolean used as a condition' })
})

test('arity mismatch errors', async () => {
  await rv(`
    fn foo(a, b) {
      return a+b
    }

    foo(1, 2, 3)
  `, { error: true, output: 'No matching method: tag"/foo"' })
})

test('op precedence', async () => {
  await rv(`
    test 1 + 2 * 3 == 7
    test (1 + 2) * 3 == 9
    test 8.0 / 2.0 * 2.0 == 8.0
    test 10 - 3 - 2 == 5
    test (true || false && false) == true
    test ((true || false) && false) == false
    `)
})

test('prefix logical negation operator', async () => {
  await rv(`
    test !false
    test !!true
    test !(true && false)
    test !true == false
  `)
})

test('prefix arithmetic negation operator', async () => {
  await rv(`
    test (-1 + 2 == 1)
    test (-(1 + 2) == -3)
    test (-1 * -2 == 2)
    test (-(1.5) == -1.5)
    test (-(-3) == 3)
  `)
})

test('pattern literal interpolation', async () => {
  await rv(`
    Answer = tag"/Answer"

    fn answer(_) { false }
    fn answer($Answer) { true }

    test answer(Answer)
    test !answer(tag"/Other")

    expected = widen(42)
    matches = if let $expected = widen(42) { true } else { false }
    differs = if let $expected = widen(41) { false } else { true }
    test matches
    test differs
  `)
})

test('relu', async () => {
  await rv(`
    fn relu(x) {
      if x > 0 {
        x
      } else {
        0
      }
    }

    test relu(widen(5)) == 5
    test relu(widen(-5)) == 0
  `)
})

test('function redefinition', async () => {
  await rv(`
    fn foo(x) { x + 1 }

    test foo(widen(5)) == 6

    fn foo(x) { x + 2 }

    test foo(widen(5)) == 7
  `)
})

test('pow', async () => {
  await rv(`
    x = widen(2)
    n = widen(3)
    r = x^n

    test r == 8

    fn pow(x, n: Int) {
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
  `)
})

test('factorial', async () => {
  await rv(`
    test factorial(0) == 1
    test factorial(5) == 120
    test factorial(big(6)) == big(720)
  `)
})

test('loop continue', async () => {
  await rv(`
    total = 0
    i = 0
    while true {
      i = i + 1
      if i == 6 {
        break
      }
      if i == 3 {
        continue
      }
      total = total + i
    }
    test total == 12
  `)
})

test('labeled loop control', async () => {
  await rv(`
    sum = 0
    i = 0
    @label outer
    while true {
      i = i + 1
      @label inner
      while true {
        if i == 5 { break outer }
        if i == 2 { continue outer }
        break
      }
      sum = sum + i
    }
    test sum == 8
  `)
})

test('for loop labels', async () => {
  await rv(`
    total = 0
    @label outer
    for x = range(1, 5) {
      if x == 2 { continue outer }
      total = total + x
      if x == 4 { break outer }
    }
    test total == 8
  `)
})

test('for unpacking', async () => {
  await rv(`
    total = 0
    pairs = [[widen(1), widen(2)], [widen(3), widen(4)]]
    for [x, y] = pairs {
      total = total + x * y
    }
    test total == 14
  `)
})

test('labeled block', async () => {
  await rv(`
    total = 0
    iterations = 0
    @label block
    let {
      iterations = iterations + 1
      if iterations > 10 { break block }
      if rem(iterations, 2) == 0 { continue block }
      total = total + iterations
      continue block
    }
    test total == 25
    test iterations == 11
  `)
})

test('recursion widening', async () => {
  await rv(`
    fn fib(n) {
      if n <= 1 {
        return n
      } else {
        return fib(n-1) + fib(n-2)
      }
    }

    test fib(20) == 6765
  `)
})

test('dynamic dispatch', async () => {
  await rv(`
    fn fib(n) { fib(n-1) + fib(n-2) }
    fn fib(0) { 0 }
    fn fib(1) { 1 }

    test fib(widen(20)) == 6765
  `)
})

test('return value casting', async () => {
  await rv(`
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
  `)
})

test('global variable', async () => {
  await rv(`
    n = widen(5)

    fn foo(m) { return n + m }

    test foo(3) == 8
  `)
})

test('scoping', async () => {
  await rv(`
    x = 1, { x = 2 }
    test x == 2
  `)
})

test('scoping', async () => {
  await rv(`
    x = widen(1), { x = 2 }
    test x == 2
  `)
})

test('let shadowing', async () => {
  await rv(`
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
  `)
})

test('match literal', async () => {
  await rv(`
    test !nil?(match(widen(1), Pattern.Literal(1)))
    test nil?(match(widen(2), Pattern.Literal(1)))
  `)
})

test('match overloads', async () => {
  await rv(`
    fn test(1, x) { x }
    fn test(2, x) { x + 1 }

    test test(widen(1), widen(3)) == 3
    test test(widen(2), widen(3)) == 4

    fn test(Complex(a, b)) { a + b }
    fn test(Complex(a, a)) { a }

    test test(Complex(1, widen(2))) == 3
    test test(Complex(2, widen(2))) == 2
  `)
})

test('runtime signature pattern', async () => {
  await rv(`
    bundle Alpha { A() }
    bundle Beta { B() }
    T = if widen(true) { Alpha } else { Beta }

    fn classify(_) { "fallback" }
    fn classify(_: T) { "dynamic" }

    test classify(A()) == "dynamic"
    test classify(B()) == "fallback"
  `)
})

test('signature pattern residual effects', async () => {
  await rv(`
    bundle Wrapped { Wrap(x) }
    bundle Other { OtherValue() }
    state = js\`return { calls: 0 }\`

    fn patternConstructor() {
      js\`return \\state.calls += 1\`
      Wrap
    }

    fn extract(_) { 0 }
    fn extract(patternConstructor()(x)) { x }

    test extract(Wrap(42)) == 42
    test extract(OtherValue()) == 0
    test Int64(js\`return \\state.calls\`) == 1 # TODO keep side effects from failing sigs
  `)
})

test('tag union', async () => {
  await rv(`
    fn eitherSym(x) {
      if x {
        return tag"foo"
      } else {
        return tag"bar"
      }
    }

    test string(eitherSym(widen(true))) == "foo"
    test string(eitherSym(widen(false))) == "bar"
  `)
})

test('clear variable', async () => {
  await rv(`
    x = 1
    clear x
    println(x)
  `, { error: true, output: 'x is not defined' })
})

test('clear function', async () => {
  await rv(`
    fn square(x) { x * x }
    clear square
    tag"square"(5)
  `, { error: true, output: 'No matching method' })
})

test('append in if', async () => {
  await rv(`
    {
      xs = []
      if widen(true) {
        append(&xs, "foo")
        append(&xs, "bar")
      }
      test xs == ["foo", "bar"]
    }
  `)
})

test('typemin/max', async () => {
  await rv(`
    test typemin(Int32(5)) == -2147483648
    test typemax(Int32(5)) ==  2147483647
  `)
})

test('allocs', async () => {
  await rv(`
    test (allocs 1 + 2) == 0
    test (allocs collect("hello")) == 6
  `)
})
