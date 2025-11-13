import * as path from 'node:path'
import * as fs from 'node:fs'
import { spawnSync } from 'node:child_process'
import { test } from 'uvu'
import assert from 'assert'
import { test as rv, runNode } from '../src/test'
import { compile } from '../src/backend/compiler'

test('show', async () => {
  await rv('show 2+2', { output: '(2 + 2) = 4' })
})

test('print js', async () => {
  await rv('println(js(1))', { output: '1' })
})

test('print float', async () => {
  await rv('println(2.0)', { output: '2' })
})

test('undefined variable errors', async () => {
  await rv('println(x)', { error: true, output: 'x is not defined' })
})

test('undefined function errors', async () => {
  await rv('foo()', { error: true, output: 'foo is not defined' })
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
  `, { error: true, output: 'No matching method' })
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

test('complex arithmetic', async () => {
  await rv(`
    z = Complex(widen(5), widen(6))

    test abs2(z) == 61
    test (z*z) == Complex(widen(-11), widen(60))
    test z == z
    test z != (z*z)
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
    test (part(xs, widen(1)) + part(xs, widen(2))) == (4+5)
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
      itr = iterator(xs)

      val = next(&itr)
      test not(nil?(val))
      test part(val, 1) == 5

      val = next(&itr)
      test not(nil?(val))
      test part(val, 1) == 6

      val = next(&itr)
      test nil?(val)
    }
  `)
})

test('simpledict', async () => {
  await rv(`
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

      test not(nil?(merge(d, tag"b", "foo")))
      test nil?(merge(d, tag"b", "bar"))
      test not(nil?(merge(d, tag"c", 9)))

      test not(nil?(merge(d, tag"a", widen(7))))
      test nil?(merge(d, tag"a", widen(8)))
    }
  `)
})

test('merge sequences', async () => {
  await rv(`
    {
      b1 = seq(Pair(tag"a", 1), Pair(tag"b", 2))
      b2 = seq(Pair(tag"b", widen(2)), Pair(tag"d", 3))
      test not(nil?(merge(&b1, b2)))

      b1 = seq(Pair(tag"a", 1), Pair(tag"b", 2))
      b2 = seq(Pair(tag"b", widen(3)), Pair(tag"d", 3))
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
      test string(tag(xs)) == "common.Empty"
      test nparts(xs) == 0
      test empty?(xs)
      test length(xs) == 0
      test allocationCount() == 0

      xs = foo(widen(5))
      test string(tag(xs)) == "common.Prepend"
      test nparts(xs) == 2
      test part(xs, 2) == 5
      test part(part(xs, 1), 2) == 4
      test not(empty?(xs))
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
      test length(rest(xs)) == 4

      xs = collect(range(1, widen(5)))
      test length(rest(xs)) == 4
    }
  `)
})

test('for expression collects values', async () => {
  await rv(`
    ys = (for x = range(1, 5) { x })
    println(ys)
  `, { output: '[1, 2, 3, 4, 5]' })
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
    test not(nil?(match(widen(1), Literal(1))))
    test nil?(match(widen(2), Literal(1)))
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

test('array destructuring', async () => {
  await rv(`
    {
      xs = [widen(2), widen(3)]
      [a, b] = xs
      test (a + b) == 5
    }
  `)
})

test('array destructuring with match', async () => {
  await rv(`
    {
      xs = [widen(2), widen(2)]
      [a, a] = xs
      test (a + a) == 4
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
      test (a + b) == 5
    }
  `)
})

test('concat strings', async () => {
  await rv(`
    test concat("a", "b") == "ab"
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

test('await', async () => {
  await rv(`
    {
      obj = await(call(js(), "dummyPromise", 5))
      test Float64(obj) == Float64(5)
    }
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

test('int/string union', async () => {
  await rv(`
    fn either(x) {
      if x {
        return widen(5)
      } else {
        return "foo"
      }
    }

    # TODO can't yet compile dynamic 'show' fallback
    fn prn(x: Int64) { println(x) }
    fn prn(x: String) { println(x) }

    prn(either(widen(true)))
    prn(either(widen(false)))
  `, { output: '5\nfoo' })
})

test('result ok pattern', async () => {
  await rv(`
    {
      Ok(x) = errcall(js(), "dummyPromise", 5)
      test Float64(x) == 5.0
    }
  `)
})

// Currently only works because of dispatcher trimming; we can't compile the
// generic fallback `show` method.
test('show result ok', async () => {
  await rv(`
    {
      x = errcall(js(), "dummyPromise", 7)
      show x
    }
  `, { output: 'x = Ok(7)' })
})

test('unwrap ok', async () => {
  await rv(`
    {
      x = unwrap(errcall(js(), "dummyPromise", 5))
      test Float64(x) == 5.0
    }
  `)
})

test('unwrap err', async () => {
  await rv(`
    unwrap(errcall(js(), "dummyErr"))
  `, { error: true, output: ['unwrap Err', 'dummy error'] })
})

test('program args', async () => {
  await rv(`
    for arg = args() {
      println(arg)
    }
  `, { output: 'node' })
})

test('append in if', async () => {
  await rv(`
    {
      xs = []
      if widen(true) {
        append(&xs, "foo")
        append(&xs, "bar")
      }
      test part(xs, 2) == "bar"
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

test('rounding', async () => {
  await rv(`
    test round(1.3) == 1
    test round(1.7) == 2
    test round(-1.3) == -1
    test round(-1.7) == -2
  `)
})

// TODO list equality
test('collect string', async () => {
  await rv(`
    show collect("foo")
  `, { output: '["f", "o", "o"]' })
})

test('print bits', async () => {
  await rv(`
    bitcast = tag"common.core.bitcast"
    show bits(Int32(10))
  `, { output: 'bits"00000000000000000000000000001010"' })
})

test('float parts', async () => {
  await rv(`
    showPack widen(Float32(0.1))
  `, { output: 'widen(Float32(0.1)) = pack(tag"common.core.Float32", bits"00111101110011001100110011001101")' })
})

test('float pack', async () => {
  await rv(`
    test (1/3) ==
      pack(Float64, bits"0011111111010101010101010101010101010101010101010101010101010101")
  `)
})

test('print dynamic bits', async () => {
  await rv(`
    show widen(bits"101")
  `, { output: 'bits"101"' })
})

test('print custom int', async () => {
  await rv(`
    show Int(widen(bits"100"))
  `, { output: 'oftype(int 3, -4)' })
})

test('part/pack float32', async () => {
  await rv(`
    {
      x = widen(Float32(0.1))
      test pack(Float32, part(x, 1)) == x
    }
  `)
})

test('brainfuck interpreter', async () => {
  const [, js] = await compile(path.join(__dirname, 'language', 'brainfuck.rv'))
  const { code, output } = await runNode(js, [path.join(__dirname, 'language', 'test.bf')])
  assert.strictEqual(output, 'Hello World!\n')
})

test('typemin/max', async () => {
  await rv(`
    test typemin(Int32(5)) == -2147483648
    test typemax(Int32(5)) ==  2147483647
  `)
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

    show group(widen(3))
  `, { output: '[[1], [1, 2], [1, 2, 3]]' })
})

test('wasi', async () => {
  await compile(path.join(__dirname, 'language', 'wasi.rv'),
    { options: { memcheck: false } })
  const wasm = path.join(__dirname, 'language', 'wasi.wasm')
  const component = path.join(__dirname, 'language', 'wasi.cli.wasm')
  const wasiCli = path.join(__dirname, '../wasi-cli')
  if (!fs.existsSync(wasiCli)) {
    const result = spawnSync('git', ['clone', 'https://github.com/WebAssembly/wasi-cli', wasiCli], { encoding: 'utf-8' })
    if (result.status !== 0) throw new Error(`Failed to clone wasi-cli: ${result.stderr}`)
  }
  const wit = path.join(wasiCli, 'wit')
  let result = spawnSync('wasm-tools', ['component', 'embed', wit, '--world', 'command', wasm, '-o', component], { encoding: 'utf-8' })
  if (result.status !== 0) throw new Error(`wasm-tools failed: ${result.stderr}`)
  result = spawnSync('wasm-tools', ['component', 'new', component, '-o', component], { encoding: 'utf-8' })
  if (result.status !== 0) throw new Error(`wasm-tools failed: ${result.stderr}`)
  result = spawnSync('wasmtime', [component], { encoding: 'utf-8' })
  assert.strictEqual(result.stdout, 'hello!\n')
})

test('show gc', async () => {
  await rv('show 2+2', { output: '(2 + 2) = 4', options: { gc: true } })
})

test.run()
