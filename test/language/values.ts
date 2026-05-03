import { test } from 'vitest'
import { test as rv } from '../../src/cli/test.js'

test('complex arithmetic', async () => {
  await rv(`
    z = Complex(widen(5), widen(6))
    w = Complex(widen(2), widen(3))

    test abs2(z) == 61
    test conj(z) == Complex(5, -6)
    test z*z == Complex(-11, 60)
    test z/w == Complex(28.0/13.0, -3.0/13.0)
    test z/2 == Complex(2.5, 3.0)
    test z == z
    test z != z*z
  `)
})

test('complex libm', async () => {
  await rv(`
    fn approx(a: Complex, b: Complex) {
      abs2(a - b) < 0.000000001
    }

    z = Complex(1.25, -0.75)
    w = Complex(2.0, 1.0)

    test sqrt(Complex(-4.0)) == Complex(0.0, 2.0)
    test approx(exp(Complex(0.0, pi)), Complex(-1.0))
    test approx(log(Complex(-1.0)), Complex(0.0, pi))
    test approx(sqrt(z) * sqrt(z), z)
    test approx(exp(log(z)), z)
    test approx(pow(z, 2.0), z*z)
    test approx(sin(z)*sin(z) + cos(z)*cos(z), Complex(1.0))
    test approx(tan(z), sin(z) / cos(z))
    test approx(cosh(z)*cosh(z) - sinh(z)*sinh(z), Complex(1.0))
    test approx(tanh(z), sinh(z) / cosh(z))
    test approx(asin(sin(z)), z)
    test approx(cos(acos(w)), w)
    test approx(atan(tan(z)), z)
    test approx(asinh(sinh(z)), z)
    test approx(cosh(acosh(w)), w)
    test approx(atanh(tanh(z)), z)
    test approx(cbrt(z) * cbrt(z) * cbrt(z), z)
  `)
}, 10000)

test('concat strings', async () => {
  await rv(`
    test concat("a", "b") == "ab"
  `)
})

test('rounding', async () => {
  await rv(`
    test round(1.3) == 1
    test round(1.7) == 2
    test round(-1.3) == -1
    test round(-1.7) == -2
  `)
})

test('collect string', async () => {
  await rv(`
    test collect("foo") == [c"f", c"o", c"o"]
  `)
})

test('string indexing', async () => {
  await rv(`
    test utf16("foo")[1] == 0x0066
    test "foo"[1] == c"f"
  `)
})

test('utf8 view', async () => {
  await rv(`
    {
      bs = utf8("hi")
      test collect(bs) == [0x68, 0x69]

      bs = utf8("🌍")
      test collect(bs) == [0xF0, 0x9F, 0x8C, 0x8D]
    }
  `)
})

test('regex contains', async () => {
  await rv(`
    test contains?("1, 2, 3", r\`\\d\`)
  `)
})

test('regex match', async () => {
  await rv(`
    ms = matches("1, 2, 3", r\`\\d\`)
    test collect(ms) == [["1"], ["2"], ["3"]]
  `)
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
    test 1/3 ==
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
