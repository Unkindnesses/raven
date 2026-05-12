import { test } from 'vitest'
import * as assert from 'assert'
import {
  Type, tag, pack, vpack, bits, float64, float32,
  issubset, union, recur, unroll, repr
} from '../src/frontend/types.js'
import { isEqual } from '../src/utils/isEqual.js'

class Random {
  private state: number

  constructor(seed: number) {
    this.state = seed >>> 0
  }

  next(): number {
    this.state = (1664525 * this.state + 1013904223) >>> 0
    return this.state / 2 ** 32
  }

  int(max: number): number {
    return Math.floor(this.next() * max)
  }

  range(min: number, max: number): number {
    return min + this.int(max - min + 1)
  }

  choose<T>(xs: readonly T[]): T {
    return xs[this.int(xs.length)]
  }
}

type TypeGenerator = (g: Generator) => Type

class Generator {
  constructor(readonly random: Random, readonly depth = 10) { }

  down(): Generator {
    return new Generator(this.random, this.depth - 1)
  }

  next(): Type {
    return this.random.choose([primitive, constant, nested])(this)
  }
}

function primitive(g: Generator): Type {
  return g.random.choose([bits(64), bits(32), float64(), float32()])
}

function constant(g: Generator): Type {
  return g.random.choose([constNumber, constTag])(g)
}

function constNumber(g: Generator): Type {
  return g.random.choose([bits(64, g.random.range(1, 5)), float64(g.random.range(1, 5))])
}

function constTag(g: Generator): Type {
  return tag(g.random.choose(['a', 'b', 'c', 'd']))
}

// Assume tags of type `Tag` for now
function ppack(g: Generator): Type {
  return recur(pack(constTag(g), ...Array.from({ length: g.random.range(0, 2) }, () => g.next())))
}

function pvpack(g: Generator): Type {
  return recur(vpack(constTag(g), g.next()))
}

function punion(g: Generator): Type {
  return union(g.next(), g.next())
}

function nested(g: Generator): Type {
  if (g.depth <= 0) return g.random.choose([primitive, constant])(g)
  return g.random.choose<TypeGenerator>([ppack, pvpack, punion])(g.down())
}

function iff(a: boolean, b: boolean): boolean {
  return a === b
}

function assertSame(actual: Type, expected: Type, message: string) {
  assert.deepEqual(actual, expected, `${message}\nactual: ${repr(actual)}\nexpected: ${repr(expected)}`)
}

test('type operations satisfy generated algebraic properties', () => {
  const random = new Random(42)
  for (let i = 0; i < 100; i++) {
    const g = new Generator(random)
    const [A, B, C] = [g.next(), g.next(), g.next()]
    const U = union(A, B)
    const context = `case ${i}\nA: ${repr(A)}\nB: ${repr(B)}\nC: ${repr(C)}`

    assert.equal(iff(issubset(A, B) && issubset(B, A), isEqual(A, B)), true, context)
    assertSame(recur(unroll(A)), A, `recur(unroll(A)) == A\n${context}`)
    assertSame(union(A, A), A, `union(A, A) == A\n${context}`)
    assertSame(union(A, B), union(B, A), `union(A, B) == union(B, A)\n${context}`)
    assertSame(union(union(A, B), C), union(A, union(B, C)), `union associativity\n${context}`)
    assertSame(union(A, U), U, `union(A, union(A, B)) == union(A, B)\n${context}`)
    assert.equal(issubset(A, U), true, `issubset(A, union(A, B))\n${context}`)
    if (issubset(A, B)) assertSame(U, B, `issubset(A, B) implies union(A, B) == B\n${context}`)
    assert.equal(iff(issubset(A, B), issubset(U, B)), true, `subset iff\n${context}`)
  }
})
