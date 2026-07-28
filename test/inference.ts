import { beforeAll, test } from 'vitest'
import * as assert from 'assert'
import { Compiler, load } from '../src/cli/compile.js'
import { tag, list, pack, vpack, int64, int32, bits, float64, onion, recursive, recurrence, nil, Any, Ref, Ptr, Tag, String, Type } from '../src/frontend/types.js'
import { key, Sig } from '../src/middle/abstract.js'
import { source } from '../src/middle/load.js'
import { Binding } from '../src/frontend/modules.js'
import { only } from '../src/utils/map.js'
import { unreachable } from '../src/utils/ir.js'

let compiler: Compiler

beforeAll(async () => {
  compiler = await Compiler.create(load)
})

function result(comp: Compiler, f: Tag, args: Type) {
  let [, ret] = comp.pipe.inferred.get([f, f, args])
  return ret
}

const some = (x: Type) => pack(tag('common.Some'), x)
const optional = (x: Type) => onion(nil, some(x))

test('infer identity', async () => {
  await compiler.reload(source('', 'fn id(x) { x }'))
  let ret = result(compiler, tag('id'), list(int64()))
  assert.deepEqual(ret, list(int64()))
})

test('infer Nil', () => {
  let ret = result(compiler, tag('common.Nil'), list())
  assert.deepEqual(ret, list(nil))
})

test('nil const', () => {
  const ret = compiler.pipe.defs.global(new Binding(tag('common'), 'nil'))
  assert.deepEqual(ret, nil)
})

test('infer bool', async () => {
  await compiler.reload(source('', 'fn id() { Bool(bits"1") }'))
  let ret = result(compiler, tag('id'), list())
  assert.deepEqual(ret, list(pack(tag('common.Bool'), bits(1, 1))))
})

test('infer int32', async () => {
  await compiler.reload(source('', 'fn id() { Int32(64*1024) }'))
  let ret = result(compiler, tag('id'), list())
  assert.deepEqual(ret, list(int32(64 * 1024)))
})

test('castTrait narrows any', () => {
  let ret = result(compiler, tag('common.castTrait'), list(tag('common.core.Float64'), Any))
  assert.deepEqual(ret, list(optional(float64())))
  ret = result(compiler, tag('common.castTrait'), list(tag('common.Int64'), Any))
  assert.deepEqual(ret, list(optional(int64())))
  ret = result(compiler, tag('common.castTrait'), list(tag('common.String'), Any))
  assert.deepEqual(ret, list(optional(String())))
  ret = result(compiler, tag('common.castTrait'), list(tag('common.Nil'), Any))
  assert.deepEqual(ret, list(optional(nil)))
  ret = result(compiler, tag('common.castTrait'), list(tag('common.core.Ref'), Any))
  assert.deepEqual(ret, list(optional(Ref)))
  ret = result(compiler, tag('common.castTrait'), list(tag('common.Ptr'), Any))
  assert.deepEqual(ret, list(optional(Ptr())))
})

test('trait types', () => {
  const inf = compiler.pipe.inferred
  assert.deepEqual(inf.traitType(tag('common.Int64')), int64())
  assert.deepEqual(inf.traitType(pack(tag('common.Params'), tag('common.UInt'), 21n)), pack(tag('common.UInt'), bits(21)))
  assert.deepEqual(inf.traitType(tag('common.String')), String())
  assert.deepEqual(inf.traitType(tag('NoSuchTrait')), unreachable)
})

test('infer pow', async () => {
  await compiler.reload(source('', `
    fn pow(x, n: Int64) {
      r = one(x)
      while n > 0 {
        n = n - one(n)
        r = r * x
      }
      return r
    }
  `))
  let ret = result(compiler, tag('pow'), list(2n, 3n))
  assert.deepEqual(ret, list(8n))
})

test('infer fib recursive', async () => {
  await compiler.reload(source('', `
    fn fib(n) {
      if widen(n <= 1) {
        return n
      } else {
        return fib(n-1) + fib(n-2)
      }
    }
  `))
  let ret = result(compiler, tag('fib'), list(20n))
  assert.deepEqual(ret, list(int64()))
})

test('expansion resolves recursion redirects', async () => {
  await compiler.reload(source('', `
    fn fib(n) {
      if widen(n <= 1) {
        return n
      } else {
        return fib(n-1) + fib(n-2)
      }
    }
  `))
  const fib = only(compiler.pipe.defs.methods(tag('fib')))
  const sig: Sig = [fib, int64(20)]
  const [, ret] = compiler.pipe.inferred.get(sig)
  assert.deepEqual(ret, int64())

  const dispatcherSig: Sig = [tag('fib'), tag('fib'), list(int64(20))]
  const [inferred] = compiler.pipe.inferred.get(dispatcherSig)
  assert.ok(Array.from(inferred).some(([_, st]) => st.expr.head === 'cast'))
  const expanded = compiler.pipe.expanded.get(dispatcherSig)
  assert.ok(!Array.from(expanded).some(([_, st]) => st.expr.head === 'cast'))
})

// Widening replaces the deep frames with a single one over the union of their
// argument types; these all need more than one round to settle afterwards, so
// they exercise the bookkeeping around retiring the frames it replaces.

test('infer widens growing return type', async () => {
  await compiler.reload(source('', `
    fn countdown(n) {
      if n <= 0 { return nil } else { return Some(countdown(n-1)) }
    }
  `))
  const ret = result(compiler, tag('countdown'), list(40n))
  assert.deepEqual(ret, list(recursive(onion(nil, some(recurrence)))))
})

test('infer widens mutual recursion', async () => {
  await compiler.reload(source('', `
    fn even(n) { if n <= 0 { return nil } else { return odd(n-1) } }
    fn odd(n) { Some(even(n-1)) }
  `))
  const ret = result(compiler, tag('even'), list(40n))
  assert.deepEqual(ret, list(recursive(onion(nil, some(recurrence)))))
})

test('infer widens growing argument type', async () => {
  await compiler.reload(source('', `
    fn build(xs, n) {
      if n <= 0 { return xs } else { return build(append(&xs, n), n-1) }
    }
  `))
  const ret = result(compiler, tag('build'), list(list(), 40n))
  assert.deepEqual(ret, list(vpack(tag('common.List'), int64())))
})

test('infer non-terminating recursion', async () => {
  await compiler.reload(source('', 'fn spin(n) { spin(n+1) }'))
  assert.deepEqual(result(compiler, tag('spin'), list(1n)), unreachable)
})

test('infer fib sequence', async () => {
  await compiler.reload(source('', `
    fn fib(n) { fib(n-1) + fib(n-2) }
    fn fib(1) { 1 }
    fn fib(0) { 0 }

    fn fibSequence(n) {
      xs = []
      for i = range(1, n) {
        append(&xs, fib(i))
      }
      return xs
    }
  `))
  let ret = result(compiler, tag('fibSequence'), list(5n))
  assert.deepEqual(ret, list(list(1n, 1n, 2n, 3n, 5n)))
})

test('infer traces straight-line code', async () => {
  await compiler.reload(source('', `
    fn plus1(x) { x + 1 }
    fn chain(x) { plus1(plus1(plus1(x))) }
  `))
  const chain = only(compiler.pipe.defs.methods(tag('chain')))
  const plus1 = only(compiler.pipe.defs.methods(tag('plus1')))
  const [ir, ret] = compiler.pipe.inferred.get([chain, int64()])
  assert.deepEqual(ret, int64())
  assert.ok(!compiler.pipe.inferred.inf.frames.has(key([plus1, int64()])))
  assert.ok(!Array.from(ir).some(([_, st]) => st.expr.head === 'call'))
})

test('infer merge keeps known record structure', async () => {
  const P = (a: Type, b: Type) => pack(tag('common.Pair'), a, b)

  await compiler.reload(source('', `
    fn mergeRecords() {
      b1 = record(Pair(tag"a", 1), Pair(tag"b", 2))
      b2 = record(Pair(tag"b", widen(2)), Pair(tag"d", 3))
      merge(&b1, b2)
    }
  `))

  let ret = result(compiler, tag('mergeRecords'), list())
  const record = pack(tag('common.Record'), P(tag('a'), int64(1)), P(tag('b'), int64(2)), P(tag('d'), int64(3)))
  assert.deepEqual(ret, list(onion(nil, record)))

  await compiler.reload(source('', `
    fn mergeRecords() {
      b1 = record(Pair(tag"a", 1), Pair(tag"b", 2))
      b2 = record(Pair(tag"b", 2), Pair(tag"d", 3))
      merge(&b1, b2)
    }
  `))

  ret = result(compiler, tag('mergeRecords'), list())
  assert.deepEqual(ret, list(record))
})
