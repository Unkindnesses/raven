import { test } from 'uvu'
import * as assert from 'assert'
import { Compiler, load } from '../src/cli/compile.js'
import { tag, list, pack, int64, int32, bits } from '../src/frontend/types.js'
import { Sig } from '../src/middle/abstract.js'
import { source } from '../src/middle/load.js'
import { Binding } from '../src/frontend/modules.js'
import { asArray } from '../src/utils/map.js'

let compiler: Compiler

test.before(async () => {
  compiler = await Compiler.create(load)
})

function result(comp: Compiler, sig: Sig) {
  let [, ret] = asArray(comp.pipe.inferred.get(sig))
  return ret
}

test('infer identity', async () => {
  await compiler.reload(source('', 'fn id(x) { x }'))
  let ret = result(compiler, [tag('id'), list(int64())])
  assert.deepEqual(ret, list(int64()))
})

test('infer Nil', () => {
  let ret = result(compiler, [tag('common.Nil'), list()])
  assert.deepEqual(ret, list(pack(tag('common.Nil'))))
})

test('nil const', () => {
  const nil = compiler.pipe.defs.global(new Binding(tag('common'), 'nil'))
  assert.deepEqual(nil, pack(tag('common.Nil')))
})

test('infer bool', async () => {
  await compiler.reload(source('', 'fn id() { Bool(bits"1") }'))
  let ret = result(compiler, [tag('id'), list()])
  assert.deepEqual(ret, list(pack(tag('common.Bool'), bits(1, 1))))
})

test('infer int32', async () => {
  await compiler.reload(source('', 'fn id() { Int32(64*1024) }'))
  let ret = result(compiler, [tag('id'), list()])
  assert.deepEqual(ret, list(int32(64 * 1024)))
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
  let ret = result(compiler, [tag('pow'), list(2n, 3n)])
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
  let ret = result(compiler, [tag('fib'), list(20n)])
  assert.deepEqual(ret, list(int64()))
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
  let ret = result(compiler, [tag('fibSequence'), list(5n)])
  assert.deepEqual(ret, list(list(1n, 1n, 2n, 3n, 5n)))
})

test.run()
