import { test } from 'uvu'
import * as assert from 'assert'
import { tag, list, int64, float64, bits, pack, String, nil, Tag, Type } from '../src/frontend/types.js'
import { Compiler, load } from '../src/cli/compile.js'
import { Tracer } from '../src/middle/tracer.js'
import { some } from '../src/utils/map.js'

let tr: Tracer

function trace(f: Tag, ...args: Type[]) {
  return some(tr._trace(f, f, list(...args)))
}

test.before(async () => {
  const compiler = await Compiler.create(load)
  tr = new Tracer(compiler.pipe.defs, compiler.pipe.interp)
})

test('trace biteqz', () => {
  let [ir, ret] = trace(tag('common.core.biteqz'), bits(32))
  assert.deepEqual(ret, list(bits(1)))
})

test('trace identity', () => {
  let [ir, ret] = trace(tag('common.identity'), int64())
  assert.deepEqual(ret, list(int64()))
})

test('trace Nil', () => {
  let [ir, ret] = trace(tag('common.Nil'))
  assert.deepEqual(ret, list(pack(tag('common.Nil'))))
})

test('trace Float64', () => {
  let [ir, ret] = trace(tag('common.core.Float64'), int64())
  assert.deepEqual(ret, list(float64()))
})

test('trace i64 +', () => {
  let [ir, ret] = trace(tag('common.+'), int64(), int64())
  assert.deepEqual(ret, list(int64()))
  assert.ok(ir.length <= 15)
})

test('trace f64 +', () => {
  let [ir, ret] = trace(tag('common.+'), float64(), float64())
  assert.deepEqual(ret, list(float64()))
  assert.ok(ir.length <= 15)
})

test('trace const i64 +', () => {
  let [ir, ret] = trace(tag('common.+'), int64(2), int64(2))
  assert.deepEqual(ret, list(int64(4)))
  assert.ok(ir.length === 1)
})

test('trace const f64 +', () => {
  let [ir, ret] = trace(tag('common.+'), float64(2), float64(2))
  assert.deepEqual(ret, list(float64(4)))
  assert.ok(ir.length === 1)
})

test('trace print', () => {
  let [ir, ret] = trace(tag('common.println'), String())
  assert.deepEqual(ret, list(nil))
  assert.ok(ir.length <= 50) // TODO shorten
})

test.run()
