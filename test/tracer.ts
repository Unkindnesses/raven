import { test } from 'uvu'
import * as assert from 'assert'
import { tag, list, int64, float64, bits, pack } from '../src/frontend/types.js'
import { Compiler, load } from '../src/cli/compile.js'
import { Tracer } from '../src/middle/tracer.js'
import { some } from '../src/utils/map.js'

let tr: Tracer

test.before(async () => {
  const compiler = await Compiler.create(load)
  tr = new Tracer(compiler.pipe.defs, compiler.pipe.interp)
})

test('trace biteqz', () => {
  let [ir, ret] = some(tr._trace(tag('common.core.biteqz'), list(bits(32))))
  assert.deepEqual(ret, list(bits(1)))
})

test('trace identity', () => {
  let [ir, ret] = some(tr._trace(tag('common.identity'), list(int64())))
  assert.deepEqual(ret, list(int64()))
})

test('trace Nil', () => {
  let [ir, ret] = some(tr._trace(tag('common.Nil'), list()))
  assert.deepEqual(ret, list(pack(tag('common.Nil'))))
})

test('trace Float64', () => {
  let [ir, ret] = some(tr._trace(tag('common.core.Float64'), list(int64())))
  assert.deepEqual(ret, list(float64()))
})

test('trace i64 +', () => {
  let [ir, ret] = some(tr._trace(tag('common.+'), list(int64(), int64())))
  assert.deepEqual(ret, list(int64()))
  assert.ok(ir.length <= 15)
})

test('trace f64 +', () => {
  let [ir, ret] = some(tr._trace(tag('common.+'), list(float64(), float64())))
  assert.deepEqual(ret, list(float64()))
  assert.ok(ir.length <= 15)
})

test('trace const i64 +', () => {
  let [ir, ret] = some(tr._trace(tag('common.+'), list(int64(2), int64(2))))
  assert.deepEqual(ret, list(int64(4)))
  assert.ok(ir.length === 1)
})

test('trace const f64 +', () => {
  let [ir, ret] = some(tr._trace(tag('common.+'), list(float64(2), float64(2))))
  assert.deepEqual(ret, list(float64(4)))
  assert.ok(ir.length === 1)
})

test.run()
