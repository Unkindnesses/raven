import { beforeAll, test } from 'vitest'
import * as assert from 'assert'
import { tag, list, int64, float64, bits, pack, String, nil, Tag, Type } from '../src/frontend/types.js'
import { Compiler, load } from '../src/cli/compile.js'
import { Tracer } from '../src/middle/tracer.js'
import { some } from '../src/utils/map.js'

let tr: Tracer

function trace(f: Tag, ...args: Type[]) {
  return some(tr.trace(f, f, list(...args)))
}

function traceCount(f: Tag, ...args: Type[]) {
  const result = some(tr.trace(f, f, list(...args)))
  return [result, tr.count] as const
}

beforeAll(async () => {
  const compiler = await Compiler.create(load)
  tr = new Tracer(compiler.pipe.defs, compiler.pipe.lowered, compiler.pipe.interp, compiler.pipe.methods)
})

test('trace biteqz', () => {
  let [, ret] = trace(tag('common.core.biteqz'), bits(32))
  assert.deepEqual(ret, list(bits(1)))
})

test('trace identity', () => {
  let [, ret] = trace(tag('common.core.identity'), int64())
  assert.deepEqual(ret, list(int64()))
})

test('trace Nil', () => {
  let [, ret] = trace(tag('common.core.Nil'))
  assert.deepEqual(ret, list(pack(tag('common.core.Nil'))))
})

test('trace Float64', () => {
  let [, ret] = trace(tag('common.core.Float64'), int64())
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

const P = (a: Type, b: Type) => pack(tag('common.record.Pair'), a, b)
const R = (...fields: Type[]) => pack(tag('common.record.Record'), ...fields)

test('trace keyindex shortcut', () => {
  const [hit, hitCount] = traceCount(tag('common.record.keyindex'), R(P(tag('a'), int64()), P(tag('b'), String())), tag('b'))
  assert.deepEqual(hit[1], list(int64(2)))
  assert.equal(hitCount, 1)

  const [miss, missCount] = traceCount(tag('common.record.keyindex'), R(P(tag('a'), int64()), P(tag('b'), String())), tag('c'))
  assert.deepEqual(miss[1], list(nil))
  assert.equal(missCount, 1)
})

test('trace match shortcut', () => {
  const node = (name: string, ...parts: Type[]) => pack(tag(`common.patterns.${name}`), ...parts)
  const bind = (name: string) => node('Bind', tag(name), node('Hole'))
  const pat = node('Pack', node('Literal', tag('common.list.List')), bind('a'), bind('b')) // [a, b]

  const [hit, hitCount] = traceCount(tag('common.match'), list(int64(), String()), pat)
  assert.deepEqual(hit[1], list(R(P(tag('a'), int64()), P(tag('b'), String()))))
  assert.equal(hitCount, 1)

  const [miss, missCount] = traceCount(tag('common.match'), list(int64()), pat)
  assert.deepEqual(miss[1], list(nil))
  assert.equal(missCount, 1)
})
