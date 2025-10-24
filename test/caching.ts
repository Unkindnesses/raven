import { test } from 'uvu'
import * as assert from 'assert'
import { Compiler } from '../src/backend/compiler'
import { Binding } from '../src/frontend/modules'
import * as types from '../src/frontend/types'
import { tag } from '../src/frontend/types'
import { source } from '../src/middle/load'
import { fingerprint, reset } from '../src/utils/cache'
import { asArray } from '../src/utils/map'
import { key, Sig } from '../src/middle/abstract'

test('globals', () => {
  const compiler = new Compiler(source('', 'foo = 1, bar = 1'))
  const defs = compiler.pipe.defs
  const foo = new Binding(tag(''), 'foo')
  const bar = new Binding(tag(''), 'bar')

  assert.deepEqual(defs.global(foo), types.int64(1))
  assert.deepEqual(defs.global(bar), types.int64(1))

  const fooId = defs.globals.id(foo)
  const barId = defs.globals.id(bar)

  compiler.reload(source('', 'foo = 1, bar = 2'))
  reset(defs, [compiler.pipe.sources])

  assert.deepEqual(defs.global(foo), types.int64(1))
  assert.deepEqual(defs.global(bar), types.int64(2))

  assert.notEqual(fooId, defs.globals.id(foo)) // TODO should be equal
  assert.notEqual(barId, defs.globals.id(bar))
})

test('methods', () => {
  const compiler = new Compiler(source('', 'fn foo(x) { x+1 }'))
  const defs = compiler.pipe.defs

  assert.equal(defs.methods(tag('foo')).length, 1)
  assert.ok(defs.methods(tag('common.core.main')).length > 0)

  const fooId = defs.table.id(tag('foo'))
  const mainId = defs.table.id(tag('common.core.main'))

  compiler.reload(source('', 'fn foo(x) { x+2 }'))
  reset(compiler.pipe)

  assert.equal(defs.methods(tag('foo')).length, 1)
  assert.notEqual(fooId, defs.table.id(tag('foo')))
  assert.equal(mainId, defs.table.id(tag('common.core.main')))
})

test('inference', () => {
  const compiler = new Compiler(source('', 'n = 1, fn foo(x) { x+n }, foo(5)'))
  const inf = compiler.pipe.inferred
  let [, fooType] = asArray(inf.get([tag('foo'), types.list(types.int64(5))]))
  assert.deepEqual(fooType, types.list(types.int64(6)))

  inf.get([tag('common.+'), types.list(types.int64(), types.int64())])
  const plusId = inf.results.id(key([tag('common.+'), types.list(types.int64(), types.int64())]))

  compiler.reload(source('', 'n = 2, fn foo(x) { x+n }, foo(5)'));

  [, fooType] = asArray(inf.get([tag('foo'), types.list(types.int64(5))]))
  assert.deepEqual(fooType, types.list(types.int64(7)))
  assert.equal(plusId, inf.results.id(key([tag('common.+'), types.list(types.int64(), types.int64())])))
})

test('compiler', () => {
  const compiler = new Compiler()
  reset(compiler.pipe)
  compiler.pipe.wasm.get([tag('common.malloc!'), types.list(types.int32())])
  const before = fingerprint(compiler.pipe)
  reset(compiler.pipe)
  const after = fingerprint(compiler.pipe)

  assert.equal(before.size, after.size)
  for (const id of before) assert.ok(after.has(id))
  assert.deepEqual(before, after)
})

test('match method', () => {
  const compiler = new Compiler()
  const sig: Sig = [tag('common.matchTrait'), types.list(tag('common.Int64'), types.int64())]

  assert.ok(!compiler.pipe.inferred.results.iscached(key(sig)))
  const matchResult = compiler.pipe.inferred.get(sig)
  assert.ok(Array.isArray(matchResult))

  compiler.reload(source('', `
    @extend, fn matchTrait(tag"Lit", x: pack(tag"Lit", val)) { Some(x) }
  `))

  assert.ok(compiler.pipe.inferred.results.iscached(key(sig)))
})

test.run()
