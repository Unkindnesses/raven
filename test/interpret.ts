import { beforeAll, test } from 'vitest'
import * as assert from 'assert'
import * as types from '../src/frontend/types.js'
import { Compiler, load } from '../src/cli/compile.js'

let compiler: Compiler

beforeAll(async () => {
  compiler = await Compiler.create(load)
})

test('interpret core pack literal params', () => {
  const int = compiler.pipe.interp

  const args = types.list(types.tag('common.patterns.Literal'), types.tag('common.patterns.Params'))
  const result = int.eval(types.tag('common.core.pack'), args)

  assert.ok(result)
  assert.deepEqual(result, types.pack(types.tag('common.patterns.Literal'), types.tag('common.patterns.Params')))
})

test('interpret matchTrait without Bool trait match', () => {
  const int = compiler.pipe.interp
  const args = types.list(types.tag('common.integer.Int'), types.bits(1, 1))
  const result = int.eval(types.tag('common.patterns.matchTrait'), args)
  assert.deepEqual(result, types.nil)
})
