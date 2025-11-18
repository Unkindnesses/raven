import { test } from 'uvu'
import * as assert from 'assert'
import * as types from '../src/frontend/types.js'
import { Compiler, load } from '../src/cli/compile.js'

const compiler = new Compiler(load)

test('interpret core pack literal params', () => {
  const int = compiler.pipe.interp

  const args = types.list(types.tag('common.Literal'), types.tag('common.Params'))
  const result = int.get(types.tag('common.core.pack'), [args])

  assert.ok(result)
  assert.deepEqual(result, types.list(types.pack(types.tag('common.Literal'), types.tag('common.Params'))))
})

test('interpret matchTrait without Bool trait match', () => {
  const int = compiler.pipe.interp
  const args = types.list(types.tag('common.Int'), types.bits(1, 1))
  const result = int.get(types.tag('common.matchTrait'), [args])
  assert.deepEqual(result, types.list(types.nil))
})

test.run()
