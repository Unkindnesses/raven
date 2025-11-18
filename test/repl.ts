import { PassThrough } from 'node:stream'
import * as assert from 'assert'
import { test } from 'uvu'
import { REPL } from '../src/cli/repl.js'

test('basic eval', async () => {
  const repl = await REPL.create({ stdout: new PassThrough() })
  assert.strictEqual((await repl.eval('2+2')).trim(), '4')
  assert.strictEqual((await repl.eval('xs = [1, 2, 3]')).trim(), '[1, 2, 3]')
  assert.strictEqual((await repl.eval('append(&xs, 4)')).trim(), '[1, 2, 3, 4]')
  assert.strictEqual((await repl.eval('xs')).trim(), '[1, 2, 3, 4]')
  assert.strictEqual((await repl.eval('x = widen(5)')).trim(), '5')
  assert.strictEqual((await repl.eval('x + 2')).trim(), '7')
  repl.close()
})

test.run()
