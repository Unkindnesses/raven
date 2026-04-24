import { PassThrough } from 'node:stream'
import * as assert from 'assert'
import { test } from 'uvu'
import { REPL } from '../src/cli/repl.js'

test('basic eval', async () => {
  const repl = new REPL({ stdout: new PassThrough() })
  try {
    await repl.init()
    assert.strictEqual((await repl.eval('2+2')).trim(), '4')
    assert.strictEqual((await repl.eval('2.0')).trim(), '2.0')
    assert.strictEqual((await repl.eval('5 / 2.5')).trim(), '2.0')
    assert.strictEqual((await repl.eval('xs = [1, 2, 3]')).trim(), '[1, 2, 3]')
    assert.strictEqual((await repl.eval('append(&xs, 4)')).trim(), '[1, 2, 3, 4]')
    assert.strictEqual((await repl.eval('xs')).trim(), '[1, 2, 3, 4]')
    assert.strictEqual((await repl.eval('x = widen(5)')).trim(), '5')
    assert.strictEqual((await repl.eval('x + 2')).trim(), '7')
  } finally {
    await repl.close()
  }
})

test('undo restores globals', async () => {
  const repl = new REPL({ stdout: new PassThrough() })
  try {
    await repl.init()
    assert.strictEqual((await repl.eval('x = 1')).trim(), '1')
    assert.strictEqual((await repl.eval('x = 2')).trim(), '2')
    assert.strictEqual((await repl.eval('undo')).trim(), '# undo x = 2')
    assert.strictEqual((await repl.eval('x')).trim(), '1')
  } finally {
    await repl.close()
  }
})

test('undo restores function definitions', async () => {
  const repl = new REPL({ stdout: new PassThrough() })
  try {
    await repl.init()
    await repl.eval('fn inc(x) { x + 1 }')
    assert.strictEqual((await repl.eval('inc(1)')).trim(), '2')
    await repl.eval('fn inc(x) { x + 2 }')
    assert.strictEqual((await repl.eval('undo')).trim(), '# undo fn inc(x) { x + 2 }')
    assert.strictEqual((await repl.eval('inc(1)')).trim(), '2')
  } finally {
    await repl.close()
  }
})

test('undo n restores multiple entries', async () => {
  const repl = new REPL({ stdout: new PassThrough() })
  try {
    await repl.init()
    assert.strictEqual((await repl.eval('x = 1')).trim(), '1')
    assert.strictEqual((await repl.eval('x = 2')).trim(), '2')
    assert.strictEqual((await repl.eval('x = 3')).trim(), '3')
    assert.strictEqual((await repl.eval('undo 2')).trim(), '# undo x = 3\n# undo x = 2')
    assert.strictEqual((await repl.eval('x')).trim(), '1')
  } finally {
    await repl.close()
  }
})

test('clear removes globals', async () => {
  const repl = new REPL({ stdout: new PassThrough() })
  try {
    await repl.init()
    assert.strictEqual((await repl.eval('x = 1')).trim(), '1')
    assert.strictEqual(await repl.eval('clear x'), '')
    await assert.rejects(() => repl.eval('x'), /x is not defined/)
  } finally {
    await repl.close()
  }
})

test('invoke closure object', async () => {
  const repl = new REPL({ stdout: new PassThrough() })
  try {
    await repl.init()
    await repl.eval('TInt64 = Pack(Literal(Int), bits 64)')
    await repl.eval('f = Function(fn (x, y) { Int64(js.Math.pow(x, y)) }, [TInt64, TInt64], TInt64)')
    assert.strictEqual((await repl.eval('f(2, 3)')).trim(), '8')
  } finally {
    await repl.close()
  }
})

test('async task', async () => {
  const repl = new REPL({ stdout: new PassThrough() })
  try {
    await repl.init()
    await repl.eval('fn task() { println("ok") }')
    assert.match(await repl.eval('await(async(task))'), /ok/)
  } finally {
    await repl.close()
  }
})

test.run()
