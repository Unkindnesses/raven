import { PassThrough } from 'node:stream'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import * as assert from 'assert'
import { test } from 'vitest'
import { REPL } from '../src/cli/repl.js'

// The repl loads relative imports from the working directory, as if it were a
// file sitting there.
async function inTempDir(files: Record<string, string>, f: () => Promise<void>) {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'raven-repl-'))
  const cwd = process.cwd()
  for (const [name, source] of Object.entries(files)) {
    fs.mkdirSync(path.dirname(path.join(dir, name)), { recursive: true })
    fs.writeFileSync(path.join(dir, name), source)
  }
  process.chdir(dir)
  try {
    await f()
  } finally {
    process.chdir(cwd)
    fs.rmSync(dir, { recursive: true, force: true })
  }
}

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
    await repl.eval('f = Function(fn (x, y) { Int64(js.Math.pow(x, y)) }, [Int64, Int64], Int64)')
    assert.strictEqual((await repl.eval('f(2, 3)')).trim(), '8')
  } finally {
    await repl.close()
  }
})

test('async task', async () => {
  const repl = new REPL({ stdout: new PassThrough() })
  try {
    await repl.init()
    assert.match(await repl.eval('await(async { println("ok") })'), /ok/)
  } finally {
    await repl.close()
  }
})

test('import a file from the working directory', async () => {
  await inTempDir({
    'greeting.rv': `
      export { greet, Greeting }

      bundle Greeting { Greeting() }

      fn greet() { "hello from the repl" }
    `,
    'util/twice.rv': `
      export { twice }

      fn twice(x) { x * 2 }
    `
  }, async () => {
    const repl = new REPL({ stdout: new PassThrough() })
    try {
      await repl.init()
      assert.strictEqual(await repl.eval('import { greet, Greeting } from "./greeting.rv"'), '')
      assert.strictEqual((await repl.eval('greet()')).trim(), '"hello from the repl"')
      assert.strictEqual((await repl.eval('string(tag(Greeting()))')).trim(), '"greeting.Greeting"')
      await repl.eval('import { twice } from "./util/twice.rv"')
      assert.strictEqual((await repl.eval('twice(21)')).trim(), '42')
    } finally {
      await repl.close()
    }
  })
})
