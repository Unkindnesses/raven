import * as path from 'node:path'
import * as fs from 'node:fs'
import { spawnSync } from 'node:child_process'
import { test } from 'vitest'
import assert from 'assert'
import { test as rv, runNode } from '../../src/cli/test.js'
import { compile } from '../../src/cli/compile.js'

const fixtureDir = path.join(__dirname, '..', 'fixtures')

test('print js', async () => {
  await rv('println(js(1))', { output: '1' })
})

test('js template string', async () => {
  await rv('println(js`return 42`)', { output: '42' })
})

test('js template with interpolation', async () => {
  await rv('x = 10\nprintln(js`return \\x + 1`)', { output: '11' })
})

test('await', async () => {
  await rv(`
    {
      p = js\`return new Promise(resolve => resolve(5))\`
      obj = await(p)
      test Float64(obj) == Float64(5)
    }
  `)
})

test('int/string union', async () => {
  await rv(`
    fn either(x) {
      if x {
        return widen(5)
      } else {
        return "foo"
      }
    }

    # TODO can't yet compile dynamic 'show' fallback
    fn prn(x: Int64) { println(x) }
    fn prn(x: String) { println(x) }

    prn(either(widen(true)))
    prn(either(widen(false)))
  `, { output: '5\nfoo' })
})

test('result ok pattern', async () => {
  await rv(`
    {
      f = js\`return { meth: n => new Promise(resolve => resolve(n)) }\`
      Ok(x) = errcall(f, "meth", 5)
      test Float64(x) == 5.0
    }
  `)
})

// Currently only works because of dispatcher trimming; we can't compile the
// generic fallback `show` method.
test('show result ok', async () => {
  await rv(`
    {
      f = js\`return { meth: n => new Promise(resolve => resolve(n)) }\`
      x = errcall(f, "meth", 7)
      show x
    }
  `, { output: 'x = Ok(js(7))' })
})

test('unwrap ok', async () => {
  await rv(`
    {
      f = js\`return { meth: n => new Promise(resolve => resolve(n)) }\`
      x = unwrap(errcall(f, "meth", 5))
      test Float64(x) == 5.0
    }
  `)
})

test('unwrap err', async () => {
  await rv(`
    f = js\`return { meth: () => { throw new Error('dummy error') } }\`
    unwrap(errcall(f, "meth"))
  `, { error: true, output: ['unwrap Err', 'dummy error'] })
})

test('program args', async () => {
  await rv(`
    for arg = args() {
      println(arg)
    }
  `, { output: 'node' })
})

test('brainfuck interpreter', async () => {
  const [, js] = await compile(path.join(fixtureDir, 'brainfuck.rv'))
  const { output } = await runNode(js, [path.join(fixtureDir, 'test.bf')])
  assert.strictEqual(output, 'Hello World!\n')
})

test('js int conversion', async () => {
  await rv(`
    test Int64(js(2828255673)) == 2828255673
    test Int32(js(2828255673)) == -1466711623
  `)
})

test('js callable', async () => {
  await rv(`
    f = js.Math.sqrt
    test f(2) == js.Math.sqrt(2)
  `)
})

test('async', async () => {
  await rv(`
    start = JSFuture()
    done = JSFuture()

    a = async {
      resolve!(start)
      await(done)
      println("first")
    }

    b = async {
      await(start)
      println("second")
      resolve!(done)
    }
  `, { output: /second\s+first/ })
})

test('js future', async () => {
  await rv(`
    future = JSFuture()
    test !resolved?(future)
    resolve!(future, js(42))
    test resolved?(future)
    resolve!(future, js(100))
    test Int64(await(future)) == 42

    rejected = JSFuture()
    part(rejected, 1).catch(js\`return () => undefined\`)
    reject!(rejected, js(5))
    test resolved?(rejected)
  `)
})

test('channel', async () => {
  await rv(`
    ch = Channel(Float64, 1)
    start = JSFuture()

    a = async {
      put!(ch, 10.0)
      println("put1")
      resolve!(start)
      put!(ch, 20.0)
      println("put2")
    }

    b = async {
      await(start)
      println(take!(ch))
      println(take!(ch))
    }
  `, { output: /put1\s+10.0\s+put2\s+20.0/ })
})

test('select', async () => {
  await rv(`
    ch1 = Channel(Float64, 1)
    ch2 = Channel(Float64, 1)
    put!(ch2, 20.0)

    select {
      case x = take!(ch1) { println(x) }
      case y = take!(ch2) { println(y) }
    }
  `, { output: /20.0/ })

  await rv(`
    ch = Channel(Float64, 1)
    put!(ch, 1.0)
    done = JSFuture()

    task = async {
      println(take!(ch))
      println(take!(ch))
      resolve!(done)
    }

    select {
      case put!(ch, 2.0) { println("put") }
    }
    await(done)
  `, { output: /1.0\s+put\s+2.0/ })
})

test('wasi', async () => {
  await compile(path.join(fixtureDir, 'wasi.rv'),
    { options: { memcheck: false } })
  const wasm = path.join(fixtureDir, 'wasi.wasm')
  const component = path.join(fixtureDir, 'wasi.cli.wasm')
  const wasiCli = path.join(__dirname, '..', '..', 'wasi-cli')
  if (!fs.existsSync(wasiCli)) {
    const result = spawnSync('git', ['clone', 'https://github.com/WebAssembly/wasi-cli', wasiCli], { encoding: 'utf-8' })
    if (result.status !== 0) throw new Error(`Failed to clone wasi-cli: ${result.stderr}`)
  }
  const wit = path.join(wasiCli, 'wit')
  let result = spawnSync('wasm-tools', ['component', 'embed', wit, '--world', 'command', wasm, '-o', component], { encoding: 'utf-8' })
  if (result.status !== 0) throw new Error(`wasm-tools failed: ${result.stderr}`)
  result = spawnSync('wasm-tools', ['component', 'new', component, '-o', component], { encoding: 'utf-8' })
  if (result.status !== 0) throw new Error(`wasm-tools failed: ${result.stderr}`)
  result = spawnSync('wasmtime', [component], { encoding: 'utf-8' })
  assert.strictEqual(result.stdout, 'hello!\n')
})
