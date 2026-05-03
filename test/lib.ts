import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { spawnSync } from 'node:child_process'
import { test } from 'vitest'
import assert from 'assert'

const source = `
  export { arity, add, state }

  x = 40

  fn state() { x }

  fn arity() { 10 }
  fn arity(_: JSObject) { 20 }

  fn add(a: JSObject, b: JSObject) {
    x = x + 1
    Int32(a) + Int32(b)
  }
`

const runner = `
  import { arity, add, state } from './foo.js'

  const out = [
    await state(),
    await arity(),
    await arity(1),
    await add(2, 3),
    await state()
  ]
  console.log(JSON.stringify(out))
`

const cliPath = path.join(process.cwd(), 'dist/cli/index.js')
const tscPath = path.join(process.cwd(), 'node_modules/typescript/bin/tsc')

function testBuildJS(args: string[], hasWasm: boolean) {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'raven-lib-'))
  const rvPath = path.join(dir, 'foo.rv')
  const jsPath = path.join(dir, 'foo.js')
  const wasmPath = path.join(dir, 'foo.wasm')
  const runPath = path.join(dir, 'run.mjs')
  try {
    fs.writeFileSync(rvPath, source)

    let out = spawnSync(process.execPath, ['--enable-source-maps', cliPath, 'build', '--js', ...args, rvPath], {
      encoding: 'utf8'
    })
    assert.strictEqual(out.status, 0, out.stderr || out.stdout)
    assert.ok(fs.existsSync(jsPath))
    assert.strictEqual(fs.existsSync(wasmPath), hasWasm)
    const js = fs.readFileSync(jsPath, 'utf8')
    if (!hasWasm) assert.ok(js.includes('__ravenInline('))

    fs.writeFileSync(runPath, runner)

    out = spawnSync(process.execPath, [runPath], {
      encoding: 'utf8'
    })
    assert.strictEqual(out.status, 0, out.stderr || out.stdout)
    assert.strictEqual(out.stdout.trim(), '[40,10,20,5,40]')
  } finally {
    fs.rmSync(dir, { recursive: true, force: true })
  }
}

test('build --js exports js shims with sidecar wasm', () => {
  testBuildJS([], true)
})

test('build --js --embed exports js shims with inline wasm', () => {
  testBuildJS(['--embed'], false)
})

test('build --js exports @ts signatures to emitted d.ts', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'raven-types-'))
  const rvPath = path.join(dir, 'foo.rv')
  const jsPath = path.join(dir, 'foo.js')
  const dtsPath = path.join(dir, 'types/foo.d.ts')
  try {
    fs.writeFileSync(rvPath, `
      export { add }

      @ts \`(a: number, b: number) => Promise<number>\`
      fn add(a, b) { Float64(a) + Float64(b) }
    `)

    let out = spawnSync(process.execPath, ['--enable-source-maps', cliPath, 'build', '--js', rvPath], {
      encoding: 'utf8'
    })
    assert.strictEqual(out.status, 0, out.stderr || out.stdout)
    assert.ok(fs.existsSync(jsPath))

    out = spawnSync(process.execPath, [
      tscPath, '--allowJs', '--declaration', '--emitDeclarationOnly', '--noCheck',
      '--module', 'nodenext', '--moduleResolution', 'nodenext', '--target', 'esnext',
      'foo.js', '--outDir', 'types'
    ], { cwd: dir, encoding: 'utf8' })
    assert.strictEqual(out.status, 0, out.stderr || out.stdout)
    assert.ok(fs.existsSync(dtsPath))
    const dts = fs.readFileSync(dtsPath, 'utf8')
    assert.ok(dts.includes('add: (a: number, b: number) => Promise<number>'))
  } finally {
    fs.rmSync(dir, { recursive: true, force: true })
  }
})
