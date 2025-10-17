import { mkdtemp, rm, writeFile } from 'node:fs/promises'
import * as path from 'node:path'
import { tmpdir } from 'node:os'
import { spawn } from 'node:child_process'
import * as assert from 'assert'
import { compile, Compiler } from './backend/compiler'
import { type Options } from './utils/options'
import { dirname } from './dirname'

export { run, test, runNode }

const execPath = path.join(dirname, '../build/backend/exec.js')

let compiler = new Compiler()

interface Result {
  code: number
  output: string
}

async function runNode(wasm: string, args: string[] = []): Promise<Result> {
  return await new Promise((resolve, reject) => {
    const child = spawn(process.execPath, ['--experimental-wasm-stack-switching', execPath, wasm, ...args], { stdio: ['ignore', 'pipe', 'pipe'] })
    let output = ''
    child.stdout?.on('data', chunk => { output += chunk.toString() })
    child.stderr?.on('data', chunk => { output += chunk.toString() })
    child.on('error', reject)
    child.on('close', code => {
      resolve({ code: code ?? 1, output })
    })
  })
}

async function run(code: string, options?: Partial<Options>): Promise<Result> {
  const comp = options === undefined ? compiler : new Compiler()
  const dir = await mkdtemp(path.join(tmpdir(), 'raven-test-'))
  const rvPath = path.join(dir, 'test.rv')
  try {
    await writeFile(rvPath, code)
    const wasm = await compile(rvPath, { options, dir, compiler: comp })
    return await runNode(wasm)
  } finally {
    await rm(dir, { recursive: true, force: true })
  }
}

interface TestOptions {
  error?: boolean
  output?: string | string[] | RegExp
  options?: Partial<Options>
}

async function test(code: string, opts: TestOptions = {}): Promise<void> {
  const { error = false, output, options } = opts
  const { code: exitCode, output: out } = await run(code, options)
  if (!error && exitCode !== 0) throw new Error(out.trim() || `Process exited with status ${exitCode}`)
  assert.strictEqual(exitCode !== 0, error, `expected ${error ? '' : 'no '}error, got status ${exitCode}\n${out}`)
  if (output instanceof RegExp) {
    assert.ok(output.test(out), `expected output to match ${output.source}\n${out}`)
    return
  }
  if (typeof output === 'string') {
    assert.ok(out.includes(output), `expected output to contain ${JSON.stringify(output)}\n${out}`)
    return
  }
  if (Array.isArray(output)) {
    for (const expected of output)
      assert.ok(out.includes(expected), `expected output to contain ${JSON.stringify(expected)}\n${out}`)
    return
  }
  if (!error) {
    const lines = out.split('\n').map(line => line.trim()).filter(Boolean)
    assert.ok(lines.length > 0, 'expected at least one test result')
    for (const line of lines) {
      const match = line.match(/^(\w+): (.*)$/)
      assert.ok(match, `unexpected output line: ${line}`)
      const [, status, message] = match
      assert.strictEqual(status, 'pass', message)
    }
  }
}
