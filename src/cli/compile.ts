import * as wasm from '../backend/wasm.js'
import { Options, withOptions } from '../utils/options.js'
import * as path from 'path'
import { chmod, mkdir, readFile, writeFile } from 'fs/promises'
import { spawn, SpawnOptions } from 'node:child_process'
import { dirname } from './dirname.js'
import { Compiler } from '../backend/compiler.js'

export { Compiler, compile, compileJS, exec, load }

const common = path.resolve(dirname, "../../common")

async function load(file: string): Promise<[string, string]> {
  if (!path.isAbsolute(file)) file = path.join(common, file)
  return [file, await readFile(file, 'utf8')]
}

interface CompileConfig {
  dir?: string
  compiler?: Compiler
  options?: Partial<Options>
  output?: string
  strip?: boolean
}

async function compile(file: string, config: CompileConfig = {}): Promise<[Compiler, string]> {
  let { dir = path.dirname(file), compiler, options = {}, output, strip = false } = config
  const base = path.basename(file, path.extname(file))
  const wasmPath = output ?? path.join(dir, `${base}.wasm`)
  await mkdir(path.dirname(wasmPath), { recursive: true })
  await withOptions(options, async () => {
    compiler ??= await Compiler.create(load)
    const em = await compiler.reload(file)
    if (!(em instanceof wasm.BatchEmitter)) throw new Error('nope')
    const bytes = wasm.emitwasm(em, compiler.pipe.wasm, strip)
    await writeFile(wasmPath, Buffer.from(bytes))
  })
  return [compiler!, wasmPath]
}

async function compileJS(file: string, config: CompileConfig = {}): Promise<[Compiler, string]> {
  let { dir = path.dirname(file), compiler, options = {}, output, strip = false } = config
  const base = path.basename(file, path.extname(file))
  const jsPath = output ?? path.join(dir, `${base}.js`)
  await mkdir(path.dirname(jsPath), { recursive: true })
  await withOptions(options, async () => {
    compiler ??= await Compiler.create(load)
    const em = await compiler.reload(file)
    if (!(em instanceof wasm.BatchEmitter)) throw new Error('nope')
    const bytes = wasm.emitwasm(em, compiler.pipe.wasm, strip)
    const base64 = Buffer.from(bytes).toString('base64')
    const runtime = await readFile(execPath, 'utf8')
    await writeFile(jsPath, `${runtime}\nbinary = Buffer.from('${base64}', 'base64')\n`)
    await chmod(jsPath, 0o755)
  })
  return [compiler!, jsPath]
}

async function run(cmd: string, args: readonly string[] = [], options: SpawnOptions = {}) {
  return await new Promise<[number | null, NodeJS.Signals | null]>((resolve, reject) => {
    const child = spawn(cmd, [...args], options)
    child.on('error', reject)
    child.on('close', (code, signal) => resolve([code, signal]))
  })
}

const execPath = path.join(dirname, '../../dist/cli/exec.js')

async function exec(file: string, args: string[] = [], config?: CompileConfig): Promise<void> {
  if (path.extname(file).toLowerCase() !== '.wasm') [, file] = await compile(file, config)
  await run('node', ['--enable-source-maps', '--experimental-wasm-jspi', execPath, file, ...args], { stdio: 'inherit' })
}
