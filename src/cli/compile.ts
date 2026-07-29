import * as wasm from '../backend/wasm.js'
import * as types from '../frontend/types.js'
import * as ast from '../frontend/ast.js'
import { Binding } from '../frontend/modules.js'
import * as patterns from '../frontend/patterns.js'
import { Options, withOptions } from '../utils/options.js'
import * as path from 'path'
import { chmod, mkdir, mkdtemp, readFile, rm, writeFile } from 'fs/promises'
import { spawn, SpawnOptions } from 'node:child_process'
import { tmpdir } from 'node:os'
import { dirname } from './dirname.js'
import { Compiler } from '../backend/compiler.js'
import { Loader } from '../frontend/packages.js'
import { Def } from '../dwarf/index.js'

export { Compiler, compile, compileJS, exec, loader, load }

const common = path.resolve(dirname, "../../common/common.rv")

function loader(packages: Record<string, string> = {}): Loader {
  return new Loader(file => readFile(file, 'utf8'), { common, ...packages })
}

const load = loader()

interface CompileConfig {
  dir?: string
  compiler?: Compiler
  options?: Partial<Options>
  packages?: Record<string, string>
  output?: string
  embed?: boolean
  esbuild?: boolean
  strip?: boolean
}

async function compile(file: string, config: CompileConfig = {}): Promise<[Compiler, string]> {
  let { dir = path.dirname(file), compiler, options = {}, packages, output, strip = false } = config
  const base = path.basename(file, path.extname(file))
  const wasmPath = output ?? path.join(dir, `${base}.wasm`)
  await mkdir(path.dirname(wasmPath), { recursive: true })
  await withOptions(options, async () => {
    compiler ??= await Compiler.create(loader(packages))
    const em = await compiler.reload(file)
    const bytes = wasm.emitwasm(em, strip)
    await writeFile(wasmPath, Buffer.from(bytes))
  })
  return [compiler!, wasmPath]
}

function isJSIdentifier(name: string): boolean {
  return /^[$A-Z_][0-9A-Z_$]*$/i.test(name)
}

function buildPaths(file: string, dir: string, output?: string): { js: string, wasm: string } {
  if (!output) {
    const base = path.basename(file, path.extname(file))
    return {
      js: path.join(dir, `${base}.js`),
      wasm: path.join(dir, `${base}.wasm`)
    }
  }
  const { dir: outDir, name, ext } = path.parse(output)
  const wasmBase = ext ? path.join(outDir, name) : output
  return { js: output, wasm: `${wasmBase}.wasm` }
}

function exportedFunctions(compiler: Compiler): [string, types.Tag][] {
  const mod = compiler.pipe.sources.module(types.tag(''))
  const out: [string, types.Tag][] = []
  for (const name of [...mod.exports.keys()].sort()) {
    const value = compiler.pipe.defs.resolve_static(new Binding(types.tag(''), name))
    if (!(value instanceof types.Tag)) continue
    out.push([name, value])
  }
  return out
}

// TODO better to have a generic means for converting to JS functions. Exported
// globals can implicitly convert to JS, and we don't need to wrap.
function libWrapperMethod(name: string, f: types.Tag) {
  const args = ast.Call(types.tag('common.collect'), ast.Call(types.tag('common.JSObject'), ast.symbol('args')))
  const body = ast.Call(types.tag('common.js'), ast.Call(f, ast.Splat(args)))
  return { body, meta: Def(name) }
}

function exportTSSignature(compiler: Compiler, fn: types.Tag): string | undefined {
  const methods = compiler.pipe.defs.methods(fn)
  const tss = methods
    .map(method => method.key.ts?.trim())
    .filter((ts): ts is string => !!ts)
  if (!tss.length) return '(...args: any[]) => Promise<any>'
  const signatures: string[] = []
  for (const ts of tss) {
    if (!signatures.includes(ts)) signatures.push(ts)
  }
  if (signatures.length === 1) return signatures[0]
  return signatures.map(sig => `(${sig})`).join(' & ')
}

function jsRuntime(exports: [string, string, string | undefined][], runtime: string, config: { wasmFile?: string, base64?: string, memcheck?: boolean, esbuild?: boolean }): string {
  const { wasmFile, base64, memcheck = false, esbuild = false } = config
  if (esbuild && !base64) runtime += `import __raven_wasm from ${JSON.stringify(`./${wasmFile}`)}\n`
  const init = base64
    ? `\nconst __raven = await __ravenInline(${JSON.stringify(base64)}, ${memcheck})\n`
    : esbuild
      ? `\nconst __raven = await __ravenLoad(new URL(__raven_wasm, import.meta.url), ${memcheck})\n`
      : `\nconst __raven = await __ravenLoad(new URL(${JSON.stringify(`./${wasmFile}`)}, import.meta.url), ${memcheck})\n`
  const wrappers = exports.map(([name, wasmName, ts], i) => {
    const fn = `__raven_fn_${i}`
    const doc = ts ? `/** @type {${ts}} */\n` : ''
    return `const ${fn} = __raven(${JSON.stringify(wasmName)})
${doc}export const ${name} = (...args) => ${fn}(args)`
  }).join('\n')
  return `${runtime}${init}${wrappers}\n`
}

async function compileJS(file: string, config: CompileConfig = {}): Promise<[Compiler, string]> {
  let { dir = path.dirname(file), compiler, options = {}, packages, output, embed: inlineWasm = false, esbuild = false, strip = false } = config
  const memcheck = options.memcheck ?? false
  const paths = buildPaths(file, dir, output)
  await mkdir(path.dirname(paths.js), { recursive: true })
  if (!inlineWasm) await mkdir(path.dirname(paths.wasm), { recursive: true })
  await withOptions({ ...options, memcheck }, async () => {
    compiler ??= await Compiler.create(loader(packages))
    const em = await compiler.reload(file)
    const exports: [string, string, string | undefined][] = []
    const runtime = await readFile(libPath, 'utf8')
    const mod = compiler.pipe.sources.module(types.tag(''))
    for (const [i, [name, fn]] of exportedFunctions(compiler).entries()) {
      if (!isJSIdentifier(name))
        throw new Error(`Cannot export ${JSON.stringify(name)} as a JS binding`)
      const tag = types.tag(`__raven.lib.${i}`)
      const sig = ast.List(tag, ast.symbol('args'))
      const method = mod.method(tag, patterns.signature(sig), { ...libWrapperMethod(tag.path, fn), sig })
      const wname = `raven.lib.${name}`
      compiler.pipe.export(em, [method, types.Ref], wname)
      exports.push([name, wname, exportTSSignature(compiler, fn)])
    }
    const bytes = wasm.emitwasm(em, strip)
    if (inlineWasm) {
      const base64 = Buffer.from(bytes).toString('base64')
      await writeFile(paths.js, jsRuntime(exports, runtime, { base64, memcheck }))
    } else {
      await writeFile(paths.wasm, Buffer.from(bytes))
      await writeFile(paths.js, jsRuntime(exports, runtime, { wasmFile: path.basename(paths.wasm), memcheck, esbuild }))
    }
    await chmod(paths.js, 0o755)
  })
  return [compiler!, paths.js]
}

async function run(cmd: string, args: readonly string[] = [], options: SpawnOptions = {}) {
  return await new Promise<[number | null, NodeJS.Signals | null]>((resolve, reject) => {
    const child = spawn(cmd, [...args], options)
    child.on('error', reject)
    child.on('close', (code, signal) => resolve([code, signal]))
  })
}

const libPath = path.join(dirname, '../../dist/cli/lib.js')
const execPath = path.join(dirname, '../../dist/cli/exec.js')

async function exec(file: string, args: string[] = [], config?: CompileConfig): Promise<void> {
  if (path.extname(file).toLowerCase() === '.wasm') {
    await run('node', ['--enable-source-maps', execPath, file, ...args], { stdio: 'inherit' })
    return
  }
  const dir = await mkdtemp(path.join(tmpdir(), 'raven-exec-'))
  const base = path.basename(file, path.extname(file))
  const output = path.join(dir, `${base}.wasm`)
  try {
    [, file] = await compile(file, { ...config, output })
    await run('node', ['--enable-source-maps', execPath, file, ...args], { stdio: 'inherit' })
  } finally {
    await rm(dir, { recursive: true, force: true })
  }
}
