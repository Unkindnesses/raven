import { tag } from '../frontend/types'
import * as mods from '../frontend/modules'
import { Interpreter, interpreter } from '../middle/interpret'
import { MatchMethods } from '../middle/patterns'
import { Inferred, Redirect } from '../middle/abstract'
import { Expanded } from '../middle/expand'
import { Inlined, opcount } from '../middle/inline'
import { refcounts } from '../middle/refcount'
import * as wasm from './wasm'
import { irfunc } from '../wasm/ir'
import { reset, pipe, type Caching } from '../utils/cache'
import { loadmodule, reload, common, SourceString } from '../middle/load'
import { binding, Options, options, withOptions } from '../utils/options'
import { assigned_globals } from '../frontend/lower'
import { core } from '../middle/primitives'
import * as path from 'path'
import { only } from '../utils/map'
import { chmod, mkdir, readFile, writeFile } from 'fs/promises'
import { spawn, SpawnOptions } from 'node:child_process'
import { dirname } from '../dirname'

export { Pipeline, Compiler, emit, withEmit, compile, compileJS, exec, run }

class Pipeline implements Caching {
  readonly sources: mods.Modules
  readonly defs: mods.Definitions
  readonly interp: Interpreter
  readonly methods: MatchMethods
  readonly inferred: Inferred
  readonly expanded: ReturnType<typeof Expanded>
  readonly inlined: ReturnType<typeof Inlined>
  readonly counted: ReturnType<typeof refcounts>
  readonly wasm: wasm.Wasm

  constructor() {
    this.sources = new mods.Modules()
    this.defs = new mods.Definitions(this.sources)
    this.interp = interpreter(this.defs)
    this.methods = MatchMethods(this.defs, this.interp)
    this.inferred = new Inferred(this.defs, this.methods)
    this.expanded = Expanded(this.inferred)
    this.inlined = Inlined(this.expanded)
    this.counted = refcounts(this.inlined)
    this.wasm = new wasm.Wasm(this.defs, this.counted)
  }

  get subcaches(): Caching[] {
    return [this.sources, this.defs, this.methods, this.inferred, this.expanded, this.inlined, this.counted, this.wasm]
  }

  reset(deps: Set<bigint>): void { reset(pipe(...this.subcaches), deps) }

  // TODO less backend-dependent
  emit(m: mods.Method, em: wasm.Emitter): void {
    let ir = this.counted.get([m])
    if (ir instanceof Redirect) throw new Error('nope')
    const gs = assigned_globals(ir)
    for (const [b, T] of gs) this.sources.set(b, T)
    if (opcount(ir) <= 0) return
    ir = this.wasm.lower(ir)
    if (gs.size > 0) reset(this)
    ir = wasm.lowerwasm_globals(ir, this.wasm.globals)
    const name = this.wasm.names.get([m])
    const fn = irfunc(name, ir)
    em.emit(this.wasm, fn)
  }

  loadcommon(emitter: wasm.Emitter): this {
    const emit = (m: mods.Method) => {
      reset(this)
      this.emit(m, emitter)
    }
    withEmit(emit, () => {
      this.sources.module(core())
      // TODO toplevel exprs should compile in the context of the relevant module,
      // rather than main. Which would make the following unnecessary.
      this.sources.module(tag('')).import(this.sources.module(tag('common')))
      loadmodule(this.sources, tag('common.core'), path.join(common, 'core.rv'))
      loadmodule(this.sources, tag('common'), path.join(common, 'common.rv'))
    })
    return this
  }
}

const [withEmit, getEmit] = binding<(m: mods.Method) => void>('emit', _ => { })
function emit(m: mods.Method): void { return getEmit()(m) }

class Compiler {
  readonly pipe: Pipeline
  readonly emitter: wasm.Emitter

  constructor(src?: string | SourceString) {
    this.pipe = new Pipeline()
    this.emitter = new wasm.BatchEmitter()
    this.loadcommon()
    if (src) this.reload(src)
  }

  loadcommon(): this {
    this.pipe.loadcommon(this.emitter)
    return this
  }

  reload(src: string | SourceString): wasm.Emitter {
    if (!(this.emitter instanceof wasm.BatchEmitter)) throw new Error('nope')
    const em = this.emitter.clone()
    const emitIR = (m: mods.Method) => {
      reset(this.pipe)
      this.pipe.emit(m, em)
    }
    withEmit(emitIR, () => { reload(this.pipe.sources, src) })
    reset(this.pipe)
    if (options().memcheck) {
      const checks = this.pipe.defs.methods(tag('common.checkAllocations'))
      this.pipe.emit(only(checks), em)
    }
    return em
  }
}

interface CompileConfig {
  dir?: string
  compiler?: Compiler
  options?: Partial<Options>
  output?: string
  strip?: boolean
}

async function compile(file: string, config: CompileConfig = {}): Promise<string> {
  let { dir = path.dirname(file), compiler = new Compiler(), options = {}, output, strip = false } = config
  const base = path.basename(file, path.extname(file))
  const wasmPath = output ?? path.join(dir, `${base}.wasm`)
  await mkdir(path.dirname(wasmPath), { recursive: true })
  await withOptions(options, async () => {
    const em = compiler.reload(file)
    if (!(em instanceof wasm.BatchEmitter)) throw new Error('nope')
    await wasm.emitwasm(em, compiler.pipe.wasm, wasmPath, strip)
  })
  return wasmPath
}

async function compileJS(file: string, config: CompileConfig = {}): Promise<string> {
  let { dir = path.dirname(file), compiler = new Compiler(), options = {}, output, strip = false } = config
  const base = path.basename(file, path.extname(file))
  const jsPath = output ?? path.join(dir, `${base}.js`)
  await mkdir(path.dirname(jsPath), { recursive: true })
  await withOptions(options, async () => {
    const em = compiler.reload(file)
    if (!(em instanceof wasm.BatchEmitter)) throw new Error('nope')
    const bytes = wasm.emitwasmBinary(em, compiler.pipe.wasm, strip)
    const base64 = Buffer.from(bytes).toString('base64')
    const runtime = await readFile(execPath, 'utf8')
    await writeFile(jsPath, `${runtime}\nbinary = Buffer.from('${base64}', 'base64')\n`)
    await chmod(jsPath, 0o755)
  })
  return jsPath
}

async function run(cmd: string, args: readonly string[] = [], options: SpawnOptions = {}) {
  return await new Promise<[number | null, NodeJS.Signals | null]>((resolve, reject) => {
    const child = spawn(cmd, [...args], options)
    child.on('error', reject)
    child.on('close', (code, signal) => resolve([code, signal]))
  })
}

const execPath = path.join(dirname, '../build/backend/exec.js')

async function exec(file: string, args: string[] = [], config?: CompileConfig): Promise<void> {
  if (path.extname(file).toLowerCase() !== '.wasm') file = await compile(file, config)
  await run('node', ['--enable-source-maps', '--experimental-wasm-stack-switching', execPath, file, ...args], { stdio: 'inherit' })
}
