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
import { lowerpattern } from '../frontend/patterns'
import * as ast from '../frontend/ast'
import { core } from '../middle/primitives'
import * as path from 'path'
import { only } from '../utils/map'
import { mkdir } from 'fs/promises'
import { spawn, SpawnOptions } from 'node:child_process'

export { Pipeline, Compiler, emit, withEmit, compile, exec, run }

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

  emitIR(ir: mods.MIR, em: wasm.Emitter): void {
    this.emit(new mods.Method(tag('common.core.main'), lowerpattern(ast.List()), ir), em)
  }

  loadcommon(emitter: wasm.Emitter): this {
    const emit = (ir: mods.MIR) => {
      reset(this)
      this.emitIR(ir, emitter)
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

const [withEmit, getEmit] = binding<(ir: mods.MIR) => void>('emit', _ => { })
function emit(ir: mods.MIR): void { return getEmit()(ir) }

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
    const emitIR = (ir: mods.MIR) => {
      reset(this.pipe)
      this.pipe.emitIR(ir, em)
    }
    withEmit(emitIR, () => { reload(this.pipe.sources, src) })
    reset(this.pipe)
    if (options().memcheck) {
      const checks = this.pipe.defs.methods.get(tag('common.checkAllocations'))
      this.pipe.emit(only(checks), em)
    }
    return em
  }
}

interface CompileConfig {
  dir?: string
  compiler?: Compiler
  options?: Partial<Options>
}

async function compile(file: string, config: CompileConfig = {}): Promise<string> {
  let { dir = path.dirname(file), compiler = new Compiler(), options = {} } = config
  const base = path.basename(file, path.extname(file))
  await mkdir(dir, { recursive: true })
  await withOptions(options, async () => {
    const em = compiler.reload(file)
    if (!(em instanceof wasm.BatchEmitter)) throw new Error('nope')
    await wasm.emitwasm(em, compiler.pipe.wasm, path.join(dir, `${base}.wasm`))
    await wasm.emitjs(path.join(dir, `${base}.js`), `${base}.wasm`)
  })
  return path.join(dir, `${base}.js`)
}

async function run(cmd: string, args: readonly string[] = [], options: SpawnOptions = {}) {
  return await new Promise<[number | null, NodeJS.Signals | null]>((resolve, reject) => {
    const child = spawn(cmd, [...args], options)
    child.on('error', reject)
    child.on('close', (code, signal) => resolve([code, signal]))
  })
}

async function exec(file: string, args: string[] = [], config?: CompileConfig): Promise<void> {
  const js = await compile(file, config)
  await run('node', ['--experimental-wasm-stack-switching', js, ...args], { stdio: 'inherit' })
}
