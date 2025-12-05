import { tag } from '../frontend/types.js'
import * as mods from '../frontend/modules.js'
import { Interpreter, interpreter } from '../middle/interpret.js'
import { MatchMethods } from '../middle/patterns.js'
import { Inferred, Redirect } from '../middle/abstract.js'
import { Expanded } from '../middle/expand.js'
import { Inlined, opcount } from '../middle/inline.js'
import { refcounts } from '../middle/refcount.js'
import * as wasm from './wasm.js'
import { irfunc } from '../wasm/ir.js'
import { reset, pipe, Caching, withtime } from '../utils/cache.js'
import { Loader, loadmodule, reload, SourceString } from '../middle/load.js'
import { binding, options } from '../utils/options.js'
import { assigned_globals } from '../frontend/lower.js'
import { core } from '../middle/primitives.js'
import { only } from '../utils/map.js'

export { Pipeline, Compiler, emit, withEmit }

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
    const name = this.wasm.names.get([m])
    const gs = assigned_globals(ir)
    let wir = this.wasm.lower(ir)
    const fns = wasm.calltree(this.wasm, irfunc(name, wir))
    for (const [b, T] of gs) this.sources.set(b, T)
    if (gs.size > 0) reset(this)
    if (opcount(ir) <= 0) return
    wir = wasm.lowerwasm_globals(wir, this.wasm.globals)
    em.emit(fns, irfunc(name, wir))
  }

  async loadcommon(emitter: wasm.Emitter, load: Loader): Promise<this> {
    const emit = (m: mods.Method) => {
      reset(this)
      this.emit(m, emitter)
    }
    await withEmit(emit, async () => {
      this.sources.module(core())
      // TODO toplevel exprs should compile in the context of the relevant module,
      // rather than main. Which would make the following unnecessary.
      this.sources.module(tag('')).import(this.sources.module(tag('common')))
      await loadmodule(this.sources, tag('common.core'), 'core.rv', load)
      await loadmodule(this.sources, tag('common'), 'common.rv', load)
    })
    return this
  }
}

const [withEmit, getEmit] = binding<(m: mods.Method) => void>('emit', _ => { })
function emit(m: mods.Method): void { return getEmit()(m) }

class Compiler {
  readonly pipe: Pipeline
  readonly emitter: wasm.Emitter
  time = 0n

  private constructor(readonly load: Loader) {
    this.pipe = new Pipeline()
    this.emitter = new wasm.BatchEmitter(this.pipe.wasm.tables)
  }

  static async create(load: Loader, src?: string | SourceString): Promise<Compiler> {
    const compiler = new Compiler(load)
    await compiler.loadcommon()
    if (src) await compiler.reload(src)
    return compiler
  }

  async loadcommon(): Promise<this> {
    const [, t] = await withtime(async () => {
      await this.pipe.loadcommon(this.emitter, this.load)
    })
    this.time += t
    return this
  }

  async reload(src: string | SourceString): Promise<wasm.Emitter> {
    if (!(this.emitter instanceof wasm.BatchEmitter)) throw new Error('nope')
    const em = this.emitter.clone()
    const [, t] = await withtime(async () => {
      const emitIR = (m: mods.Method) => {
        reset(this.pipe)
        this.pipe.emit(m, em)
      }
      await withEmit(emitIR, async () => { await reload(this.pipe.sources, src, this.load) })
      reset(this.pipe)
      if (options().memcheck && em.funcs.some(fn => fn.name.startsWith('common.malloc!'))) {
        const checks = this.pipe.defs.methods(tag('common.checkAllocations'))
        this.pipe.emit(only(checks), em)
      }
    })
    this.time += t
    return em
  }
}
