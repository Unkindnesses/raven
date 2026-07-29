import * as types from '../frontend/types.js'
import { tag } from '../frontend/types.js'
import * as mods from '../frontend/modules.js'
import { Traced } from '../middle/tracer.js'
import { MatchMethods } from '../middle/patterns.js'
import { Inferred, Sig } from '../middle/abstract.js'
import { Expanded } from '../middle/expand.js'
import { Inlined, opcount } from '../middle/inline.js'
import { isreftype, release_method, refcounts } from '../middle/refcount.js'
import * as wasm from './wasm.js'
import { reset, reuse, pipe, Caching, withtime } from '../utils/cache.js'
import { loadmodule, reload, SourceString } from '../middle/load.js'
import { Loader } from '../frontend/packages.js'
import { binding, options } from '../utils/options.js'
import { Lowered, assigned_globals } from '../frontend/lower.js'
import { core, invoke_method } from '../middle/primitives.js'
import { only } from '../utils/map.js'
import { Def } from '../dwarf/index.js'

export { Pipeline, Compiler, emit, withEmit }

class Pipeline implements Caching {
  readonly sources: mods.Modules
  readonly defs: mods.Definitions
  readonly lowered: Lowered
  readonly methods: MatchMethods
  readonly interp: Traced
  readonly inferred: Inferred
  readonly expanded: ReturnType<typeof Expanded>
  readonly inlined: ReturnType<typeof Inlined>
  readonly counted: ReturnType<typeof refcounts>
  readonly wasm: wasm.Wasm

  constructor(sources = new mods.Modules()) {
    this.sources = sources
    this.defs = new mods.Definitions(this.sources)
    this.lowered = new Lowered(this.sources)
    this.methods = new MatchMethods(this.defs, this.lowered)
    this.interp = Traced.create(this.defs, this.lowered, this.methods)
    this.inferred = new Inferred(this.defs, this.lowered, this.methods, this.interp)
    this.expanded = Expanded(this.inferred)
    this.inlined = Inlined(this.expanded)
    this.counted = refcounts(this.inlined)
    this.wasm = new wasm.Wasm(this.defs, this.counted)
  }

  get subcaches(): Caching[] {
    return [this.sources, this.defs, this.lowered, this.methods, this.interp, this.inferred, this.expanded, this.inlined, this.counted, this.wasm]
  }

  fork(): Pipeline {
    return reuse(new Pipeline(this.sources.clone()), this)
  }

  reset(deps: Set<bigint>): void { reset(pipe(...this.subcaches), deps) }

  // TODO less backend-dependent
  emit(em: wasm.Emitter, m: mods.Method): void {
    const ir = this.counted.get([m])
    const name = this.wasm.names.get([m])
    const gs = assigned_globals(ir)
    let wir = this.wasm.lower(ir)
    const fns = wasm.calltree(this.wasm, wasm.lowerfunc(name, wir))
    for (const [b, T] of gs) this.sources.set(b, T)
    if (gs.size > 0) reset(this)
    if (opcount(ir) <= 0) return
    if (em instanceof wasm.BatchEmitter) this.destructors(gs, em)
    wir = wasm.lowerwasm_globals(wir, this.wasm.globals)
    em.emit(fns, wasm.lowerfunc(name, wir))
  }

  export(em: wasm.Emitter, sig: Sig, as?: string): string {
    const func = this.wasm.get(sig)
    em.export(wasm.calltree(this.wasm, func), func, as)
    return func.name
  }

  private destructors(gs: Map<mods.Binding, types.Type>, em: wasm.BatchEmitter) {
    for (const [b, T] of gs) {
      if (!isreftype(T)) continue
      const ids = this.wasm.globals.get(b)
      const fname = `__release_global.${ids[0]}`
      const code = mods.MIR(Def(fname))
      const value = code.push(code.stmt(mods.xglobal(b), { type: T }))
      code.return(code.push(code.stmt(new mods.Invoke(release_method, [value]), { type: types.nil })))
      const wir = this.wasm.lower(code)
      const func = wasm.lowerfunc(fname, wir)
      const calls = wasm.calltree(this.wasm, func)
      em.destructor(calls, func)
    }
  }

  async loadcommon(emitter: wasm.Emitter, load: Loader): Promise<this> {
    const emit = (m: mods.Method) => {
      reset(this)
      this.emit(emitter, m)
    }
    await withEmit(emit, async () => {
      this.sources.module(core())
      await loadmodule(this.sources, load, load.entry('common'))
    })
    reset(this)
    return this
  }
}

const [withEmit, getEmit] = binding<(m: mods.Method) => void>('emit', _ => { })
function emit(m: mods.Method): void { return getEmit()(m) }

class Compiler {
  readonly pipe: Pipeline
  readonly emitter: wasm.BatchEmitter
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

  async reload(src: string | SourceString): Promise<wasm.BatchEmitter> {
    const em = this.emitter.clone()
    const [, t] = await withtime(async () => {
      const emitIR = (m: mods.Method) => {
        reset(this.pipe)
        this.pipe.emit(em, m)
      }
      await withEmit(emitIR, async () => { await reload(this.pipe.sources, src, this.load) })
      reset(this.pipe)
      if (options().memcheck && em.funcs.some(fn => fn.name.startsWith('common.malloc!'))) {
        em.main.push(...em.destructors)
        const checks = this.pipe.defs.methods(tag('common.checkAllocations'))
        this.pipe.emit(em, only(checks))
      }
      if (em.imports.some(imp => imp.mod === 'support' && imp.name === 'async'))
        this.pipe.export(em, [invoke_method, types.Func, types.list(), types.list(), types.list()], '__raven_async_task')
    })
    this.time += t
    return em
  }
}
