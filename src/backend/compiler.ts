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
    const gs = assigned_globals(ir)
    for (const [b, T] of gs) this.sources.set(b, T)
    if (opcount(ir) <= 0) return
    let wir = this.wasm.lower(ir)
    if (gs.size > 0) reset(this)
    wir = wasm.lowerwasm_globals(wir, this.wasm.globals)
    const name = this.wasm.names.get([m])
    const fn = irfunc(name, wir)
    em.emit(this.wasm, fn)
  }

  loadcommon(emitter: wasm.Emitter, load: Loader): this {
    const emit = (m: mods.Method) => {
      reset(this)
      this.emit(m, emitter)
    }
    withEmit(emit, () => {
      this.sources.module(core())
      // TODO toplevel exprs should compile in the context of the relevant module,
      // rather than main. Which would make the following unnecessary.
      this.sources.module(tag('')).import(this.sources.module(tag('common')))
      loadmodule(this.sources, tag('common.core'), 'core.rv', load)
      loadmodule(this.sources, tag('common'), 'common.rv', load)
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

  constructor(readonly load: Loader, src?: string | SourceString) {
    this.pipe = new Pipeline()
    this.emitter = new wasm.BatchEmitter()
    this.loadcommon()
    if (src) this.reload(src)
  }

  loadcommon(): this {
    const [, t] = withtime(() => {
      this.pipe.loadcommon(this.emitter, this.load)
    })
    this.time += t
    return this
  }

  reload(src: string | SourceString): wasm.Emitter {
    if (!(this.emitter instanceof wasm.BatchEmitter)) throw new Error('nope')
    const em = this.emitter.clone()
    const [, t] = withtime(() => {
      const emitIR = (m: mods.Method) => {
        reset(this.pipe)
        this.pipe.emit(m, em)
      }
      withEmit(emitIR, () => { reload(this.pipe.sources, src, this.load) })
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
