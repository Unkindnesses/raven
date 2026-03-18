import * as types from '../frontend/types.js'
import { Type } from '../frontend/types.js'
import * as wasm from '../wasm/wasm.js'
import { binary } from '../wasm/binary.js'
import { irfunc, Instr, setdiff, Value as WValue, WIR, xref } from '../wasm/ir.js'
import { unreachable, Anno, Pipe, expr, Val, Branch, Expr, asType } from '../utils/ir.js'
import { isEqual } from '../utils/isEqual.js'
import { wlayout } from '../middle/expand.js'
import { Cache, Caching, DualCache, reset as resetCaches, pipe, reuse } from '../utils/cache.js'
import { Binding, Definitions, MIR, Method, StringRef, JS, Func, Global, SetGlobal, Wasm as WasmCall, callargs, Value as MValue, asValue } from '../frontend/modules.js'
import { Def } from '../dwarf/index.js'
import { Redirect, Sig } from '../middle/abstract.js'
import { Accessor } from '../utils/fixpoint.js'
import { xtuple } from '../frontend/lower.js'
import { asArray, some } from '../utils/map.js'

export { wasmPartials, Wasm, BatchEmitter, StreamEmitter, Emitter, emitwasm, lowerwasm, lowerwasm_globals, calltree }

type PartialFn = (...args: Type[]) => Type

// WASM partial primitives
// These are supposed to be defined in Raven, but we don't yet have a mechanism
// for const prop, so this is a stopgap.

const wasmPartials = new Map<string, PartialFn>()

function bin(kind: 'float32' | 'float64', op: (a: number, b: number) => number): PartialFn {
  return (a, b) => {
    if (!(a.kind === kind && b.kind === kind)) throw new Error(`Expected ${kind}`)
    return kind === 'float32'
      ? types.float32(op(some(a.value), some(b.value)))
      : types.float64(op(some(a.value), some(b.value)))
  }
}

wasmPartials.set('f32.add', bin('float32', (a, b) => a + b))
wasmPartials.set('f64.add', bin('float64', (a, b) => a + b))
wasmPartials.set('f64.mul', bin('float64', (a, b) => a * b))

function wparts(T: Anno<Type>): wasm.ValueType[] {
  return T === unreachable ? [] : wlayout(T)
}

function tableid<T>(xs: T[], x: T): number {
  const i = xs.findIndex(y => isEqual(y, x))
  if (i !== -1) return i
  xs.push(x)
  return xs.length - 1
}

interface JSInline {
  code: string
  params: string[]
}

class Tables {
  globals: [string, wasm.ValueType][] = []
  strings: string[] = []
  funcs: string[] = []
  js: JSInline[] = []
  string(s: string): string {
    tableid(this.strings, s)
    return s
  }
  func(f: string): number { return tableid(this.funcs, f) }
  jsinline(code: string, params: string[]): number {
    return tableid(this.js, { code, params })
  }
}

function instr<T>(instr: wasm.Instruction, ...args: (T | number)[]): Instr<T> {
  return new Instr(instr, args)
}

function lowerconst(x: MValue): WValue {
  const T = x.type
  if (T.kind === 'bits') {
    if (T.size <= 32) return WValue.i32(T.value)
    if (T.size <= 64) return WValue.i64(T.value)
    throw new Error(`Unsupported bit width ${T.size}`)
  }
  if (T.kind === 'float32') return WValue.f32(T.value)
  if (T.kind === 'float64') return WValue.f64(T.value)
  throw new Error(`Expected bits/float constant, got ${types.repr(T)}`)
}

function lowerwasm(ir: MIR, names: DualCache<Sig | WSig, string>, globals: Cache<Binding, string[]>, tables: Tables): WIR {
  const out = WIR(ir.meta)
  const env = new Map<number, Val<WIR>>()
  // TODO deprecate array types
  const type = (t: Anno<Type>): Anno<wasm.ValueType[]> => t === unreachable ? [] : wlayout(t)
  const rename = (x: Val<MIR>) => typeof x === 'number' ? some(env.get(x)) : lowerconst(asValue(x))
  const coerce = (x: Val<MIR>) =>
    typeof x === 'number' || x instanceof MValue ? rename(x) :
      out.push(out.stmt(xtuple(), { type: [] })) // TODO just filter these out – or empty Const?
  for (const block of ir.blocks()) {
    if (block.id !== 0) out.newBlock()
    const ob = out.block()
    for (let i = 0; i < block.args.length; i++) {
      const arg = block.args[i]
      const type = wlayout(asType(block.argtypes[i]))
      env.set(arg, ob.argument(type))
    }
    for (const [v, st] of block) {
      if (st.expr instanceof StringRef) {
        env.set(v, out.push({ ...st, expr: instr(wasm.GetGlobal(tables.string(st.expr.value))), type: [wasm.externref] }))
      } else if (st.expr instanceof JS) {
        const id = tables.jsinline(st.expr.code, st.expr.params)
        const argTypes = st.expr.params.map(() => wasm.externref)
        const fname = names.get([['inline', `js_${id}`], argTypes, [wasm.externref]])
        const args = st.expr.body.map(rename)
        env.set(v, out.push({ ...st, expr: instr(wasm.Call(fname), ...args), type: [wasm.externref] }))
      } else if (st.expr instanceof Func) {
        const sig = [st.expr.method, ...st.expr.body.map(x => asType(ir.type(x)))] as Sig
        const name = names.get(sig)
        env.set(v, out.push({ ...st, expr: xtuple(WValue.i32(tables.func(name))), type: type(st.type) }))
      } else if (['tuple', 'ref'].includes(st.expr.head)) {
        env.set(v, out.push({ ...st, expr: st.expr.map(rename as any) as unknown as Expr<WValue>, type: type(st.type) }))
      } else if (st.expr.head === 'cast') { // TODO just use `tuple` instead
        const arg = st.expr.body[0]
        if (!isEqual(wlayout(asType(st.type)), wlayout(asType(ir.type(arg)))))
          throw new Error('cast: layout mismatch')
        env.set(v, rename(arg))
      } else if (st.expr instanceof Global) {
        const ids = globals.get(st.expr.binding)
        const parts = wlayout(asType(st.type))
        const ps: Val<WIR>[] = []
        for (let i = 0; i < ids.length; i++)
          ps.push(out.push(out.stmt(instr(wasm.GetGlobal(ids[i])), { type: [parts[i]] })))
        if (ps.length === 1) env.set(v, ps[0])
        else env.set(v, out.push({ ...st, expr: xtuple(...ps), type: parts }))
      } else if (st.expr instanceof WasmCall) {
        const [callee, args] = [st.expr.callee, st.expr.body]
        let expr: Expr<WValue>
        if (Array.isArray(callee)) {
          const I = args.flatMap(a => wlayout(asType(ir.type(a))))
          const O = st.expr.result ?? wparts(st.type)
          const name = names.get([callee, I, O])
          expr = instr(wasm.Call(name), ...args.map(rename))
        } else {
          expr = instr(callee, ...args.map(rename))
        }
        env.set(v, out.push({ ...st, expr: expr, type: type(st.type) }))
        if (st.type === unreachable) out.push(out.stmt(instr(wasm.unreachable), { type: [] })) // TODO unnecessary?
      } else if (['call', 'invoke'].includes(st.expr.head)) {
        let [F, args] = callargs(ir, st.expr)
        const Ts = args.map(a => asType(ir.type(a)))
        const sig = [F, ...Ts] as Sig
        args = args.filter(x => !types.isValue(asType(ir.type(x))))
        const expr = instr(wasm.Call(names.get(sig)), ...args.map(rename))
        env.set(v, out.push({ ...st, expr: expr, type: wparts(st.type) }))
      } else if (st.expr.head === 'call_indirect') {
        const [id, ...args] = st.expr.body
        const I = args.flatMap(x => wlayout(asType(ir.type(x))))
        const O = wlayout(asType(st.type))
        env.set(v, out.push({ ...st, expr: instr(wasm.CallIndirect(wasm.Signature(I, O), 'funcs'), ...args.map(rename), rename(id)), type: O }))
      } else if (st.expr instanceof Branch) {
        const expr = st.expr.map(coerce)
        env.set(v, out.push({ ...st, expr, type: unreachable }))
      } else if (st.expr.head === 'setglobal') {
        const expr = st.expr.map(coerce as any) as unknown as Expr<WValue>
        env.set(v, out.push({ ...st, expr, type: type(st.type) }))
      } else throw new Error(`unrecognised ${st.expr.head} expression`)
    }
  }
  return out
}

function lowerwasm_globals(ir: WIR, globals: Cache<Binding, string[]>): WIR {
  const pr = new Pipe(ir)
  for (const [v, st] of pr) {
    if (!(st.expr instanceof SetGlobal)) continue
    pr.delete(v)
    const ids = globals.get(st.expr.binding)
    for (let i = 0; i < ids.length; i++) {
      const p = pr.push(pr.stmt(xref(st.expr.value, i + 1)))
      pr.push(pr.stmt(instr(wasm.SetGlobal(ids[i]), p), { type: [] }))
    }
  }
  return pr.finish()
}

function frame(code: Accessor<Sig, Redirect | MIR>, sig: Sig): MIR {
  let res = code.get(sig)
  while (res instanceof Redirect) res = code.get(res.to)
  return res
}

type WSig = [[string, string], wasm.ValueType[], wasm.ValueType[]]

function wname(f: types.Tag | Method | [string, string]): string {
  if (f instanceof types.Tag) return f.path
  if (f instanceof Method) return `${f.name.path}:method`
  if (Array.isArray(f)) return `${f[0]}:${f[1]}`
  throw new Error('unreachable')
}

class Wasm implements Caching {
  tables: Tables
  count: Map<string, number>
  globals: Cache<Binding, string[]>
  names: DualCache<Sig | WSig, string>
  funcs: Cache<Sig, wasm.Func>
  constructor(defs: Definitions, code: Accessor<Sig, Redirect | MIR>) {
    this.tables = new Tables()
    this.count = new Map<string, number>()
    // TODO pretty sure this is wrong; binding chains lead to duplicated globals
    this.globals = new Cache<Binding, string[]>(b => {
      let T: Anno<Type> | Binding = b
      while (T instanceof Binding) T = defs.global(T)
      const l = wparts(T)
      const names: string[] = []
      for (let i = 0; i < l.length; i++) {
        const name = `${b.mod.path}.${b.name}:${this.tables.globals.length}`
        this.tables.globals.push([name, l[i]])
        names.push(name)
      }
      return names
    })
    // TODO should be `funcs`, not `code`, to make global redefs of the same type
    // more efficient. But that creates an awkward cycle between names and funcs.
    this.names = new DualCache<Sig | WSig, string>(sig => {
      if (!Array.isArray(sig[0])) code.get(sig as Sig) // new name if code changes
      const id = wname(sig[0])
      const c = (this.count.get(id) ?? 0) + 1
      this.count.set(id, c)
      return `${id}:${c}`
    })
    this.funcs = new Cache<Sig, wasm.Func>(sig => {
      // TODO: we use `frame` to avoid redirects, but this can duplicate function
      // bodies. Should instead avoid calling redirected sigs, eg via casting.
      return irfunc(this.names.get(sig), this.lower(frame(code, sig)))
    })
  }
  lower(ir: MIR) { return lowerwasm(ir, this.names, this.globals, this.tables) }
  get(sig: Sig): wasm.Func { return this.funcs.get(sig) }

  reuse(ch: this) {
    this.tables = ch.tables
    this.count = ch.count
    reuse(pipe(this.globals, this.names, this.funcs), pipe(ch.globals, ch.names, ch.funcs))
    return this
  }

  get subcaches() { return [this.globals, this.names, this.funcs] }
  reset(deps: Set<bigint>) { resetCaches(pipe(this.globals, this.names, this.funcs), deps) }
}

function calltree(mod: Wasm, root: wasm.Func): Map<string, wasm.Func | WSig> {
  const calls = new Map<string, wasm.Func | WSig>()
  const visit = (f: string) => {
    if (calls.has(f)) return
    const sig = mod.names.getkey(f)
    if (Array.isArray(sig[0])) return calls.set(f, sig as WSig)
    const func = mod.get(sig as Sig)
    calls.set(f, func)
    for (const g of wasm.callees(func)) visit(g)
  }
  for (const f of wasm.callees(root)) visit(f)
  // TODO the guard is a bit hacky, but old funcs can be invalidated.
  // Ideally we'd know which funcs are new/used in the tree.
  for (const f of mod.tables.funcs) if (mod.names.hasvalue(f)) visit(f)
  return calls
}

// Batch emitter, for AOT compilation

type Emitter = { emit(calls: Map<string, wasm.Func | WSig>, func: wasm.Func): void }

class BatchEmitter implements Emitter {
  tables: Tables
  main: string[]
  destructors: string[]
  seen: Set<string>
  funcs: wasm.Func[]
  imports: wasm.Import[]
  exports: wasm.Export[]
  constructor(tables: Tables) {
    this.tables = tables
    this.main = []
    this.destructors = []
    this.seen = new Set()
    this.funcs = []
    this.imports = []
    this.exports = []
  }

  clone(): BatchEmitter {
    const em = new BatchEmitter(this.tables)
    em.main = [...this.main]
    em.destructors = [...this.destructors]
    em.seen = new Set(this.seen)
    em.funcs = [...this.funcs]
    em.imports = [...this.imports]
    em.exports = [...this.exports]
    return em
  }

  private emitFunc(calls: Map<string, wasm.Func | WSig>, func: wasm.Func) {
    this.funcs.push(func)
    for (const f of wasm.callees(func)) this.emitName(calls, f)
  }

  private emitName(calls: Map<string, wasm.Func | WSig>, f: string) {
    if (this.seen.has(f)) return
    this.seen.add(f)
    const fn = some(calls.get(f))
    if (Array.isArray(fn)) {
      const [imp, I, O] = fn
      this.imports.push(wasm.Import(...imp, wasm.Signature(I, O, f)))
    } else this.emitFunc(calls, fn)
  }

  emit(calls: Map<string, wasm.Func | WSig>, func: wasm.Func) {
    this.emitFunc(calls, func)
    // TODO emit only new funcs
    for (const f of this.tables.funcs) if (calls.get(f)) this.emitName(calls, f)
    this.main.push(func.name)
  }

  destructor(calls: Map<string, wasm.Func | WSig>, func: wasm.Func) {
    this.emitFunc(calls, func)
    // TODO emit only new funcs
    for (const f of this.tables.funcs) if (calls.get(f)) this.emitName(calls, f)
    this.destructors.push(func.name)
  }

  export(name: string, as = name) {
    const existing = this.exports.some(ex => ex.name === name && ex.as === as)
    if (!existing) this.exports.push(wasm.Export(name, as))
  }
}

const refTable = 'jsrefs'
const refCounter = 'jsrefs.next'
const refGlobals: [string, wasm.ValueType][] = [
  [refCounter, wasm.i32],
  ['allocs', wasm.i32],
  ['frees', wasm.i32]
]

function startfunc(main: string[]): wasm.Func {
  const meta = Def('_start')
  const instrs = [...main.map(m => wasm.Call(m)), wasm.Const(wasm.NumType.i32, 0)]
  const body = wasm.Block(instrs, instrs.map(() => wasm.LineInfo([[meta, meta.source]])))
  return wasm.Func('_start', wasm.Signature([], [wasm.NumType.i32]), [], body, meta)
}

function stringImports(strings: string[]): wasm.Import[] {
  // TODO names from table
  return strings.map(value =>
    wasm.Import('strings', value, wasm.Global(value, wasm.externref, { mut: false })))
}

function moduleTables(tables: Tables): wasm.Table[] {
  return [
    wasm.Table('funcs', wasm.funcref, tables.funcs.length),
    wasm.Table(refTable, wasm.externref, 0)
  ]
}

function metaSection(tables: Tables): wasm.CustomSection[] {
  if (tables.js.length === 0) return []
  const meta = { js: tables.js }
  return [wasm.CustomSection('raven.meta', new TextEncoder().encode(JSON.stringify(meta)))]
}

function wasmmodule(em: BatchEmitter): wasm.Module {
  em.funcs.unshift(startfunc(em.main))
  const mod = wasm.Module({
    funcs: em.funcs,
    imports: [...stringImports(em.tables.strings), ...em.imports],
    exports: [
      wasm.Export('_start'),
      wasm.Export('_start', 'cm32p2|wasi:cli/run@0.2|run'),
      wasm.Export('cm32p2_memory'),
      wasm.Export(refTable),
      wasm.Export('allocs'),
      wasm.Export('frees'),
      ...em.exports
    ],
    globals: [...em.tables.globals, ...refGlobals].map(g => wasm.Global(...g)),
    tables: moduleTables(em.tables),
    elems: [wasm.Elem('funcs', em.tables.funcs)],
    mems: [wasm.Mem('cm32p2_memory', 0)],
    customs: metaSection(em.tables)
  })
  return mod
}

function emitwasm(em: BatchEmitter, strip = false): Uint8Array {
  return binary(wasmmodule(em), strip)
}

// Stream emitter, for REPL

function wimport(mod: Map<string, wasm.Func | WSig>, f: string): wasm.Import {
  const sig = some(mod.get(f))
  if (Array.isArray(sig)) {
    const [imp, I, O] = sig
    return wasm.Import(...imp, wasm.Signature(I, O, f))
  } else {
    return wasm.Import('wasm', f, { ...sig.sig, name: f })
  }
}

class StreamEmitter implements Emitter {
  tables: Tables
  seen: Set<string>
  queue: wasm.Module[]
  globals: number
  constructor(tables: Tables) {
    this.tables = tables
    this.seen = new Set()
    this.queue = []
    this.globals = -1
  }

  private emitFunc(calls: Map<string, wasm.Func | WSig>, func: wasm.Func, fs: wasm.Func[], imports: string[]) {
    fs.push(func)
    for (const f of wasm.callees(func)) this.emitName(calls, f, fs, imports)
  }

  private emitName(calls: Map<string, wasm.Func | WSig>, f: string, fs: wasm.Func[], imports: string[]) {
    imports.push(f)
    if (this.seen.has(f)) return
    this.seen.add(f)
    const fn = some(calls.get(f))
    if (!Array.isArray(fn)) this.emitFunc(calls, fn, fs, imports)
  }

  emit(calls: Map<string, wasm.Func | WSig>, func: wasm.Func) {
    const first = this.globals === -1
    if (first) this.globals = 0
    const fs: wasm.Func[] = []
    const imports: string[] = []
    this.emitFunc(calls, func, fs, imports)
    for (const f of this.tables.funcs) this.emitName(calls, f, fs, imports)
    fs.unshift(startfunc([func.name]))
    const iimports = setdiff(imports, fs.map(f => f.name)).map(f => wimport(calls, f))
    const gimports: wasm.Import[] = []
    const globalTypes = [...refGlobals, ...this.tables.globals]
    for (let i = 1; i <= this.globals; i++) {
      const [name, type] = globalTypes[i - 1]
      gimports.push(wasm.Import('wasm', name, wasm.Global(name, type)))
    }
    const globals: wasm.Global[] = []
    for (let i = this.globals + 1; i <= globalTypes.length; i++)
      globals.push(wasm.Global(...globalTypes[i - 1]))
    if (!first) {
      iimports.push(wasm.Import('wasm', 'memory', wasm.Mem('memory', 0)))
      for (const t of moduleTables(this.tables))
        iimports.push(wasm.Import('wasm', t.name, t))
    }
    const wmod = wasm.Module({
      funcs: fs,
      imports: [...stringImports(this.tables.strings), ...gimports, ...iimports],
      exports: [
        wasm.Export('memory'),
        ...moduleTables(this.tables).map(x => wasm.Export(x.name)),
        ...fs.map(f => wasm.Export(f.name, f.name)),
        ...globals.map(g => wasm.Export(g.name, g.name))],
      globals,
      tables: first ? moduleTables(this.tables) : [],
      elems: [wasm.Elem('funcs', Array.from(this.tables.funcs))],
      mems: first ? [wasm.Mem('memory', 0)] : [],
      customs: metaSection(this.tables)
    })
    this.queue.push(wmod)
    this.globals = globalTypes.length
  }
}
