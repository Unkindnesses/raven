import * as types from '../frontend/types.js'
import { Type, Bits, asBits, bits } from '../frontend/types.js'
import * as wasm from '../wasm/wasm.js'
import { binary } from '../wasm/binary.js'
import { irfunc, Instr, setdiff, Value, WIR, asValue } from '../wasm/ir.js'
import { unreachable, Anno, Pipe, expr, Val, Branch, Expr, asType } from '../utils/ir.js'
import { isEqual } from '../utils/isEqual.js'
import { wlayout } from '../middle/expand.js'
import { Cache, Caching, DualCache, reset as resetCaches, pipe } from '../utils/cache.js'
import { Binding, Definitions, MIR, Method, StringRef, JS, Global, SetGlobal, Wasm as WasmCall, callargs } from '../frontend/modules.js'
import { Def } from '../dwarf/index.js'
import { Redirect, type Sig } from '../middle/abstract.js'
import { Accessor } from '../utils/fixpoint.js'
import { xtuple } from '../frontend/lower.js'
import { asArray, some } from '../utils/map.js'

export { wasmPartials, Wasm, BatchEmitter, StreamEmitter, Emitter, emitwasm, lowerwasm, lowerwasm_globals, calltree }

type PartialFn = (...args: Type[]) => Type

// WASM partial primitives
// These are supposed to be defined in Raven, but we don't yet have a mechanism
// for const prop, so this is a stopgap.

const wasmPartials = new Map<string, PartialFn>()

function bin(n: number, op: (a: bigint, b: bigint) => bigint, signed = false): PartialFn {
  return (a, b) => {
    const [x, y] = [asBits(a), asBits(b)]
    if (x.size !== n || y.size !== n) throw new Error('Bit width mismatch')
    const conv = (x: Bits) => signed ? x.signed() : x.value
    return bits(n, op(conv(x), conv(y)))
  }
}

function cmp(n: number, op: (a: bigint, b: bigint) => boolean, signed = false): PartialFn {
  return (a, b) => {
    const [x, y] = [asBits(a), asBits(b)]
    if (x.size !== n || y.size !== n) throw new Error('Bit width mismatch')
    const conv = (x: Bits) => signed ? x.signed() : x.value
    return bits(1, op(conv(x), conv(y)))
  }
}

wasmPartials.set('i64.add', bin(64, (a, b) => a + b))
wasmPartials.set('i64.sub', bin(64, (a, b) => a - b))
wasmPartials.set('i64.mul', bin(64, (a, b) => a * b))

wasmPartials.set('i64.eq', cmp(64, (a, b) => a === b))
wasmPartials.set('i64.gt_s', cmp(64, (a, b) => a > b, true))
wasmPartials.set('i64.lt_s', cmp(64, (a, b) => a < b, true))
wasmPartials.set('i64.le_s', cmp(64, (a, b) => a <= b, true))

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

function lowerwasm(ir: MIR, names: DualCache<Sig | WSig, string>, globals: Cache<Binding, string[]>, tables: Tables): WIR {
  const out = WIR(ir.meta)
  const env = new Map<number, Val<WIR>>()
  // TODO deprecate array types
  const type = (t: Anno<Type>): Anno<wasm.ValueType[]> => t === unreachable ? [] : wlayout(t)
  const rename = (x: Val<MIR>) => typeof x === 'number' ? some(env.get(x)) : asValue(x)
  const coerce = (x: Val<MIR>) =>
    typeof x === 'number' || x instanceof Value ? rename(x) :
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
      } else if (st.expr.head === 'func') {
        const [f, I, O] = st.expr.body
        const name = names.get([types.asTag(f), asType(ir.type(I))])
        env.set(v, out.push({ ...st, expr: xtuple(Value.i32(tables.func(name))), type: type(st.type) }))
      } else if (['tuple', 'ref'].includes(st.expr.head)) {
        env.set(v, out.push({ ...st, expr: st.expr.map(rename) as Expr<Value>, type: type(st.type) }))
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
        let expr: Expr<Value>
        if (Array.isArray(callee)) {
          const I = args.flatMap(a => wlayout(types.abstract(asType(ir.type(a))))) // TODO shouldn't get consts here
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
        const sig: Sig = [F, ...Ts]
        args = args.filter(x => !types.isValue(asType(ir.type(x))))
        const expr = instr(wasm.Call(names.get(sig)), ...args.map(rename))
        env.set(v, out.push({ ...st, expr: expr, type: wparts(st.type) }))
      } else if (st.expr.head === 'call_indirect') {
        const [f, args] = st.expr.body
        const I = wlayout(asType(ir.type(args)))
        const O = wlayout(asType(st.type))
        env.set(v, out.push({ ...st, expr: instr(wasm.CallIndirect(wasm.Signature(I, O), 'funcs'), rename(args), rename(f)), type: O }))
      } else if (st.expr instanceof Branch) {
        const expr = st.expr.map(coerce)
        env.set(v, out.push({ ...st, expr, type: unreachable }))
      } else if (st.expr.head === 'setglobal') {
        const expr = st.expr.map(coerce) as Expr<Value>
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
      let p = st.expr.value
      if (asArray(st.type).length > 1)
        p = pr.push(pr.stmt(expr('ref', p, Value.i64(i + 1))))
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
  globals: Cache<Binding, string[]>
  names: DualCache<Sig | WSig, string>
  funcs: Cache<Sig, wasm.Func>
  constructor(defs: Definitions, code: Accessor<Sig, Redirect | MIR>) {
    this.tables = new Tables()
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
    const count = new Map<string, number>()
    // TODO should be `funcs`, not `code`, to make global redefs of the same type
    // more efficient. But that creates an awkward cycle between names and funcs.
    this.names = new DualCache<Sig | WSig, string>(sig => {
      if (!Array.isArray(sig[0])) code.get(sig as Sig) // new name if code changes
      const id = wname(sig[0])
      const c = (count.get(id) ?? 0) + 1
      count.set(id, c)
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
  for (const f of mod.tables.funcs) visit(f)
  return calls
}

// Batch emitter, for AOT compilation

type Emitter = { emit(calls: Map<string, wasm.Func | WSig>, func: wasm.Func): void }

class BatchEmitter implements Emitter {
  tables: Tables
  main: string[]
  seen: Set<string>
  funcs: wasm.Func[]
  imports: wasm.Import[]
  constructor(tables: Tables) {
    this.tables = tables
    this.main = []
    this.seen = new Set()
    this.funcs = []
    this.imports = []
  }

  clone(): BatchEmitter {
    const em = new BatchEmitter(this.tables)
    em.main = [...this.main]
    em.seen = new Set(this.seen)
    em.funcs = [...this.funcs]
    em.imports = [...this.imports]
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
    for (const f of this.tables.funcs) this.emitName(calls, f)
    this.main.push(func.name)
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
      wasm.Export('frees')
    ],
    globals: [...em.tables.globals, ...refGlobals].map(g => wasm.Global(...g)),
    tables: moduleTables(em.tables),
    elems: [wasm.Elem('funcs', em.tables.funcs)],
    mems: [wasm.Mem('cm32p2_memory', 0)],
    customs: metaSection(em.tables)
  })
  return mod
}

function emitwasm(em: BatchEmitter, mod: Wasm, strip = false): Uint8Array {
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
