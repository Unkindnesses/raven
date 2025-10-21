import * as types from '../frontend/types'
import { Type, Bits, asBits, bits, tag } from '../frontend/types'
import * as wasm from '../wasm/wasm'
import { binary as wasmBinary } from '../wasm/binary'
import { writeFile } from 'fs/promises'
import { options } from '../utils/options'
import { irfunc, Instr, setdiff } from '../wasm/ir'
import { unreachable, Anno, Pipe, expr, Val, asAnno, Branch } from '../utils/ir'
import isEqual from 'lodash/isEqual'
import { layout } from '../middle/expand'
import { Cache, Caching, DualCache, reset as resetCaches, pipe } from '../utils/cache'
import { Binding, Definitions, MIR, WImport, WIntrinsic, Method, Const, asFunc, asBinding, StringRef } from '../frontend/modules'
import { Def } from '../dwarf'
import { Redirect, type Sig } from '../middle/abstract'
import { Accessor } from '../utils/fixpoint'
import { xtuple } from '../frontend/lower'

export { wasmPartials, Wasm, BatchEmitter, StreamEmitter, Emitter, emitwasm, emitwasmBinary, lowerwasm, lowerwasm_globals }

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

function wparts(T: Anno<Type>): wasm.Type[] {
  return T === unreachable ? [] : layout(types.asType(T))
}

class WGlobals implements Caching {
  types: wasm.Type[]
  globals: Cache<Binding, number[]>
  constructor(defs: Definitions) {
    this.types = []
    this.globals = new Cache<Binding, number[]>(b => {
      let T: Anno<Type> | Binding = b
      while (T instanceof Binding) T = defs.global(T)
      const start = this.types.length - 1
      const l = wparts(T)
      this.types.push(...l)
      return Array.from({ length: l.length }, (_, i) => start + i + 1)
    })
  }
  get subcaches() { return [this.globals] }
  get(b: Binding): number[] { return this.globals.get(b) }
}

function tableid<T>(xs: T[], x: T): number {
  const i = xs.findIndex(y => isEqual(y, x))
  if (i !== -1) return i
  xs.push(x)
  return xs.length - 1
}

class Tables {
  strings: string[] = []
  funcs: string[] = []
  string(s: string): number { return tableid(this.strings, s) }
  func(f: string): number { return tableid(this.funcs, f) }
}

function instr<T>(instr: wasm.Instruction, ...args: (T | number)[]): Instr<T> {
  return new Instr(instr, args)
}

function lowerwasm(ir: MIR, names: DualCache<Sig | WSig, string>, globals: WGlobals, tables: Tables): MIR {
  const pr = new Pipe(ir)
  for (let [v, st] of pr) {
    if (st.expr instanceof StringRef) {
      const name = names.get([new WImport('support', 'string'), [wasm.Type.i32], [wasm.Type.i32]])
      pr.set(v, instr(wasm.Call(name), Const.i32(tables.string(st.expr.value))))
      pr.setType(v, [wasm.Type.i32])
    } else if (st.expr.head === 'func') {
      const [f, I, O] = st.expr.body
      const name = names.get([types.asTag(f), types.asType(I)])
      pr.set(v, xtuple(Const.i32(tables.func(name))))
    } else if (['tuple', 'ref'].includes(st.expr.head)) {
    } else if (st.expr.head === 'cast') { // TODO just use `tuple` instead
      const arg = st.expr.body[0]
      if (!isEqual(layout(types.asType(st.type)), layout(types.asType(ir.type(arg)))))
        throw new Error('cast: layout mismatch')
      pr.replace(v, arg)
    } else if (st.expr.head === 'global') {
      const ids = globals.get(asBinding(st.expr.body[0]))
      const parts = layout(types.asType(st.type))
      const ps: Val<MIR>[] = []
      for (let i = 0; i < ids.length; i++)
        ps.push(pr.insert(v, pr.stmt(instr(wasm.GetGlobal(ids[i])), { type: [parts[i]] })))
      if (ps.length === 1) pr.replace(v, ps[0])
      else pr.set(v, xtuple(...ps))
    } else if (st.expr.head === 'call' && (st.expr.body[0] instanceof WIntrinsic || st.expr.body[0] instanceof WImport)) {
      const [callee, ...args] = st.expr.body
      if (callee instanceof WImport) {
        const I = args.flatMap(a => layout(types.abstract(types.asType(ir.type(a))))) // TODO shouldn't get consts here
        const O = st.type === unreachable ? [] : layout(types.asType(st.type))
        const name = names.get([callee, I, O])
        st = { ...st, expr: instr(wasm.Call(name), ...args) }
      } else {
        st = { ...st, expr: instr(wasm.Op(callee.name), ...args) }
      }
      // TODO deprecate array types
      pr.setStmt(v, { ...st, type: st.type === unreachable ? [] : Array.isArray(st.type) ? st.type : layout(types.asType(st.type)) })
      if (st.type === unreachable) pr.push(pr.stmt(instr(wasm.unreachable), { type: [] }))
    } else if (st.expr.head === 'call') {
      const Ts = st.expr.body.map(a => ir.type(a))
      if (Ts.some(t => t === unreachable)) throw new Error('unreachable arg in call')
      const sig: Sig = [asFunc(Ts[0]), ...Ts.slice(1).map(t => types.asType(t))]
      const args = st.expr.body.slice(1).filter(x => !types.isValue(types.asType(ir.type(x))))
      const expr = instr(wasm.Call(names.get(sig)), ...args)
      pr.setStmt(v, { ...st, expr, type: wparts(asAnno(types.asType, st.type)) })
    } else if (st.expr.head === 'call_indirect') {
      const [f, args] = st.expr.body
      const I = layout(types.asType(ir.type(args)))
      const O = layout(types.asType(st.type))
      pr.setStmt(v, { ...st, expr: instr(wasm.CallIndirect(wasm.Signature(I, O), 0), args, f), type: O })
    } else if (st.expr instanceof Branch) {
    } else if (st.expr.head === 'set' && st.expr.body[0] instanceof Binding) {
    } else throw new Error(`unrecognised ${st.expr.head} expression`)
  }
  const out = pr.finish()
  for (const b of out.blocks())
    for (let i = 0; i < b.bb.args.length; i++)
      b.bb.args[i][1] = layout(types.asType(b.bb.args[i][1]))
  return out
}

function lowerwasm_globals(ir: MIR, globals: WGlobals): MIR {
  const pr = new Pipe(ir)
  for (const [v, st] of pr) {
    if (!(st.expr.head === 'set' && st.expr.body[0] instanceof Binding)) continue
    pr.delete(v)
    const ids = globals.get(st.expr.body[0])
    for (let i = 0; i < ids.length; i++) {
      let p = st.expr.body[1]
      if (wparts(asAnno(types.asType, st.type)).length > 1)
        p = pr.push(pr.stmt(expr('ref', p, Const.i64(i + 1))))
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

type WSig = [WImport, wasm.Type[], wasm.Type[]]

function wname(f: types.Tag | Method | WImport): string {
  if (f instanceof types.Tag) return f.path
  if (f instanceof Method) return `${f.name.path}:method`
  if (f instanceof WImport) return `${f.mod}:${f.name}`
  throw new Error('unreachable')
}

class Wasm implements Caching {
  globals: WGlobals
  tables: Tables
  names: DualCache<Sig | WSig, string>
  funcs: Cache<Sig, wasm.Func>
  constructor(defs: Definitions, code: Accessor<Sig, Redirect | MIR>) {
    this.globals = new WGlobals(defs)
    this.tables = new Tables()
    const count = new Map<string, number>()
    // TODO should be `funcs`, not `code`, to make global redefs of the same type
    // more efficient. But that creates an awkward cycle between names and funcs.
    this.names = new DualCache<Sig | WSig, string>(sig => {
      if (!(sig[0] instanceof WImport)) code.get(sig as Sig) // new name if code changes
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
  haskey(name: string): boolean {
    return this.names.hasvalue(name) && !(this.names.getkey(name)[0] instanceof WImport)
  }

  getByName(name: string): wasm.Func {
    const sig = this.names.getkey(name)
    if (sig[0] instanceof WImport) throw new Error('nope')
    return this.get(sig as Sig)
  }

  get subcaches() { return [this.globals, this.names, this.funcs] }
  reset(deps: Set<bigint>) { resetCaches(pipe(this.globals, this.names, this.funcs), deps) }
}

// Batch emitter, for AOT compilation

type Emitter = { emit(mod: Wasm, func: wasm.Func): void }

class BatchEmitter implements Emitter {
  main: string[]
  seen: Set<string>
  funcs: wasm.Func[]
  imports: wasm.Import[]
  constructor() {
    this.main = []
    this.seen = new Set()
    this.funcs = []
    this.imports = []
  }

  clone(): BatchEmitter {
    const em = new BatchEmitter()
    em.main = [...this.main]
    em.seen = new Set(this.seen)
    em.funcs = [...this.funcs]
    em.imports = [...this.imports]
    return em
  }

  private emitFunc(mod: Wasm, func: wasm.Func) {
    this.funcs.push(func)
    for (const f of wasm.callees(func)) this.emitName(mod, f)
  }

  private emitName(mod: Wasm, f: string) {
    if (this.seen.has(f)) return
    this.seen.add(f)
    if (!mod.names.hasvalue(f)) return
    const key = mod.names.getkey(f)
    if (key[0] instanceof WImport) {
      const [imp, I, O] = key as WSig
      this.imports.push(wasm.Import(imp.mod, imp.name, f, wasm.Signature(I, O)))
    } else {
      this.emitFunc(mod, mod.get(key as Sig))
    }
  }

  emit(mod: Wasm, func: wasm.Func) {
    this.emitFunc(mod, func)
    for (const f of mod.tables.funcs) this.emitName(mod, f)
    this.main.push(func.name)
  }
}

function startfunc(main: string[]): wasm.Func {
  const meta = Def('_start', undefined, true)
  const instrs = [...main.map(m => wasm.Call(m)), wasm.Const(wasm.Type.i32, 0)]
  const body = wasm.Block(instrs, instrs.map(() => wasm.LineInfo([[meta, meta.source]])))
  return wasm.Func('_start', wasm.Signature([], [wasm.Type.i32]), [], body, meta)
}

function metaSection(strings: string[]): wasm.CustomSection {
  const meta = { strings, jsalloc: options().jsalloc }
  return wasm.CustomSection('raven.meta', new TextEncoder().encode(JSON.stringify(meta)))
}

function wasmmodule(em: BatchEmitter, globals: WGlobals, tables: Tables): wasm.Module {
  em.funcs.unshift(startfunc(em.main))
  return wasm.Module({
    funcs: em.funcs,
    imports: em.imports,
    exports: [
      wasm.Export('_start', '_start'),
      wasm.Export('_start', 'cm32p2|wasi:cli/run@0.2|run')
    ],
    globals: globals.types.map(t => wasm.Global(t)),
    tables: [wasm.Table(tables.funcs.length)],
    elems: [wasm.Elem(0, tables.funcs)],
    mems: [wasm.Mem(0, undefined, 'cm32p2_memory')],
    customs: [metaSection(tables.strings)]
  })
}

async function binary(m: wasm.Module, file: string, strip = false): Promise<void> {
  await writeFile(file, wasmBinary(m, strip))
}

async function emitwasm(em: BatchEmitter, mod: Wasm, out: string, strip = false): Promise<void> {
  await binary(wasmmodule(em, mod.globals, mod.tables), out, strip)
}

function emitwasmBinary(em: BatchEmitter, mod: Wasm, strip = false): Uint8Array {
  return wasmBinary(wasmmodule(em, mod.globals, mod.tables), strip)
}

// Stream emitter, for REPL

function wimport(mod: Wasm, f: string): wasm.Import {
  const sig = mod.names.getkey(f)
  if (sig[0] instanceof WImport) {
    const [imp, I, O] = sig as WSig
    return wasm.Import(imp.mod, imp.name, f, wasm.Signature(I, O))
  } else {
    const fn = mod.get(sig as Sig)
    return wasm.Import('wasm', f, f, fn.sig)
  }
}

class StreamEmitter implements Emitter {
  seen: Set<string>
  queue: wasm.Module[]
  globals: number
  constructor() {
    this.seen = new Set()
    this.queue = []
    this.globals = -1
  }

  private emitFunc(mod: Wasm, func: wasm.Func, fs: wasm.Func[], imports: string[]) {
    fs.push(func)
    for (const f of wasm.callees(func)) this.emitName(mod, f, fs, imports)
  }

  private emitName(mod: Wasm, f: string, fs: wasm.Func[], imports: string[]) {
    imports.push(f)
    if (this.seen.has(f)) return
    this.seen.add(f)
    if (mod.haskey(f)) this.emitFunc(mod, mod.getByName(f), fs, imports)
    else imports.push(f)
  }

  emit(mod: Wasm, func: wasm.Func) {
    const first = this.globals === -1
    if (first) this.globals = 0
    const fs: wasm.Func[] = []
    const imports: string[] = []
    this.emitFunc(mod, func, fs, imports)
    for (const f of mod.tables.funcs) this.emitName(mod, f, fs, imports)
    fs.unshift(startfunc([func.name]))
    const iimports = setdiff(imports, fs.map(f => f.name)).map(f => wimport(mod, f))
    const gimports: wasm.Import[] = []
    for (let i = 1; i <= this.globals; i++)
      gimports.push(wasm.Import('wasm', `global${i - 1}`, `global${i - 1}`, wasm.Global(mod.globals.types[i - 1])))
    const globals: wasm.Global[] = []
    for (let i = this.globals + 1; i <= mod.globals.types.length; i++)
      globals.push(wasm.Global(mod.globals.types[i - 1], { name: `global${i - 1}` }))
    if (!first) iimports.push(wasm.Import('wasm', 'memory', 'memory', wasm.Mem(0)))
    // TODO shared table
    const wmod = wasm.Module({
      funcs: fs,
      imports: [...gimports, ...iimports],
      exports: fs.map(f => wasm.Export(f.name, f.name)),
      globals,
      tables: [wasm.Table(mod.tables.funcs.length)],
      elems: [wasm.Elem(0, Array.from(mod.tables.funcs))],
      mems: first ? [wasm.Mem(0, undefined, 'memory')] : [],
      customs: [metaSection(mod.tables.strings)]
    })
    this.queue.push(wmod)
    this.globals = mod.globals.types.length
  }
}
