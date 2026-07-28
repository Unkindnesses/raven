import * as types from '../frontend/types.js'
import { Tag, Type } from '../frontend/types.js'
import {
  Method, Definitions, IRValue, Wasm, Call, Dispatch, Invoke, Closure, Global, MIR, SetGlobal,
  StringRef, JS, Value
} from '../frontend/modules.js'
import { Lowered, xpack } from '../frontend/lower.js'
import { Def, Stack } from '../dwarf/index.js'
import * as ir from '../utils/ir.js'
import { unreachable } from '../utils/ir.js'
import { Branch, asType } from '../utils/ir.js'
import { dispatcherDef, indexer, matchMethods, Interpreter, Methods, partial_match, Match } from './patterns.js'
import { wasmPartials } from '../backend/wasm.js'
import { getIntValue, invoke_method, load_method, notnil_method, pack_method, packcat_method, part_method, store_method, tagcast_method } from './primitives.js'
import { isEqual } from '../utils/isEqual.js'
import { some } from '../utils/map.js'
import { Caching, CycleCache } from '../utils/cache.js'
import { Accessor } from '../utils/fixpoint.js'
import { xcall, xlist } from '../frontend/lower.js'
import { partialPrimitive } from './prim_map.js'
import { pattern } from '../frontend/patterns.js'

export { Tracer, Traced }

type Func = Dispatch | Method
type Trace = [MIR, ir.Anno<Type>] | undefined

const TRACE_LIMIT = 1_000

function push(code: ir.Fragment<MIR>, ex: ir.Expr<IRValue>, T: types.Type): ir.Val<MIR> {
  return types.isValue(T) ? T : code.push(code.stmt(ex, { type: T }))
}

function parts(ir: MIR, v: ir.Val<MIR>): ir.Val<MIR>[] | undefined {
  if (v instanceof Value) return
  if (typeof v !== 'number') return types.allparts(v)
  if (!ir.has(v)) return
  const st = ir.get(v)
  if (st.expr.head === 'pack') return st.expr.body
  if (st.expr instanceof Invoke && pack_method.isEqual(st.expr.method))
    return parts(ir, st.expr.body[0])?.slice(1)
  if (st.expr instanceof Invoke && packcat_method.isEqual(st.expr.method)) {
    const xs = parts(ir, st.expr.body[0])?.slice(1)
    if (!xs || xs.length === 0) return
    const init = parts(ir, xs[0])
    if (!init) return
    let ys = [...init]
    for (const x of xs.slice(1)) {
      const ps = parts(ir, x)
      if (!ps) return
      ys = [...ys, ...ps.slice(1)]
    }
    return ys
  }
}

function ispure(ex: ir.Expr<IRValue>): boolean {
  if (ex.head === 'pack') return true
  if (ex instanceof Invoke) {
    if ([pack_method, part_method, packcat_method].some(x => x.isEqual(ex.method))) return true
  }
  return false
}

class TraceIR implements ir.Fragment<MIR> {
  ir: MIR
  stack: [Def, Stack][] = []
  maps = [new Map<number, IRValue | number>()]
  uses = new Map<number, number>()
  id = 0
  constructor(meta: Def) {
    this.ir = MIR(meta)
  }

  var(): number { return this.id -= 1 }

  get map() { return this.maps[this.maps.length - 1] }

  defaultSrc(s?: ir.Source): Stack {
    if (this.stack.length === 0) throw new Error('trace error')
    return [[this.stack[this.stack.length - 1][0], s]]
  }

  stmt(expr: ir.Expr<IRValue>, opts: ir.StmtOpts<Type> = {}): ir.Statement<IRValue, Type> {
    let { src } = opts
    if (!Array.isArray(src)) src = this.defaultSrc(src)
    return this.ir.stmt(expr, { ...opts, src })
  }

  substitute(x: IRValue | number): IRValue | number {
    return typeof x === 'number' ? some(this.map.get(x)) : x
  }

  substitution(x: IRValue | number): IRValue | number {
    if (typeof x !== 'number') return x
    const v = this.var()
    this.map.set(v, x)
    return v
  }

  replace(x: number, y: IRValue | number) {
    this.map.set(x, this.substitute(y))
  }

  argument(T: Type) {
    return this.substitution(this.ir.argument(T))
  }

  push(stmt: ir.Statement<IRValue, Type>) {
    stmt = { ...stmt, expr: stmt.expr.map(x => this.substitute(x)), src: [...this.stack.flatMap(x => x[1]), ...stmt.src] }
    if (stmt.expr instanceof Invoke &&
      (notnil_method.isEqual(stmt.expr.method) || tagcast_method.isEqual(stmt.expr.method))) {
      let x = stmt.expr.body[0]
      if (isEqual(this.ir.type(x), stmt.type)) return this.substitution(x)
    }
    if (stmt.expr instanceof Invoke && part_method.isEqual(stmt.expr.method)) {
      let i = asType(this.ir.type(stmt.expr.body[1]))
      let ps = parts(this.ir, stmt.expr.body[0])
      if (ps && getIntValue(i)) return this.substitution(ps[getIntValue(i)!])
    }
    stmt.expr.map(x => {
      if (typeof x === 'number') this.uses.set(x, (this.uses.get(x) ?? 0) + 1)
      return x
    })
    // TODO: deduplicate
    return this.substitution(this.ir.push(stmt))
  }

  delete(v: number) {
    this.ir.get(v).expr.map(x => {
      if (typeof x === 'number') this.uses.set(x, some(this.uses.get(x)) - 1)
      return x
    })
    this.ir.delete(v)
  }

  prune() {
    for (const [v, st] of [...this.ir].reverse()) {
      if ((this.uses.get(v) ?? 0) === 0 && ispure(st.expr)) this.delete(v)
    }
  }

  type(v: ir.Val<MIR>) {
    return this.ir.type(this.substitute(v))
  }

  return(v: ir.Val<MIR>) {
    v = this.substitute(v)
    if (typeof v === 'number') this.uses.set(v, (this.uses.get(v) ?? 0) + 1)
    this.ir.return(v)
  }

  enter(def: Def, frame?: Stack) {
    frame ??= this.stack.length > 0 ? this.defaultSrc() : []
    this.stack.push([def, frame])
  }

  exit() { this.stack.pop() }

  scope(args: number[], inputs: ir.Val<MIR>[]) {
    if (args.length !== inputs.length) throw new Error('trace error')
    inputs = inputs.map(x => this.substitute(x))
    this.maps.push(new Map())
    for (let i = 0; i < args.length; i++)
      this.map.set(args[i], inputs[i])
    return this
  }

  unscope(val: ir.Val<MIR>) {
    val = this.substitute(val)
    this.maps.pop()
    return this.substitution(val)
  }
}

function keyindex(Ts: Type[]): Type | undefined {
  if (Ts.length !== 2) return
  const [record, key] = Ts
  if (!(key instanceof Tag) || record.kind !== 'pack' || !types.tag('common.record/Record').isEqual(types.tagOf(record))) return
  const fields = types.parts(record)
  for (let i = 0; i < fields.length; i++) {
    const field = fields[i]
    if (field.kind !== 'pack' || !types.tag('common.record/Pair').isEqual(types.tagOf(field))) return
    const k = types.part(field, 1)
    if (!(k instanceof Tag)) return
    if (k.isEqual(key)) return types.int64(i + 1)
  }
  return types.nil
}

function packv(code: ir.Fragment<MIR>, ...xs: ir.Val<MIR>[]): ir.Val<MIR> {
  return push(code, xpack<IRValue>(...xs), types.pack(...xs.map(x => ir.asType(code.type(x)))))
}

function bindings(code: ir.Fragment<MIR>, V: types.Type, val: ir.Val<MIR>, m: Match): ir.Val<MIR> {
  const fields = [...m].map(([name, [, path]]) =>
    packv(code, types.tag('common.record/Pair'), types.tag(name), indexer(code, V, val, path, push)))
  return packv(code, types.tag('common.record/Record'), ...fields)
}

function static_match(int: Interpreter, code: ir.Fragment<MIR>, Ts: types.Type, args: ir.Val<MIR>): ir.Val<MIR> | undefined {
  if (Ts.kind !== 'pack' || types.nparts(Ts) !== 2) return
  const [V, P] = [types.part(Ts, 1), types.part(Ts, 2)]
  const m = partial_match(int, pattern(P), V)
  if (m === undefined) return
  return m === null ? types.nil : bindings(code, V, indexer(code, Ts, args, [1], push), m)
}

function swapresult(code: ir.Fragment<MIR>, want: boolean, have: boolean, result: ir.Val<MIR>): ir.Val<MIR> {
  if (want === have) return result
  const T = asType(code.type(result))
  return want
    ? push(code, xlist(result), types.list(T))
    : push(code, xcall(part_method, result, types.Type(1n)), types.part(T, 1))
}

class Tracer {
  count = 0

  constructor(readonly defs: Definitions, readonly lowered: Lowered,
    readonly interp: Interpreter, readonly methods: Methods) { }

  trace(f: Func, ...args: Type[]): Trace {
    this.count = 0
    const meta = f instanceof Method ? this.lowered.ir(f).meta : dispatcherDef(f)
    const code = new TraceIR(meta)
    const argv = args.map(a => code.argument(a))
    const ret = this.traceCall(code, f, argv)
    if (ret === undefined) return
    code.return(ret)
    code.prune()
    return [code.ir, code.type(ret)]
  }

  traceCall(code: TraceIR, x: Func, args: ir.Val<MIR>[], src?: Stack): ir.Val<MIR> | undefined {
    if (x instanceof Method) return this.traceMethod(code, x, args, src)
    if (args.length !== 2) throw new Error('bug')
    if (this.count++ > TRACE_LIMIT) return
    code.enter(dispatcherDef(x), src)
    const result = this.traceFunc(code, x, args[0], args[1])
    code.exit()
    return result
  }

  traceIR(code: TraceIR, ir: MIR): ir.Val<MIR> | undefined {
    let bl = 1
    while (true) {
      for (const [v, st] of ir.block(bl)) {
        if (st.expr instanceof Call) {
          const op = code.type(st.expr.f)
          if (!(op instanceof Tag)) return // TODO support closures
          const result = this.traceCall(code, new Dispatch(op, st.expr.swap), st.expr.body, st.src)
          if (result === undefined) return
          code.replace(v, result)
        } else if (st.expr instanceof Invoke) {
          const result = this.traceMethod(code, st.expr.method, st.expr.body, st.src)
          if (result === undefined) return
          code.replace(v, result)
        } else if (st.expr instanceof Wasm) {
          let T = st.type
          const opname = !Array.isArray(st.expr.callee) && st.expr.callee.kind === 'op' ? st.expr.callee.name : ''
          if (opname && wasmPartials.has(opname)) {
            const args = st.expr.args.map(a => asType(code.type(a)))
            if (args.every(types.isValue)) T = wasmPartials.get(opname)!(...args)
            if (types.isValue(asType(T))) {
              code.replace(v, asType(T))
              continue
            }
          }
          if (T === unreachable) return
          code.replace(v, code.push({ ...st, type: T }))
        } else if (st.expr.head === 'pack') {
          const args = st.expr.body.map(a => asType(code.type(a)))
          const T = types.pack(...args)
          if (types.isValue(T)) code.replace(v, T)
          else code.replace(v, code.push({ ...st, type: types.pack(...args) }))
        } else if (st.expr instanceof Closure) {
          const T = types.Closure(st.expr.method, ...st.expr.args.map(a => asType(code.type(a))))
          if (types.isValue(T)) code.replace(v, T)
          else code.replace(v, code.push({ ...st, type: T }))
        } else if (st.expr instanceof Branch) {
          if (st.expr.isreturn()) return st.expr.args[0]
          if (st.expr.isunreachable()) throw new Error('unimplemented')
          if (st.expr.isconditional()) {
            const cond = asType(code.type(st.expr.when))
            if (!types.issubset(cond, types.bool())) throw new Error(`Expected boolean condition`)
            if (!types.isValue(cond)) return
            if (!isEqual(cond, types.bool(true))) continue
          }
          bl = st.expr.target
          let as = st.expr.args
          ir.block(st.expr.target).args.forEach((arg, i) => code.replace(arg, as[i]))
          break
        } else if (st.expr.head === 'tuple') {
          const T = asType(st.type)
          if (!types.isValue(T)) throw new Error('assert isvalue(st.type)')
          code.replace(v, T)
        } else if (st.expr instanceof StringRef) {
          code.replace(v, code.push({ ...st, type: types.String() }))
        } else if (st.expr instanceof JS) {
          code.replace(v, code.push({ ...st, type: types.Ref }))
        } else if (st.expr instanceof Global) {
          const T = this.defs.resolve_static(st.expr.binding)
          if (T === unreachable) return
          if (types.isValue(T)) code.replace(v, T)
          else code.replace(v, code.push({ ...st, type: T }))
        } else if (st.expr instanceof SetGlobal) {
          code.replace(v, code.push({ ...st, type: types.nil }))
        } else {
          throw new Error(`Unknown expr type ${st.expr.head}`)
        }
      }
    }
  }

  traceMethod(code: TraceIR, meth: Method, args: ir.Val<MIR>[], src?: Stack): ir.Val<MIR> | undefined {
    const Ts = args.map(a => asType(code.type(a)))
    if (meth.name.isEqual(types.tag('common.record/keyindex'))) {
      const result = keyindex(Ts)
      if (result !== undefined) return result
    }
    // We can't evaluate `rvtype` here.
    if ([invoke_method, load_method].some(m => m.isEqual(meth))) return
    const partial = partialPrimitive(meth)
    if (partial) {
      const result = partial(...Ts)
      if (result === ir.unreachable) return
      if (![invoke_method, store_method].some(m => m.isEqual(meth)) && types.isValue(result)) return result
      return code.push(code.stmt(xcall(meth, ...args), { type: result }))
    } else {
      const ir = this.lowered.ir(meth)
      code.scope(ir.block(1).args, args)
      code.enter(ir.meta, src)
      const ret = this.traceIR(code, ir)
      code.exit()
      if (ret === undefined) return
      return code.unscope(ret)
    }
  }

  traceFunc(code: TraceIR, func: Dispatch, f: ir.Val<MIR>, args: ir.Val<MIR>): ir.Val<MIR> | undefined {
    const F = asType(code.type(f))
    const Ts = asType(code.type(args))
    if (types.tag('common.patterns/match').isEqual(F)) {
      const result = static_match(this.interp, code, Ts, args)
      if (result !== undefined) return swapresult(code, func.swap, false, result)
    }
    const fullTs = types.list(F, Ts)
    const full = code.push(code.stmt(xlist<IRValue>(f, args), { type: fullTs }))
    for (const [meth, m] of this.methods.get([func.func, fullTs])) {
      if (m === undefined) return
      if (this.traceMethod(code, meth.signature, []) === undefined) return // trace side effects
      const as = meth.sig.args.map((a, i) => indexer(code, fullTs, full, m.get(a)![1], push))
      const result = this.traceMethod(code, meth, as)
      if (result === undefined) return
      return swapresult(code, func.swap, meth.swaps, result)
    }
  }
}

class Traced implements Caching, Interpreter {
  private constructor(readonly results: Accessor<[Func, ...Type[]], Trace>) { }

  static create(defs: Definitions, lowered: Lowered, methods?: Methods) {
    const init = (_: [Func, ...Type[]]): Trace => undefined
    const results = new CycleCache<[Func, ...Type[]], Trace>(init, (self, sig) => {
      const int = new Traced(self)
      methods ??= { get: key => matchMethods(defs, int, key) }
      return new Tracer(defs, lowered, int, methods).trace(...sig)
    })
    return new Traced(results)
  }

  get subcaches() { return [this.results as Caching] }

  trace(func: Func, ...args: Type[]): Trace {
    return this.results.get([func, ...args])
  }

  eval(func: Tag, ...args: Type[]): Type | undefined {
    const result = this.results.get([new Dispatch(func), func, ...args])
    if (result === undefined || result[1] === ir.unreachable) return
    return asType(result[1])
  }
}
