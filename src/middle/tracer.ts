import * as types from '../frontend/types.js'
import { Tag, Type } from '../frontend/types.js'
import { Method, Definitions, IRValue, Wasm, Invoke, Global, MIR, SetGlobal, StringRef, JS, Value } from '../frontend/modules.js'
import { Def, Stack } from '../dwarf/index.js'
import * as ir from '../utils/ir.js'
import { unreachable } from '../utils/ir.js'
import { Branch, asType } from '../utils/ir.js'
import { dispatcherDef, partial_match, Path, Interpreter } from './patterns.js'
import { wasmPartials } from '../backend/wasm.js'
import { getIntValue, invoke_method, notnil_method, pack_method, packcat_method, part_method, tagcast_method } from './primitives.js'
import { isEqual } from '../utils/isEqual.js'
import { asNumber, only, some } from '../utils/map.js'
import { Caching, CycleCache } from '../utils/cache.js'
import { Accessor } from '../utils/fixpoint.js'
import { xcall, xlist, xpart } from '../frontend/lower.js'

export { Tracer, Traced }

type Func = Tag | Method
type Trace = [MIR, ir.Anno<Type>] | undefined

const TRACE_LIMIT = 1_000

function parts(ir: MIR, v: ir.Val<MIR>): ir.Val<MIR>[] | undefined {
  if (v instanceof Value) return
  if (typeof v !== 'number') return types.parts(v)
  if (!ir.has(v)) return
  const st = ir.get(v)
  if (st.expr.head === 'pack') return st.expr.body
  if (st.expr instanceof Invoke && st.expr.method === pack_method)
    return parts(ir, asNumber(st.expr.body[0]))?.slice(1)
  // TODO packcat
}

function ispure(ex: ir.Expr<IRValue>): boolean {
  if (ex.head === 'pack') return true
  if (ex instanceof Invoke) {
    if ([pack_method, part_method, packcat_method].some(x => x === ex.method)) return true
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
    if (stmt.expr instanceof Invoke && (stmt.expr.method === notnil_method || stmt.expr.method === tagcast_method)) {
      let x = stmt.expr.body[0]
      if (isEqual(this.ir.type(x), stmt.type)) return this.substitution(x)
    }
    if (stmt.expr instanceof Invoke && stmt.expr.method === part_method) {
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

function indexer(code: ir.Fragment<MIR>, T: types.Type, arg: ir.Val<MIR>, path: Path): ir.Val<MIR> {
  if (path.length === 0) return arg
  const [p, ...rest] = path
  if (typeof p !== 'number') {
    const ps: ir.Val<MIR>[] = []
    for (let i = p.start; i <= p.end; i++) {
      const part = types.part(T, i)
      if (types.isValue(part)) ps.push(part)
      else ps.push(code.push(code.stmt(xpart(arg, types.Type(BigInt(i))), { type: part })))
    }
    const L = types.list(...ps.map(v => code.type(v) as types.Type))
    arg = types.isValue(L) ? L : code.push(code.stmt(xlist<IRValue>(...ps), { type: L }))
  } else {
    T = types.part(T, p)
    arg = types.isValue(T) ? T : code.push(code.stmt(xpart(arg, types.Type(BigInt(p))), { type: T }))
  }
  return indexer(code, T, arg, rest)
}

class Tracer {
  count = 0

  constructor(readonly defs: Definitions, readonly int: Interpreter) { }

  _trace(f: Func, ...args: Type[]): [MIR, ir.Anno<Type>] | undefined {
    const meta = f instanceof Tag ? dispatcherDef(f) : this.defs.ir(f).meta
    const code = new TraceIR(meta)
    const argv = args.map(a => code.argument(a))
    const ret = this.trace(code, f, argv)
    if (ret === undefined) return
    code.return(ret)
    code.prune()
    return [code.ir, code.type(ret)]
  }

  trace(code: TraceIR, x: Func, args: ir.Val<MIR>[], src?: Stack): ir.Val<MIR> | undefined {
    if (x instanceof Method) return this.traceMethod(code, x, args, src)
    if (this.count++ > TRACE_LIMIT) return
    code.enter(dispatcherDef(x), src)
    const result = this.traceFunc(code, x, only(args))
    code.exit()
    return result
  }

  traceIR(code: TraceIR, ir: MIR, ...args: ir.Val<MIR>[]): ir.Val<MIR> | undefined {
    let bl = 1
    while (true) {
      for (const [v, st] of ir.block(bl)) {
        if (st.expr.head === 'call') {
          const op = code.type(st.expr.body[0])
          if (!(op instanceof Tag)) return
          const result = this.trace(code, op, st.expr.body.slice(1), st.src)
          if (result === undefined) return
          code.replace(v, result)
        } else if (st.expr instanceof Invoke) {
          const result = this.traceMethod(code, st.expr.method, st.expr.body as ir.Val<MIR>[], st.src)
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
          code.replace(v, code.push({ ...st, type: code.type(st.expr.value) }))
        } else {
          throw new Error(`Unknown expr type ${st.expr.head}`)
        }
      }
    }
  }

  traceMethod(code: TraceIR, meth: Method, args: ir.Val<MIR>[], src?: Stack): ir.Val<MIR> | undefined {
    const Ts = args.map(a => asType(code.type(a)))
    if (meth.func) {
      const result = meth.func(...Ts)
      if (result === ir.unreachable) return
      if (meth !== invoke_method && types.isValue(result)) return result
      return code.push(code.stmt(xcall(meth, ...args), { type: result }))
    } else {
      const ir = some(this.defs.ir(meth))
      code.scope(ir.block(1).args, args)
      code.enter(ir.meta, src)
      const ret = this.traceIR(code, ir, ...args)
      code.exit()
      if (ret === undefined) return
      return code.unscope(ret)
    }
  }

  traceFunc(code: TraceIR, func: Tag, args: ir.Val<MIR>): ir.Val<MIR> | undefined {
    const Ts = asType(code.type(args))
    const methods = this.defs.methods(func)
    for (const meth of methods.slice().reverse()) {
      const m = partial_match(this.int, meth.sig.pattern, Ts)
      if (m === null) continue
      if (m === undefined) return
      const as = meth.sig.args.map((a, i) => indexer(code, Ts, args, m.get(a)![1]))
      let result = this.traceMethod(code, meth, as)
      if (result === undefined) return
      const T = asType(code.type(result))
      if (meth.sig.swap.size === 0)
        result = types.isValue(T)
          ? types.list(T)
          : code.push(code.stmt(xlist(result), { type: types.list(T) }))
      return result
    }
  }
}

class Traced implements Caching {
  readonly results: Accessor<[Func, ...Type[]], Trace>

  constructor(readonly defs: Definitions, results?: Accessor<[Func, ...Type[]], Trace>) {
    if (results) {
      this.results = results
    } else {
      const init = (_: [Func, ...Type[]]): Trace => undefined
      this.results = new CycleCache<[Func, ...Type[]], Trace>(init, (self, sig) => {
        const int = new Traced(defs, self)
        return new Tracer(defs, int)._trace(...sig)
      })
    }
  }

  get subcaches() { return [this.results as Caching] }

  trace(func: Func, ...args: Type[]): Trace {
    return this.results.get([func, ...args])
  }

  get(func: Func, args: Type[]): Type | undefined {
    const result = this.results.get([func, ...args])
    if (result === undefined || result[1] === ir.unreachable) return
    return asType(result[1])
  }
}
