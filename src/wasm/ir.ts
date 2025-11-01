import isEqual from 'lodash/isEqual'
import * as wasm from './wasm'
import { Func, NumType, ValueType } from './wasm'
import { Pipe, liveness, Branch, Expr, expr, Source, CFG, Component, components, entry, Val } from '../utils/ir'
import { asArray, HashSet, HashMap, some, asNumber } from '../utils/map'
import { LineInfo, Def } from '../dwarf'
import { instructionToString } from './wat'
import * as ir from '../utils/ir'

export { Locals, stackshuffle, locals, shiftbps, irfunc, Instr, setdiff, union, intersect, Value, asValue, isValue, WIR }

class Value {
  constructor(
    readonly type: NumType,
    readonly value: number | bigint) { }
  static i32(v: number | bigint) { return new Value(NumType.i32, v) }
  static i64(v: number | bigint) { return new Value(NumType.i64, v) }
  static f32(v: number | bigint) { return new Value(NumType.f32, v) }
  static f64(v: number | bigint) { return new Value(NumType.f64, v) }
  static from(t: NumType, v: number | bigint): Value {
    if (t === NumType.i32) return Value.i32(v)
    if (t === NumType.i64) return Value.i64(v)
    if (t === NumType.f32) return Value.f32(v)
    if (t === NumType.f64) return Value.f64(v)
    let _: never = t
    throw new Error(`unreachable`)
  }
  toString() { return `${this.type}(${this.value})` }
}

function asValue(x: unknown): Value {
  if (x instanceof Value) return x
  throw new Error(`Expected Value, got ${typeof x}`)
}

function isValue(x: unknown): x is Value {
  return x instanceof Value
}

type WIR = ir.IR<Value, ValueType[]>

function showIRValue(x: Value | ValueType[]): string {
  if (Array.isArray(x)) return `[${x.join(', ')}]`
  return x.toString()
}

function irTypeOf(x: Value): ValueType[] {
  if (x instanceof Value) {
    if (x.type === 'i32') return [wasm.i32]
    if (x.type === 'i64') return [wasm.i64]
    if (x.type === 'f32') return [wasm.f32]
    if (x.type === 'f64') return [wasm.f64]
    throw new Error('unreachable')
  }
  throw new Error('no type values in WIR')
}

function WIR(meta: Def): WIR {
  return new ir.IR<Value, ValueType[]>(meta, irTypeOf, showIRValue)
}

type Const = wasm.Instruction & { kind: 'const' }

class Instr<T> extends Expr<T> {
  constructor(public instr: wasm.Instruction, body: (T | number)[] = []) {
    super('instr', body)
  }
  map(f: (x: T | number) => T | number): Instr<T> {
    return new Instr(this.instr, this.body.map(f))
  }
  show(pr: (x: T | number) => string): string {
    return `(${instructionToString(this.instr)} ${this.body.map(pr).join(' ')})`
  }
}

interface Locals<T> {
  stack: (T | Value)[]
  store: HashSet<T>
}

function Locals<T>(stack: (T | Value)[], store = new HashSet<T>()): Locals<T> {
  return { stack, store }
}

function toConst(c: Value): Const {
  switch (c.type) {
    case 'i32': return wasm.Const(wasm.i32, c.value)
    case 'i64': return wasm.Const(wasm.i64, c.value)
    case 'f32': return wasm.Const(wasm.f32, c.value)
    case 'f64': return wasm.Const(wasm.f64, c.value)
    default: throw new Error(`Unknown IR const type ${c.type}`)
  }
}

function setdiff<T>(xs: T[], ys: T[]): T[] {
  const res: T[] = []
  for (const x of xs)
    if (!ys.some(y => isEqual(x, y)) && !res.some(r => isEqual(r, x)))
      res.push(x)
  return res
}

function union<T>(xs: Iterable<T>, ys: Iterable<T>) {
  const s = new HashSet<T>()
  for (const x of xs) s.add(x)
  for (const y of ys) s.add(y)
  return s
}

function intersect<T>(xs: HashSet<T>, ys: Iterable<any>) {
  const s = new HashSet<T>()
  for (const y of ys) if (xs.has(y)) s.add(y)
  return s
}

function prefix<T>(xs: T[], ys: T[]): [number, number] {
  const dp = new Array<number>(ys.length + 1).fill(0)
  let len = 0, pos = ys.length
  for (let i = 0; i < xs.length; i++) for (let j = ys.length - 1; j >= 0; j--) {
    if (isEqual(xs[i], ys[j])) {
      dp[j + 1] = dp[j] + 1
      if (i + 1 === dp[j + 1] && dp[j + 1] > len) {
        len = dp[j + 1]
        pos = j + 1
      }
    } else dp[j + 1] = 0
  }
  return [len, pos]
}

type StackOp<T> =
  | { kind: 'get'; x: T }
  | { kind: 'set'; x: T }
  | { kind: 'tee'; x: T }
  | { kind: 'drop' }

function stackshuffle<T>(locals: Locals<T>, target: Locals<T>, options: { store?: boolean } = {}): [(StackOp<T> | Value)[], Locals<T>] {
  const { store = false } = options
  const implicit = setdiff(target.stack, locals.stack).filter((x): x is T => !isValue(x))
  const state: Locals<T> = { stack: [...locals.stack], store: union(locals.store, implicit) }
  const needed = union(target.store, target.stack)
  const path: (StackOp<T> | Value)[] = []
  const load = (x: T | Value) => {
    if (isValue(x)) {
      path.push(x)
      state.stack.push(x)
    } else {
      const last = path[path.length - 1]
      if (last && !isValue(last) && isEqual(last, { kind: 'set', x }))
        path[path.length - 1] = { kind: 'tee', x }
      else
        path.push({ kind: 'get', x })
      state.stack.push(x)
    }
  }
  while (true) {
    const [len, pos] = prefix(target.stack, state.stack)
    let live = state.store
    if (!store) live = union(state.store, state.stack.slice(0, state.stack.length - len).filter((x): x is T => !isValue(x)))
    if (
      pos === state.stack.length &&
      target.stack.slice(len).every(x => isValue(x) || state.store.has(x)) &&
      Array.from(target.store.values()).every(x => live.has(x))
    ) {
      target.stack.slice(len).forEach(load)
      break
    }
    const v = state.stack.pop()!
    if (isValue(v) || state.store.has(v) || !needed.has(v))
      path.push({ kind: 'drop' })
    else {
      path.push({ kind: 'set', x: v })
      state.store.add(v)
    }
  }
  return [path, state]
}

function opExpr(op: StackOp<[number, number]> | Value): Expr<Value> {
  if (isValue(op)) return new Instr(toConst(op), [])
  switch (op.kind) {
    case 'drop': return new Instr(wasm.Drop(), [])
    case 'get':
    case 'set':
    case 'tee':
      return expr(op.kind, op.x as any)
  }
}

function stack(ir: WIR): [WIR, wasm.ValueType[]] {
  type Part = Value | [number, number]
  let ret: wasm.ValueType[] = []
  const env = new Map<number, Part[]>()
  const parts = (x: Val<WIR>): Part[] =>
    typeof x === 'number' ? some(env.get(x)) : [x]
  const partsSet = (x: number, Ts: wasm.ValueType[]) => env.set(x, Array.from(Ts, (_, i) => [x, i + 1]))
  const lv = liveness(ir)
  const live = (v: number) => {
    const s = new HashSet<[number, number]>()
    for (const x of some(lv.stmts.get(v)))
      for (const p of some(env.get(x))) if (Array.isArray(p)) s.add(p)
    return s
  }
  for (const bl of ir.blocks())
    for (let i = 0; i < bl.args.length; i++)
      partsSet(bl.args[i], asArray(bl.argtypes[i]))
  const pr = new Pipe(ir)
  for (const bl of pr.blocks()) {
    let stack: Part[] = []
    for (const [v, st] of bl) {
      const ex = st.expr
      const src = st.src
      if (ex.head === 'tuple') {
        env.set(v, ex.body.flatMap(parts))
        pr.delete(v)
      } else if (ex.head === 'ref') {
        env.set(v, [some(parts(ex.body[0])[Number(asValue(ex.body[1]).value) - 1])])
        pr.delete(v)
      } else if (ex instanceof Instr) {
        partsSet(v, asArray(st.type))
        const args = ex.body.flatMap(parts)
        const [ops, state] = stackshuffle(Locals(stack), Locals(args, intersect(live(v), stack)))
        ops.forEach(op => pr.insert(v, pr.stmt(opExpr(op), { src })))
        pr.set(v, new Instr(ex.instr, []))
        stack = [...state.stack.slice(0, state.stack.length - args.length), ...parts(v)]
      } else if (ex instanceof Branch) {
        if (ex.isreturn()) {
          const result = ex.args[0]
          const parttype = (p: Part): wasm.ValueType => {
            if (isValue(p)) return p.type
            const [x, i] = p
            return asArray(ir.type(x))[i - 1]
          }
          ret = parts(result).map(parttype)
          const [ops, _] = stackshuffle(Locals(stack), Locals(parts(result)))
          ops.forEach(op => pr.insert(v, pr.stmt(opExpr(op), { src })))
          pr.set(v, new Instr(wasm.Return(), []))
          stack = []
        } else if (ex.isunreachable()) {
          pr.set(v, new Instr(wasm.unreachable, []))
        } else {
          const bargs = ex.args.flatMap(parts)
          // TODO in this case the args could be in any order
          let [ops, state] = stackshuffle(Locals(stack), Locals(bargs, intersect(live(v), stack)))
          ops.forEach(op => pr.insert(v, pr.stmt(opExpr(op), { src })))
          const sets = ir.block(ex.target).args.flatMap(a => parts(a)).reverse()
          for (const x of sets)
            pr.insert(v, pr.stmt(opExpr({ kind: 'set', x: asArray(x) }), { src }))
          stack = state.stack.slice(0, state.stack.length - sets.length)
          const cond = ex.isconditional() ? parts(ex.when) : [];
          // We are allowed to leave dead values on the stack when branching, but
          // anything live must be stored in a local.
          [ops, state] = stackshuffle(Locals(stack), Locals(cond, intersect(live(v), stack)), { store: true })
          ops.forEach(op => pr.insert(v, pr.stmt(opExpr(op), { src })))
          pr.set(v, new Instr(wasm.Branch(ex.target, ex.isconditional()), []))
          stack = state.stack.slice(0, state.stack.length - cond.length)
        }
      } else {
        throw new Error(`Unrecognised expression ${ex.head}`)
      }
    }
  }
  const out = pr.finish()
  // We `set` block args above before the block is created, so have to
  // do substitution afterwards.
  for (const [v, st] of out) {
    if (!(['get', 'set', 'tee'].includes(st.expr.head))) continue
    const [x, i] = st.expr.body[0] as any
    out.set(v, expr(st.expr.head, pr.substitute(x), Value.i64(i)))
  }
  return [out, ret]
}

// TODO new liveness / interference analysis so we can reuse slots.
// Will also have to filter redundant moves (due to block args) in that case.
function locals(ir: WIR): [WIR, wasm.ValueType[]] {
  const locals: wasm.ValueType[] = []
  const slots = new HashMap<[number, number], number>()
  const b1 = ir.block(1)
  for (let ai = 0; ai < b1.args.length; ai++)
    asArray(b1.argtypes[ai]).forEach((t, i) => {
      slots.set([b1.args[ai], i + 1], locals.length)
      locals.push(t)
    })
  const slot = (v: number, i: number) => {
    if (slots.has([v, i])) return slots.get([v, i])!
    const s = locals.length
    locals.push(asArray(ir.type(v))[i - 1])
    slots.set([v, i], s)
    return s
  }
  for (const [v, st] of ir) {
    const ex = st.expr
    if (!(['get', 'set', 'tee'].includes(ex.head))) continue
    const [x, i] = [asNumber(ex.body[0]), Number(asValue(ex.body[1]).value)]
    const s = slot(x, i)
    ir.set(v, new Instr(ex.head === 'get' ? wasm.GetLocal(s) : wasm.SetLocal(s, ex.head === 'tee'), []))
  }
  for (const bl of ir.blocks()) bl.bb.args.length = 0
  return [ir, locals]
}

// When stepping over, debuggers will go to either the next line or the next
// is_stmt, whichever comes first. To avoid stepping to each call twice, we
// find contiguous sequences of instrs with the same source location, and move
// any breakpoint to the top of the sequence.
function shiftbps(ir: WIR): WIR {
  for (const bl of ir.blocks()) {
    let ip: number | undefined = undefined
    let src: Source | undefined = undefined
    for (const [v, st] of bl) {
      const loc = st.src[st.src.length - 1][1]
      if (loc !== src) {
        [ip, src] = [v, loc]
      } else if (src && st.bp) {
        const idx = some(ip)
        ir.setStmt(idx, { ...ir.get(idx), bp: true })
        ir.setStmt(v, { ...st, bp: false })
      }
    }
  }
  return ir
}

class Relooping {
  readonly scopes: (wasm.Block | wasm.Loop)[]
  readonly targets: number[]
  constructor(readonly ir: WIR, readonly cfg: CFG) {
    this.scopes = [wasm.Block()]
    this.targets = []
  }
  pushscope(bl: wasm.Block | wasm.Loop, target: number) {
    wasm.instr(this.scopes[this.scopes.length - 1], bl, LineInfo([[this.ir.meta, undefined]]))
    this.scopes.push(bl)
    this.targets.push(target)
  }
  popscope() {
    this.scopes.pop()
    this.targets.pop()
  }
  reloopBlock(i: number) {
    const b = this.ir.block(i)
    for (const [_, st] of b) {
      if (!(st.expr instanceof Instr) || st.expr.body.length)
        throw new Error('Expected wasm Instr')
      const instr = st.expr.instr
      if (instr.kind === 'branch') {
        const idx = this.targets.slice().reverse().findIndex(b => b === instr.level)
        if (idx < 0) throw new Error(`Branch target ${instr.level} not found in scopes`)
        wasm.instr(this.scopes[this.scopes.length - 1], wasm.Branch(idx, instr.cond), LineInfo(st.src, st.bp))
      } else {
        wasm.instr(this.scopes[this.scopes.length - 1], instr, LineInfo(st.src, st.bp))
      }
    }
  }
  reloop(cs: Component) {
    if (typeof cs === 'number') return this.reloopBlock(cs)
    // Insert blocks for forward jumps
    for (let i = cs.length - 1; i >= 1; i--)
      this.pushscope(wasm.Block(), entry(cs[i]))
    for (let i = 0; i < cs.length; i++) {
      // Pop forward jumps to this block
      if (i > 0) this.popscope()
      const ch = cs[i]
      if (Array.isArray(ch)) this.pushscope(wasm.Loop(), entry(ch))
      // Block body
      this.reloop(ch)
      if (Array.isArray(ch)) this.popscope()
    }
  }
}

function reloop(ir: WIR, cfg: CFG): wasm.Block {
  const rl = new Relooping(ir, cfg)
  rl.reloop(components(cfg))
  if (rl.scopes.length !== 1) throw new Error('Scope imbalance after reloop')
  if (rl.targets.length !== 0) throw new Error('Target stack not empty after reloop')
  return rl.scopes[0] as wasm.Block
}

function irfunc(name: string, ir: WIR): Func {
  const cfg = new CFG(ir)
  const params = ir.block(1).argtypes.flatMap(asArray)
  const [ir2, ret] = stack(ir)
  const [ir3, ls] = locals(ir2)
  const ir4 = shiftbps(ir3)
  const localsOnly = ls.slice(params.length)
  const body = reloop(ir4, cfg)
  return wasm.Func(name, wasm.Signature(params, ret), localsOnly, body, ir.meta)
}
