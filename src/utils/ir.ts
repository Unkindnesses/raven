import { asNumber } from "../utils/map"
import { HashMap, some } from "./map"
import { WorkQueue } from "./fixpoint"
import { identity, isEqual } from "lodash"

export {
  Anno, Unreachable, unreachable, asAnno, Source, Slot, slot, Expr, expr,
  Statement, stmt, IR, Block, Pipe, Branch, CFG, Component, components, entry, rename,
  getIndent, withIndent, Val, Fragment, asIR,
  liveness_after, liveness,
  predecessors, fuseblocks, expand, prune, ssa, renumber
}

type Unreachable = '⊥'
const unreachable: Unreachable = '⊥'

type Anno<T> = T | Unreachable

function asAnno<T>(asT: (x: any) => T, x: any): Anno<T> {
  return x === unreachable ? unreachable : asT(x)
}

interface Source {
  readonly file: string
  readonly line: number
  readonly col: number
}

class Slot {
  constructor(readonly name: string) { }
  toString() { return `@${this.name}` }
}

const slot = (name: string) => new Slot(name)

class Expr<T> {
  constructor(readonly head: string, private readonly _body: (T | number)[] = []) { }
  get body(): (T | number)[] { return this._body }
  map(f: (x: T | number) => T | number): Expr<T> {
    return new Expr(this.head, this.body.map(f))
  }
  show(pr: (x: T) => string): string {
    return this.body.length ? `${this.head} ${this.body.map(x => showVar(x, pr)).join(', ')}` : this.head
  }
}

function expr<T>(head: string, ...body: (T | number)[]): Expr<T> {
  return new Expr(head, body)
}

class Branch<T> extends Expr<T> {
  constructor(readonly target: number, readonly args: (T | number)[] = [], readonly when?: number) {
    super('branch')
  }
  get body() {
    return this.when === undefined
      ? this.args
      : [this.when, ...this.args]
  }
  static return<T>(arg: T | number) { return new Branch<T>(0, [arg]) }
  static unreachable<T>() { return new Branch<T>(0, []) }
  isconditional(): this is { when: number } { return this.when !== undefined }
  isreturn() { return this.target === 0 && this.args.length === 1 }
  isunreachable() { return this.target === 0 && this.args.length === 0 }
  map(f: (x: T | number) => T | number): Branch<T> {
    const when = this.when === undefined ? undefined : asNumber(f(this.when))
    return new Branch(this.target, this.args.map(f), when)
  }
  show(pr: (x: T) => string): string {
    if (this.isunreachable()) return "unreachable"
    if (this.isreturn()) return `return ${showVar(this.args[0], pr)}`
    let result = `br ${this.target}`
    if (this.args.length > 0) result += ` (${this.args.map(x => showVar(x, pr)).join(', ')})`
    if (this.when !== undefined) result += ` if ${showVar(this.when, pr)}`
    return result
  }
}

interface Statement<T, A> {
  readonly expr: Expr<T>
  readonly type: Anno<A>
  readonly src: Source | undefined
  readonly bp: boolean
}

function stmt<T, A>(expr: Expr<T>, { type, src, bp }: { type?: Anno<A>, src?: Source, bp?: boolean } = {}): Statement<T, A> {
  bp ??= false
  type ??= unreachable
  return { expr, type, src, bp }
}

interface BasicBlock<T, A> {
  readonly stmts: [number, Statement<T, A>][]
  readonly args: [number, Anno<A>][]
}

type T<I> = I extends IR<infer T, any, any> ? T : never
type A<I> = I extends IR<any, infer A, any> ? A : never
type M<I> = I extends IR<any, any, infer M> ? M : never

type Val<I extends IR<any, any, any>> = T<I> | number

// TODO perhaps better to define an interface
type Fragment<I extends IR<any, any, any>> = I | Block<I> | Pipe<I>

class IR<T, A, M> {
  readonly _defs: [number, number][] = []
  readonly _blocks: BasicBlock<T, A>[] = [{ stmts: [], args: [] }]
  constructor(readonly meta: M,
    readonly show: (x: any) => string = (x: any) => `${x} `,
    readonly typeOf: (x: T) => T | Anno<A> = (x: T) => x) { }

  get length() { return this._defs.reduce((acc, [_, i]) => acc + (i > 0 ? 1 : 0), 0) }

  blockIdx(v: number): [number, number] {
    const [b, i] = this._defs[v - 1]
    if (b < 0) throw new Error(`Variable ${v} not found`)
    return [b, i]
  }
  blockOf(v: number): Block<IR<T, A, M>> {
    return this.block(this.blockIdx(v)[0] + 1)
  }
  get(v: number): Statement<T, A> {
    const [b, i] = this.blockIdx(v)
    if (i < 0) throw new Error(`Variable ${v} is an argument`)
    return this._blocks[b].stmts[i][1]
  }
  type(x: T | number): T | Anno<A> {
    if (typeof x !== 'number') return this.typeOf(x)
    const [b, i] = this.blockIdx(x)
    if (i >= 0) return this._blocks[b].stmts[i][1].type
    return this._blocks[b].args[-i - 1][1]
  }
  setStmt(v: number, stmt: Statement<T, A>) {
    const [b, i] = this.blockIdx(v)
    if (i < 0) throw new Error(`Variable ${v} not found`)
    this._blocks[b].stmts[i][1] = stmt
  }
  set(v: number, ex: Expr<T>) {
    this.setStmt(v, { ...this.get(v), expr: ex })
  }
  setType(v: number, type: Anno<A>) {
    this.setStmt(v, { ...this.get(v), type })
  }
  has(x: number): boolean {
    if (x < 1 || x > this._defs.length) return false
    const [_, i] = this._defs[x - 1]
    return i >= 0
  }
  block(i: number = this._blocks.length): Block<IR<T, A, M>> {
    if (i < 1 || i > this._blocks.length) throw new Error(`Block index out of bounds: ${i} `)
    return new Block(this, i - 1)
  }
  *blocks(): Generator<Block<IR<T, A, M>>> {
    for (let i = 1; i <= this._blocks.length; i++) yield this.block(i)
  }
  *[Symbol.iterator](): Generator<[number, Statement<T, A>]> {
    for (let v = 1; v <= this._defs.length; v++) {
      if (this.has(v)) yield [v, this.get(v)]
    }
  }
  newBlock(): Block<IR<T, A, M>> {
    this._blocks.push({ stmts: [], args: [] })
    return this.block()
  }
  argument(type: Anno<A>): number {
    return this.block(1).argument(type)
  }
  push(x: Statement<T, A>): number {
    return this.block().push(x)
  }
  branch(target: number | Block<IR<T, A, M>>, args: (T | number)[] = [],
    { when, src, bp }: { when?: number, src?: Source, bp?: boolean } = {}): number {
    return this.block().branch(target, args, { when, src, bp })
  }
  return(arg: T | number, opts: { src?: Source, bp?: boolean } = {}) { this.block().return(arg, opts) }
  delete(v: number) {
    this._defs[v - 1] = [-1, -1]
  }
  toString(): string { return showIR(this) }
  get blockCount(): number { return this._blocks.length }
  clone(): IR<T, A, M> {
    const y = new IR<T, A, M>(this.meta, this.show, this.typeOf)
    y._blocks.length = 0
    for (const bb of this._blocks)
      y._blocks.push({
        stmts: bb.stmts.map(([v, st]) => [v, { ...st, expr: st.expr.map(identity) }]),
        args: bb.args.map(([a, t]) => [a, t])
      })
    y._defs.length = 0
    for (const [b, i] of this._defs) y._defs.push([b, i])
    return y
  }
  deleteBlock(i: number): void {
    if (i < 1 || i > this._blocks.length) throw new Error(`Block index out of bounds: ${i} `)
    const idx = i - 1
    this._blocks.splice(idx, 1)
    if (i !== this._blocks.length + 1) {
      for (const b of this.blocks()) for (const [v, st] of b)
        if (st.expr instanceof Branch && st.expr.target >= i)
          this.setStmt(v, { ...st, expr: new Branch(st.expr.target - 1, st.expr.args, st.expr.when) })
    }
    for (let v = 1; v <= this._defs.length; v++) {
      const [b, pos] = this._defs[v - 1]
      if (b === idx) this._defs[v - 1] = [-1, -1]
      else if (b > idx) this._defs[v - 1] = [b - 1, pos]
    }
  }
}

function asIR(x: any): IR<any, any, any> {
  if (x instanceof IR) return x
  throw new Error(`Expected IR, got ${typeof x}`)
}

class Block<I extends IR<any, any, any>> {
  constructor(readonly ir: I, readonly id: number) { }
  get bb() { return this.ir._blocks[this.id] }
  get args() { return this.bb.args.map(([arg, _]) => arg) }
  get argtypes(): Anno<A<I>>[] { return this.bb.args.map(([_, type]) => type) }
  get length() { let n = 0; for (const _ of this) n++; return n }
  type(x: T<I> | number): T<I> | Anno<A<I>> { return this.ir.type(x) }

  branchStart(): number {
    let i = this.bb.stmts.length
    while (i > 0 && this.bb.stmts[i - 1][1].expr.head === 'branch') i--
    return i
  }

  insert(pos: number, x: Statement<T<I>, A<I>>): number {
    const v = this.ir._defs.length + 1
    this.bb.stmts.splice(pos, 0, [v, x])
    for (let i = pos + 1; i < this.bb.stmts.length; i++) {
      const [v, _] = this.bb.stmts[i]
      if (this.ir._defs[v - 1][0] >= 0) this.ir._defs[v - 1] = [this.id, i]
    }
    this.ir._defs.push([this.id, pos])
    return v
  }

  push(x: Statement<T<I>, A<I>>): number {
    if (!(x.expr instanceof Branch)) {
      const bs = this.branchStart()
      if (bs < this.bb.stmts.length) return this.insert(bs, x)
    }
    const v = this.ir._defs.length + 1
    this.bb.stmts.push([v, x])
    this.ir._defs.push([this.id, this.bb.stmts.length - 1])
    return v
  }

  branches(): Branch<T<I>>[] {
    return this.bb.stmts
      .map(([_, stmt]) => stmt.expr)
      .filter(st => st instanceof Branch)
  }

  canbranch() { return this.branches().length === 0 || this.branches().slice(-1)[0].isconditional() }

  argument(type: Anno<A<I>>, { value }: { value?: T<I> | number } = {}): number {
    this.ir._defs.push([this.id, -(this.args.length + 1)])
    const arg = this.ir._defs.length
    this.bb.args.push([arg, type])
    if (value !== undefined)
      for (const bl of this.ir.blocks())
        for (const br of bl.branches())
          if (br.target === this.id + 1)
            br.args.push(value)
    return arg
  }

  branch(target: number | Block<I>, args: (T<I> | number)[] = [],
    { when, src, bp }: { when?: number, src?: Source, bp?: boolean } = {}): number {
    const targetId = typeof target === 'number' ? target : target.id + 1
    return this.push(stmt<T<I>, A<I>>(new Branch(targetId, args, when), { src, bp }))
  }
  return(arg: T<I> | number, opts: { src?: Source, bp?: boolean } = {}) { this.branch(0, [arg], opts) }
  unreachable(opts: { src?: Source, bp?: boolean } = {}) { this.branch(0, [], opts) }

  *[Symbol.iterator](): Generator<[number, Statement<T<I>, A<I>>]> {
    for (const [v, stmt] of this.bb.stmts) if (this.ir.has(v)) yield [v, stmt]
  }

  toString(): string { return showBlock(this) }
}

// Printing

let __indent = 0
function getIndent() { return __indent }
function withIndent<T>(delta: number, f: () => T): T {
  const old = __indent
  __indent += delta
  try { return f() }
  finally { __indent = old }
}

function showVar<T>(x: Anno<T> | number, show: (x: T) => string): string {
  if (typeof x === 'number') return `%${x}`
  if (x === unreachable) return '⊥'
  return show(x)
}

function showBlock<T, A, M>(block: Block<IR<T, A, M>>): string {
  let show = (x: T | Anno<A> | number) => showVar(x, block.ir.show)
  function showLine(src: Source, bp: boolean): string {
    let result = " # "
    const filename = src.file.split('/').pop() || src.file
    result += `${filename}:${src.line}:${src.col}`
    if (bp) result += " 🔴"
    return result
  }
  const tab = "  ".repeat(__indent + 1)
  let result = `${"  ".repeat(__indent)}${block.id + 1}:`
  if (block.args.length > 0)
    result += ` (${block.ir._blocks[block.id].args.map(([arg, type]) => type === unreachable ? `%${arg}` : `%${arg} :: ${show(type)}`).join(', ')})`

  for (const [x, st] of block) {
    result += '\n' + tab
    if (x > 0) result += `%${x} = `
    result += st.expr.show(block.ir.show)
    if (!(st.expr instanceof Branch) && st.type !== unreachable) result += ` :: ${show(st.type)}`
    if (st.src) result += showLine(st.src, st.bp)
  }
  return result
}

function showIR<T, A, M>(ir: IR<T, A, M>): string {
  let result = ir.meta ? `${ir.meta}\n` : ''
  result += ir.block(1).toString()
  for (let i = 2; i <= ir._blocks.length; i++) {
    result += '\n' + ir.block(i).toString()
  }
  return result
}

// IR Passes

function zip<T, U>(a: T[], b: U[]): [T, U][] {
  return a.map((x, i) => [x, b[i]])
}

function successors<T, A, M>(block: Block<IR<T, A, M>>): Block<IR<T, A, M>>[] {
  return block.branches().map(br => br.target).filter(target => target > 0).map(id => block.ir.block(id))
}

function predecessors<T, A, M>(block: Block<IR<T, A, M>>): Block<IR<T, A, M>>[] {
  return Array.from(block.ir.blocks()).filter(b => successors(b).some(s => s.id === block.id))
}

function definitions<T, A, M>(b: Block<IR<T, A, M>>): number[] {
  const out: number[] = []
  for (let i = 1; i <= b.ir._defs.length; i++)
    if (b.ir._defs[i - 1][0] === b.id) out.push(i)
  return out
}

function usages<T, A, M>(b: Block<IR<T, A, M>>): Set<number> {
  const used = new Set<number>()
  for (const [_, st] of b)
    for (const x of st.expr.body)
      if (typeof x === 'number') used.add(x)
  return used
}

function fuseable<T, A, M>(block: Block<IR<T, A, M>>): boolean {
  const preds = predecessors(block)
  return preds.length === 0 || (preds.length === 1 && successors(preds[0]).length === 1)
}

function rename<T, A>(env: Map<number, number | T>, stmt: Statement<T, A>, { fallthrough }: { fallthrough?: boolean } = {}): Statement<T, A> {
  fallthrough ??= false
  const f = (x: T | number) => {
    return typeof x === 'number' && (!fallthrough || env.has(x)) ? some(env.get(x)) : x
  }
  return { ...stmt, expr: stmt.expr.map(f) }
}

function fuseblocks<T, A, M>(ir: IR<T, A, M>): IR<T, A, M> {
  const blocks = Array.from(ir.blocks())
  const newIR = new IR<T, A, M>(ir.meta, ir.show, ir.typeOf)
  const skip = new Set<number>()
  for (let i = 1; i < blocks.length; i++) if (fuseable(blocks[i])) skip.add(blocks[i].id + 1)
  const blockEnv = new Map<number, number>()
  const varEnv = new Map<number, number | T>()
  blockEnv.set(0, 0)
  for (const block of blocks) {
    const blockId = block.id + 1
    if (skip.has(blockId)) continue
    if (blockId !== 1) newIR.newBlock()
    blockEnv.set(blockId, newIR._blocks.length)
    for (const [arg, type] of block.bb.args) {
      const newArg = newIR.block().argument(type)
      varEnv.set(arg, newArg)
    }
    const inlineBlock = (targetBlock: Block<IR<T, A, M>>) => {
      for (const [v, stmt] of targetBlock) {
        if (stmt.expr instanceof Branch && stmt.expr.target > 0 && skip.has(stmt.expr.target)) {
          const targetBlockToInline = blocks[stmt.expr.target - 1]
          for (const [targetArg, sourceValue] of zip(targetBlockToInline.args, stmt.expr.args))
            varEnv.set(targetArg, typeof sourceValue === 'number' ? some(varEnv.get(sourceValue)) : sourceValue)
          inlineBlock(targetBlockToInline)
        } else {
          varEnv.set(v, newIR.block().push(rename(varEnv, stmt)))
        }
      }
    }
    inlineBlock(block)
  }
  for (const [v, stmt] of newIR)
    if (stmt.expr instanceof Branch)
      newIR.setStmt(v, { ...stmt, expr: new Branch(some(blockEnv.get(stmt.expr.target)), stmt.expr.args, stmt.expr.when) })
  return newIR
}

function usecounts<T, A, M>(ir: IR<T, A, M>): Map<number, number> {
  const counts = new Map<number, number>()
  for (const [_, stmt] of ir)
    for (const x of stmt.expr.body)
      if (typeof x === 'number') counts.set(x, (counts.get(x) ?? 0) + 1)
  return counts
}

function deletearg<T, A, M>(b: Block<IR<T, A, M>>, indices: number[]): void {
  for (const i of indices.sort((a, b) => b - a)) {
    const arg = b.args[i]
    b.bb.args.splice(i, 1)
    for (const c of b.ir.blocks())
      for (const br of c.branches())
        if (br.target === b.id + 1) br.args.splice(i, 1)
    b.ir._defs[arg - 1] = [-1, -1]
    for (let j = i; j < b.bb.args.length; j++) {
      const [blk, pos] = b.ir._defs[b.bb.args[j][0] - 1]
      b.ir._defs[b.bb.args[j][0] - 1] = [blk, pos + 1]
    }
  }
}

function branches<T, A, M>(from: Block<IR<T, A, M>>, to: Block<IR<T, A, M>>): Branch<T>[] {
  return from.branches().filter(br => br.target === to.id + 1)
}

function expand<T, A, M>(ir: IR<T, A, M>): IR<T, A, M> {
  const worklist = Array.from(ir.blocks()).map(b => b.id + 1)
  const spats = new Map<number, Map<number, number>>()
  for (const b of ir.blocks()) spats.set(b.id + 1, new Map())
  while (worklist.length > 0) {
    const bid = worklist.pop()!
    const b = ir.block(bid)
    if (b.id === 0) continue
    const defs = new Set(definitions(b))
    const used = usages(b)
    for (const v of used) if (!defs.has(v)) {
      if (spats.get(bid)!.has(v)) continue
      // TODO type hack!
      spats.get(bid)!.set(v, b.argument(ir.type(v) as Anno<A>, { value: v }))
      for (const p of predecessors(b))
        if (!worklist.includes(p.id + 1)) worklist.push(p.id + 1)
    }
    const env = spats.get(bid)!
    for (const [v, st] of b) ir.setStmt(v, rename(env, st, { fallthrough: true }))
  }
  return ir
}

function prune<T, A, M>(ir: IR<T, A, M>): IR<T, A, M> {
  const usages = usecounts(ir)
  const worklist = Array.from(ir.blocks()).map(b => b.id + 1)
  const queue = (blockId: number) => { if (!worklist.includes(blockId)) worklist.push(blockId) }
  const rename = (env: Map<number, number | T>) => {
    for (const b of ir.blocks())
      for (const [v, stmt] of b)
        ir.setStmt(v, {
          ...stmt, expr: stmt.expr.map(x => {
            if (!(typeof x == 'number' && env.has(x))) return x
            successors(b).forEach(succ => queue(succ.id + 1))
            return some(env.get(x))
          })
        })
  }
  while (worklist.length > 0) {
    const blockId = worklist.shift()!
    const b = ir.block(blockId)
    if (b.args.length === 0) continue
    const brs = Array.from(ir.blocks()).flatMap(a => branches(a, b))
    if (brs.length === 0) continue
    // Redundant due to all inputs being the same
    const inputs = b.args.map((arg, i) => [...new Set(brs.map(br => br.args[i]).filter(x => x !== arg))])
    const del = inputs.map((inp, i) => inp.length === 1 ? i : -1).filter(i => i >= 0)
    const renameMap = new Map(del.map(i => [b.args[i], inputs[i][0]]))
    if (del.length > 0) {
      deletearg(b, del) // TODO update usages?
      rename(renameMap)
    }
    // Redundant due to not being used
    const unused = b.args.map((arg, i) => [arg, i])
      .filter(([arg]) => (usages.get(arg) ?? 0) === 0)
      .map(([_, i]) => i)
    if (unused.length > 0) {
      for (const a of predecessors(b)) {
        for (const br of branches(a, b))
          for (const i of unused)
            if (typeof br.args[i] === 'number')
              usages.set(br.args[i], (usages.get(br.args[i]) ?? 0) - 1)
        queue(a.id + 1)
      }
      deletearg(b, unused)
    }
  }
  return ir
}

function ssa<T, A, M>(ir: IR<T, A, M>): IR<T, A, M> {
  let current = 1
  const defs = new Map<number, HashMap<Slot, number | T>>()
  const todo = new Map<number, Map<number, Slot[]>>()
  Array.from(ir.blocks()).forEach(b => {
    defs.set(b.id + 1, new HashMap())
    todo.set(b.id + 1, new Map())
  })
  function reaching(block: Block<IR<T, A, M>>, slot: Slot): number | T {
    const blockId = block.id + 1
    if (defs.get(blockId)!.has(slot)) return some(defs.get(blockId)!.get(slot))
    if (blockId === 1) throw new Error(`undefined ${slot.name}`)
    const arg = block.argument(unreachable)
    defs.get(blockId)!.set(slot, arg)
    for (const pred of predecessors(block)) {
      const predId = pred.id + 1
      if (predId < current) {
        for (const br of pred.branches().filter(b => b.target === blockId))
          br.args.push(reaching(pred, slot))
      } else {
        const predTodo = todo.get(predId)!
        if (!predTodo.has(blockId)) predTodo.set(blockId, [])
        predTodo.get(blockId)!.push(slot)
      }
    }
    return arg
  }
  let resolve = (block: Block<IR<T, A, M>>, x: number | T) =>
    x instanceof Slot ? reaching(block, x) : x
  let rename = (block: Block<IR<T, A, M>>, expr: Expr<T>) =>
    expr.map(x => resolve(block, x))
  for (const block of ir.blocks()) {
    current = block.id + 1
    for (const [v, stmt] of block) {
      const expr = stmt.expr
      if (expr.head === 'set' && expr.body[0] instanceof Slot) {
        const slot = expr.body[0] as Slot
        const value = expr.body[1]
        defs.get(current)!.set(slot, resolve(block, value))
        ir.delete(v)
      } else {
        ir.setStmt(v, { ...stmt, expr: rename(block, expr) })
      }
    }
    for (const [succId, slots] of todo.get(current)!)
      for (const br of block.branches().filter(b => b.target === succId))
        br.args.push(...slots.map(slot => reaching(block, slot)))
  }
  return ir
}

class PipeBlock<I extends IR<any, any, any>> {
  constructor(readonly pipe: Pipe<I>, readonly id: number) {
    if (id > 1) pipe.to.newBlock()
    const block = pipe.from.block(id)
    for (let i = 0; i < block.args.length; i++) {
      const v = pipe.to.block().argument(block.argtypes[i])
      pipe.map.set(block.args[i], v)
    }
  }

  *[Symbol.iterator](): Generator<[number, Statement<T<I>, A<I>>]> {
    for (const [v, stmt] of this.pipe.from.block(this.id)) {
      this.pipe.map.set(v, this.pipe.to.push(this.pipe.substituteStmt(stmt)))
      yield [v, stmt]
    }
  }
}

class Pipe<I extends IR<any, any, any>> {
  from: I
  to: I
  map: Map<number, number | T<I>>
  id: number

  constructor(ir: I) {
    this.from = ir
    this.to = new IR(ir.meta, ir.show, ir.typeOf) as I
    this.map = new Map()
    this.id = 0
  }

  var(): number { return this.id -= 1 }

  substitute(x: T<I> | number): T<I> | number {
    return typeof x === 'number' ? some(this.map.get(x)) : x
  }

  substituteStmt(stmt: Statement<T<I>, A<I>>): Statement<T<I>, A<I>> {
    return { ...stmt, expr: stmt.expr.map(x => this.substitute(x)) }
  }

  push(stmt: Statement<T<I>, A<I>>): number {
    const v = this.var()
    this.map.set(v, this.to.push(this.substituteStmt(stmt)))
    return v
  }

  setStmt(v: number, stmt: Statement<T<I>, A<I>>): void {
    this.to.setStmt(asNumber(this.substitute(v)), this.substituteStmt(stmt))
  }

  set(v: number, ex: Expr<T<I>>): void {
    this.to.set(asNumber(this.substitute(v)), ex.map(x => this.substitute(x)))
  }

  setType(v: number, type: Anno<A<I>>): void {
    this.to.setType(asNumber(this.substitute(v)), type)
  }

  type(x: T<I> | number): T<I> | Anno<A<I>> {
    if (typeof x !== 'number') return this.to.typeOf(x)
    return this.to.type(this.substitute(x))
  }

  replace(v: number, y: number | T<I>): void {
    if (this.map.has(v) && this.map.get(v) !== 0) { // TODO remove
      let x = asNumber(this.substitute(v))
      if (!this.isLastDef(x)) throw new Error(`Replacing used variable %${v}`)
      this.delete(v)
    }
    this.map.set(v, this.substitute(y))
  }

  private isLastDef(v: number): boolean {
    return v === this.to._defs.length && isEqual(
      this.to._defs[v - 1],
      [this.to._blocks.length - 1, this.to._blocks[this.to._blocks.length - 1].stmts.length - 1]
    )
  }

  delete(v: number): void {
    const v2 = asNumber(this.substitute(v))
    // this.map.delete(v)
    this.map.set(v, 0) // TODO remove this
    if (this.isLastDef(v2)) {
      this.to._defs.pop()
      this.to._blocks[this.to._blocks.length - 1].stmts.pop()
    } else this.to.delete(v2) // TODO disable for safety
  }

  insert(v: number, stmt: Statement<T<I>, A<I>>, { after }: { after?: boolean } = {}): number {
    after ??= false
    const v2 = asNumber(this.substitute(v))
    const x = this.substituteStmt(stmt)
    const tmp = this.var()
    if (this.isLastDef(v2)) { // we can make this case efficient by renumbering
      if (after) {
        this.map.set(tmp, this.to.push(x))
      } else {
        this.map.set(v, this.to.push(this.to.get(v2)))
        this.to.setStmt(v2, x)
        this.map.set(tmp, v2)
      }
    } else throw new Error('not supported')
    return tmp
  }

  *blocks(): Generator<PipeBlock<I>> {
    const it = this.from.blocks() as Generator<Block<I>>
    for (const block of it) yield new PipeBlock(this, block.id + 1)
  }

  *[Symbol.iterator](): Generator<[number, Statement<T<I>, A<I>>]> {
    for (const bl of this.blocks())
      for (const [v, st] of bl) yield [v, st]
  }

  finish(): I { return this.to }
}

function renumber<T, A, M>(ir: IR<T, A, M>): IR<T, A, M> {
  const p = new Pipe(ir)
  for (const _ of p) { }
  return p.finish()
}

class CFG {
  edges: Map<number, number[]>
  readonly length: number
  constructor(ir: IR<any, any, any>) {
    this.length = ir._blocks.length
    this.edges = new Map()
    for (const b of ir.blocks()) this.edges.set(b.id + 1, successors(b).map(s => s.id + 1))
  }
}

// SCCs

function strongconnected<T, A, M>(cfg: CFG, blocks: number[]): number[][] {
  const preorder = new Map<number, number>()
  const S: number[] = []
  const P: number[] = []
  let C = 0
  const comps: number[][] = []
  const search = (v: number) => {
    preorder.set(v, ++C)
    S.push(v)
    P.push(v)
    for (const w of some(cfg.edges.get(v))) {
      if (!blocks.includes(w)) continue
      else if (!preorder.has(w)) search(w)
      else if (!comps.some(c => c.includes(w)))
        while (preorder.get(P[P.length - 1])! > preorder.get(w)!) P.pop()
    }
    if (P[P.length - 1] === v) {
      P.pop()
      const comp: number[] = []
      while (true) {
        const x = S.pop()!
        comp.unshift(x)
        if (x === v) break
      }
      comps.unshift(comp)
    }
  }
  for (const b of blocks) if (!preorder.has(b)) search(b)
  return comps
}

type Component = number | Component[]

function entry(x: Component): number { return typeof x === 'number' ? x : entry(x[0]) }

function components<T, A, M>(cfg: CFG, blocks?: number[]): Component {
  const bs = blocks ?? Array.from({ length: cfg.length }, (_, i) => i + 1)
  if (bs.length === 1) {
    const bl = bs[0]
    const self = (cfg.edges.get(bl) ?? []).includes(bl)
    // Singleton components represent one-block loops
    return self ? [bl] : bl
  } else {
    // Assume the first block is the entry.
    const cs = strongconnected(cfg, bs.slice(1))
    return [bs[0], ...cs.map(c => components(cfg, c.sort((a, b) => a - b)))]
  }
}

// Liveness

function liveness_after<T, A, M>(block: Block<IR<T, A, M>>, lv: Map<number, Set<number>>): Set<number> {
  const live = new Set<number>()
  for (const b of successors(block))
    for (const x of some(lv.get(b.id + 1)))
      if (!b.args.includes(x)) live.add(x)
  return live
}

// Variables needed before each block is run, and after each statement is run.
// Block variables include their arguments.
function liveness<T, A, M>(ir: IR<T, A, M>): { stmts: Map<number, Set<number>>, blocks: Map<number, Set<number>> } {
  const stmts = new Map<number, Set<number>>()
  for (const [v, _] of ir) stmts.set(v, new Set())
  const blocks = new Map<number, Set<number>>()
  for (const b of ir.blocks()) blocks.set(b.id + 1, new Set())
  const queue = new WorkQueue<number>()
  for (const b of ir.blocks()) queue.push(b.id + 1)
  while (!queue.empty) {
    const bid = queue.pop()
    const b = ir.block(bid)
    let live = liveness_after(b, blocks)
    for (const [v, st] of Array.from(b).reverse()) {
      const sv = stmts.get(v)!
      for (const x of live) sv.add(x)
      live.delete(v)
      for (const x of st.expr.body) if (typeof x === 'number') live.add(x)
    }
    let changed = false
    const old = blocks.get(bid)!
    for (const x of live) if (!old.has(x)) { old.add(x); changed = true }
    if (changed) for (const p of predecessors(b)) queue.push(p.id + 1)
  }
  return { stmts, blocks }
}
