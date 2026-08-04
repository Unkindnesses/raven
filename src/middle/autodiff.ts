import * as types from '../frontend/types.js'
import { Type, tag } from '../frontend/types.js'
import {
  Block, Branch, Expr, Fragment, Pipe, Statement, StmtOpts, Val, expand,
  merge_returns, predecessors,
} from '../utils/ir.js'
import { IRValue, Call, Invoke, MIR, Method, Dispatch, calltarget, xclosure } from '../frontend/modules.js'
import { xlist, xpart } from '../frontend/lower.js'
import { Def } from '../dwarf/index.js'
import { Lowering } from './prim_map.js'
import {
  forward_method, notnil_method, pack_method, packcat_method, part_method,
  pullback_method, reverse_method, set_method, tagcast_method,
} from './primitives.js'
import { asNumber, only, some } from '../utils/map.js'
import { isEqual } from '../utils/isEqual.js'

export { autodiff, autograd, pullback }

type Stmt = Statement<IRValue, Type>

function push(code: Fragment<MIR>, ex: Expr<IRValue>, meta?: StmtOpts<Type>): Val<MIR> {
  return code.push(code.stmt(ex, meta))
}

function call(code: Fragment<MIR>, f: Val<MIR>, args: Val<MIR>[], swap = false, meta?: StmtOpts<Type>): Val<MIR> {
  const xs = push(code, xlist(...args))
  return push(code, new Call(f, xs, swap), meta)
}

function invoke(code: Fragment<MIR>, method: Method, args: Val<MIR>[], meta?: StmtOpts<Type>): Val<MIR> {
  return push(code, new Invoke(method, args), meta)
}

// Activity analysis

type Edge = [number, number]

function dataflow(source: MIR): Edge[] {
  const edges: Edge[] = []
  for (const bl of source.blocks())
    for (const [v, st] of bl) {
      const ex = st.expr
      if (ex instanceof Branch) {
        if (ex.target === 0) continue
        const args = source.block(ex.target).args
        ex.args.forEach((x, i) => { if (typeof x === 'number') edges.push([x, args[i]]) })
      } else for (const x of ex.body)
        if (typeof x === 'number') edges.push([x, v])
    }
  return edges
}

function reachable(edges: Edge[], roots: number[]): Set<number> {
  const next = new Map<number, number[]>()
  for (const [x, y] of edges) next.set(x, [...next.get(x) ?? [], y])
  const seen = new Set(roots)
  const queue = [...seen]
  while (queue.length > 0) {
    const x = some(queue.pop())
    for (const y of next.get(x) ?? [])
      if (!seen.has(y)) { seen.add(y); queue.push(y) }
  }
  return seen
}

function results(source: MIR): number[] {
  const out: number[] = []
  for (const [, st] of source)
    if (st.expr instanceof Branch && st.expr.isreturn() && typeof st.expr.args[0] === 'number')
      out.push(st.expr.args[0])
  return out
}

function differentiable(source: MIR, inputs: number[]): Set<number> {
  const edges = dataflow(source)
  const active = reachable(edges, inputs)
  const used = reachable(edges.map(([x, y]): Edge => [y, x]), results(source))
  return new Set([...active].filter(v => used.has(v)))
}

// Forward

function splitResult(code: Fragment<MIR>, x: Val<MIR>): [Val<MIR>, Val<MIR>] {
  const part = (i: bigint) => push(code, xpart(x, Type(i)))
  return [part(1n), part(2n)]
}

type Gradient = (d: Diff, args: Val<MIR>[]) => Val<MIR>

let forwardPrimitives: Map<bigint, Gradient> | undefined

function primitiveGradients(): Map<bigint, Gradient> {
  const gs = new Map<bigint, Gradient>()

  gs.set(pack_method.id, (d, [x]) =>
    invoke(d.code, pack_method, [d.tangent(x)]))

  gs.set(packcat_method.id, (d, xs) =>
    invoke(d.code, packcat_method, xs.map(x => d.tangent(x))))

  gs.set(part_method.id, (d, [xs, i]) =>
    invoke(d.code, part_method, [d.tangent(xs), d.primal(i)]))

  gs.set(notnil_method.id, (d, [x]) =>
    invoke(d.code, notnil_method, [d.tangent(x)]))

  gs.set(tagcast_method.id, (d, [x, T]) =>
    invoke(d.code, tagcast_method, [d.tangent(x), d.primal(T)]))

  return gs
}

function forwardPrimitive(ex: Expr<IRValue>): Gradient | undefined {
  forwardPrimitives ??= primitiveGradients()
  if (!(ex instanceof Invoke)) return
  return ex.method.isSig ? undefined : forwardPrimitives.get(ex.method.id)
}

class Diff {
  readonly code: MIR
  private readonly primals = new Map<number, Val<MIR>>()
  private readonly tangents = new Map<number, Val<MIR>>()
  private readonly needed: Set<number>
  constructor(readonly source: MIR, tangents: number[]) {
    this.code = MIR(Def(`${source.meta.name} (diff)`))
    const args = source.block(1).args
    args.forEach((a, i) => this.primals.set(a, this.code.argument()))
    tangents.forEach((i, j) => this.tangents.set(args[i], this.code.argument()))
    this.needed = differentiable(source, tangents.map(i => args[i]))
  }

  primal(x: Val<MIR>): Val<MIR> {
    return typeof x === 'number' ? some(this.primals.get(x)) : x
  }

  tangent(x: Val<MIR>): Val<MIR> {
    if (typeof x === 'number' && this.tangents.has(x)) return some(this.tangents.get(x))
    return call(this.code, tag('common.autodiff/tangent'), [this.primal(x)])
  }

  private derivative(st: Stmt): [Val<MIR>, Val<MIR>] {
    const ex = st.expr
    if (ex.head === 'pack') {
      return [
        push(this.code, ex.map(x => this.primal(x)), st),
        push(this.code, ex.map(x => this.tangent(x))),
      ]
    }
    if (ex instanceof Invoke) {
      const grad = forwardPrimitive(ex)
      if (grad) {
        const primal = invoke(this.code, ex.method, ex.body.map(x => this.primal(x)), st)
        return [primal, grad(this, ex.body)]
      }
      const result = invoke(this.code, forward_method.wrap(ex.method), [
        ...ex.body.map(x => this.primal(x)), ...ex.body.map(x => this.tangent(x)),
      ], st)
      return splitResult(this.code, result)
    }
    if (ex instanceof Call) {
      const [f, xs] = ex.body
      const args = [this.primal(f), this.primal(xs), this.tangent(xs)]
      const result = ex.swap
        ? invoke(this.code, forward_method.param(Type(true)), args, st)
        : call(this.code, tag('common.core/forward'), args, false, st)
      return splitResult(this.code, result)
    }
    throw new Error(`forward does not support ${ex.head})}`)
  }

  run(): MIR {
    const ir = this.code
    for (const bl of this.source.blocks()) {
      if (bl.id > 0) {
        ir.newBlock()
        for (const a of bl.args)
          this.primals.set(a, ir.block().argument())
        for (const a of bl.args)
          if (this.needed.has(a)) this.tangents.set(a, ir.block().argument())
      }
      for (const [v, st] of bl) {
        const ex = st.expr
        if (ex instanceof Branch) {
          if (ex.isunreachable()) {
            ir.push(st)
          } else if (ex.isreturn()) {
            const x = this.primal(ex.args[0])
            const dx = this.tangent(ex.args[0])
            const result = push(ir, xlist(x, dx))
            ir.return(result, st)
          } else {
            const args = this.source.block(ex.target).args
            push(ir, new Branch(ex.target, [
              ...ex.args.map(x => this.primal(x)),
              ...ex.args.filter((_, i) => this.needed.has(args[i])).map(x => this.tangent(x)),
            ], ex.when === undefined ? undefined : this.primal(ex.when)), st)
          }
        } else if (this.needed.has(v)) {
          const [x, dx] = this.derivative(st)
          this.primals.set(v, x)
          this.tangents.set(v, dx)
        } else {
          this.primals.set(v, push(ir, ex.map(x => this.primal(x)), st))
        }
      }
    }
    return ir
  }
}

function diffMethod(code: Lowering, method: Method, Ts: Type[]): MIR {
  if (Ts.length % 2 !== 0) throw new Error('forward expects a tangent for every argument')
  const n = Ts.length / 2
  return new Diff(code.ir(method, ...Ts.slice(0, n)),
    Array.from({ length: n }, (_, i) => i)).run()
}

function diffCall(code: Lowering, Ts: Type[], swaps: boolean): MIR {
  if (Ts.length !== 3) throw new Error('forward expects a function, arguments and tangents')
  const [F, xs] = Ts
  return new Diff(code.ir(new Dispatch(calltarget(F), swaps), F, xs), [1]).run()
}

function autodiff(code: Lowering, diff: Method, ...Ts: Type[]): MIR {
  const method = diff.wrapped
  return method ? diffMethod(code, method, Ts) : diffCall(code, Ts, isEqual(diff.params[0], Type(true)))
}

// Reverse

type Sig = [Method | undefined, Type[], boolean]

type RuleContext = {
  ir: Fragment<MIR>
  alpha: (x: Val<MIR>) => Val<MIR>
  src: Stmt['src']
}

type Rule = (cx: RuleContext, d: Val<MIR>, args: Val<MIR>[]) => (Val<MIR> | undefined)[]

let reversePrimitives: Map<bigint, Rule> | undefined

function zero(ir: Fragment<MIR>, x: Val<MIR>): Val<MIR> {
  return call(ir, tag('common.autodiff/tangent'), [x], false)
}

function primitiveRules(): Map<bigint, Rule> {
  const rs = new Map<bigint, Rule>()
  rs.set(part_method.id, ({ ir, alpha, src }, d, [xs, i]) => {
    const dxs = invoke(ir, set_method, [zero(ir, alpha(xs)), alpha(i), d], { src })
    return [dxs, undefined]
  })
  rs.set(notnil_method.id, (_, d) => [d])
  rs.set(tagcast_method.id, (_, d) => [d, undefined])
  return rs
}

function reversePrimitive(method: Method): Rule | undefined {
  reversePrimitives ??= primitiveRules()
  return method.isSig ? undefined : reversePrimitives.get(method.id)
}

type Primal = {
  ir: MIR
  pr: MIR
  values: Map<number, Val<MIR>>
  pullbacks: Map<number, Val<MIR>>
  branches: Map<number, number>
  needed: Set<number>
}

function primal(ir: MIR, needed: Set<number>) {
  const pr = new Pipe(ir)
  const pullbacks = new Map<number, Val<MIR>>()
  for (const [v, st] of pr) {
    const ex = st.expr
    if (!needed.has(v)) continue
    if (ex instanceof Invoke) {
      if (reversePrimitive(ex.method)) continue
      pr.delete(v)
      const result = invoke(pr, reverse_method.wrap(ex.method), ex.body, st)
      pr.replace(v, push(pr, xpart(result, Type(1n))))
      pullbacks.set(v, pr.substitute(push(pr, xpart(result, Type(2n)))))
    } else if (ex instanceof Call) {
      const [f, xs] = ex.body
      pr.delete(v)
      const result = ex.swap
        ? invoke(pr, reverse_method.param(Type(true)), [f, xs], st)
        : push(pr, new Call(tag('common.core/reverse'),
          invoke(pr, packcat_method, [push(pr, xlist(push(pr, xlist(f)), xs))])), st)
      pr.replace(v, push(pr, xpart(result, Type(1n))))
      pullbacks.set(v, pr.substitute(push(pr, xpart(result, Type(2n)))))
    }
  }
  return [pr.finish(), pr.map, pullbacks] as const
}

// TODO byte branch ids
function record_branches(ir: MIR): Map<number, number> {
  const branches = new Map<number, number>()
  for (const bl of ir.blocks()) {
    const preds = predecessors(bl)
    if (preds.length < 2) continue
    branches.set(bl.id + 1, bl.argument())
    preds.forEach((p, i) => {
      for (const br of p.branches())
        if (br.target === bl.id + 1) br.args.push(types.int64(i + 1))
    })
  }
  return branches
}

function Primal(ir: MIR): Primal {
  ir = expand(merge_returns(ir))
  const needed = differentiable(ir, ir.block(1).args)
  const [pr, values, pullbacks] = primal(ir, needed)
  const branches = record_branches(pr)
  return { ir, pr, values, pullbacks, branches, needed }
}

class Alpha extends Expr<IRValue> {
  constructor(readonly value: number) { super('alpha') }
  map(_: (x: Val<MIR>) => Val<MIR>): Alpha { return this }
  show(_: (x: Val<MIR>) => string): string { return `alpha %${this.value}` }
}

function sig(ir: MIR, needed: Set<number>): number[][] {
  return Array.from(ir.blocks(), bl =>
    [...new Set(bl.branches().flatMap(br => br.args)
      .filter((x): x is number => typeof x === 'number' && needed.has(x)))])
}

function nilable(bl: Block<MIR>, x: number): boolean {
  return bl.branches().some(br => !br.args.includes(x))
}

type Adjoint = {
  primal: MIR
  adjoint: MIR
  order: number[]
}

function xaccum(ir: Fragment<MIR>, xs: Val<MIR>[]): Val<MIR> {
  return xs.length == 0 ? types.nil :
    xs.length === 1 ? xs[0] :
      call(ir, tag('common.autodiff/accum'), xs, false)
}

function branchTo(from: Block<MIR>, to: Block<MIR>): Branch<IRValue> {
  return only(from.branches().filter(br => br.target === to.id + 1))
}

function blockorder(ir: MIR): number[] {
  const seen = new Set<number>()
  const order: number[] = []
  const visit = (b: number) => {
    if (seen.has(b)) return
    seen.add(b)
    for (const p of predecessors(ir.block(b))) visit(p.id + 1)
    order.push(b)
  }
  visit(ir.block().id + 1)
  return order.reverse()
}

function adjointcfg(pr: Primal) {
  const ir = MIR(Def(`${pr.ir.meta.name} (pullback)`))
  const order = blockorder(pr.ir)
  const sigs = sig(pr.ir, pr.needed)
  for (const [i, b] of order.entries()) {
    const bl = pr.ir.block(b)
    const rb = i === 0 ? ir.block(1) : ir.newBlock()
    const preds = predecessors(bl)
    const conds = preds.slice(0, -1).map((_, j) =>
      call(rb, tag('common/=='), [push(rb, new Alpha(asNumber(some(pr.branches.get(b))))), types.int64(j + 1)]))
    preds.forEach((p, j) => rb.branch(order.indexOf(p.id + 1) + 1, [], { when: conds[j] }))
    if (b !== 1 && preds.length === 0) rb.unreachable()
    if (i === 0) rb.argument()
    else for (const _ of sigs[b - 1]) rb.argument()
  }
  return [ir, order, sigs] as const
}

function adjoint(pr: Primal, method: boolean): Adjoint {
  const [ir, order, sigs] = adjointcfg(pr)
  for (const b of order) {
    const bl = pr.ir.block(b)
    const rb = ir.block(order.indexOf(b) + 1)
    const alpha = (x: Val<MIR>): Val<MIR> =>
      typeof x !== 'number' ? x : push(rb, new Alpha(asNumber(pr.values.get(x))))
    const adjoints = new Map<number, Val<MIR>[]>()
    const accum = (x: Val<MIR>, d: Val<MIR>) => {
      if (typeof x === 'number') adjoints.set(x, [...(adjoints.get(x) ?? []), d])
    }
    const grad = (x: number) => {
      adjoints.set(x, [xaccum(rb, adjoints.get(x) ?? [zero(rb, alpha(x))])])
      return adjoints.get(x)![0]
    }
    // Backprop through (successor) branch arguments
    sigs[b - 1].forEach((x, i) => {
      if (nilable(bl, x)) accum(x, zero(rb, alpha(x)))
      accum(x, rb.args[i])
    })
    // Backprop through statements
    for (const [v, st] of [...bl].reverse()) {
      if (st.expr instanceof Branch || !pr.needed.has(v)) continue
      const ex = st.expr
      if (ex.head === 'pack') {
        ex.body.forEach((x, i) => typeof x === 'number' && accum(x, push(rb, xpart(grad(v), Type(BigInt(i))))))
      } else if (ex instanceof Invoke) {
        if (reversePrimitive(ex.method)) {
          const cx: RuleContext = { ir: rb, alpha, src: st.src }
          reversePrimitive(ex.method)!(cx, grad(v), ex.body).forEach((dx, i) => {
            if (dx !== undefined) accum(ex.body[i], dx)
          })
        } else {
          const pb = push(rb, new Alpha(asNumber(pr.pullbacks.get(v))))
          const ds = call(rb, pb, [grad(v)], false, st)
          ex.body.forEach((x, i) => accum(x, push(rb, xpart(ds, Type(BigInt(i + 1))))))
        }
      } else if (ex instanceof Call) {
        const pb = push(rb, new Alpha(asNumber(pr.pullbacks.get(v))))
        const ds = call(rb, pb, [grad(v)], false, st)
        accum(ex.body[1], ds)
      } else throw new Error(`reverse does not support ${ex.head}`)
    }
    if (b === 1) {
      // Backprop function arguments
      rb.return(method ? push(rb, xlist(...bl.args.map(a => grad(a)))) : grad(bl.args[1]))
    } else {
      // Backprop through (predecessor) branch arguments
      for (const [v, st] of rb) {
        const br = st.expr
        if (!(br instanceof Branch) || br.target === 0) continue
        const pred = order[br.target - 1]
        const inputs = branchTo(pr.ir.block(pred), bl).args
        const args = sigs[pred - 1].map(x =>
          xaccum(rb, inputs.flatMap((a, i) => a === x ? [grad(bl.args[i])] : [])))
        ir.set(v, new Branch(br.target, args, br.when))
      }
    }
  }
  return { primal: pr.pr, adjoint: ir, order }
}

// Emit

function alphaUses(ir: MIR | Block<MIR>): Set<number> {
  return new Set([...ir].flatMap(([, st]) => st.expr instanceof Alpha ? [st.expr.value] : []))
}

function pullbackMethod([meth, Ts, swaps]: Sig): Method {
  return pullback_method.wrap(meth).param(Type(swaps), ...Ts)
}

function forwardStacks(adj: Adjoint, sig: Sig): [MIR, number[]] {
  const pr = new Pipe(adj.primal)
  const alphas = [...alphaUses(adj.adjoint)]
  for (const bl of pr.blocks()) {
    const stacks = alphas.map(() => bl.id === 1 ? push(pr, xlist()) : bl.argument())
    const body = [...bl]
    const i = adj.order.indexOf(bl.id)
    if (i >= 0) for (const alpha of alphaUses(adj.adjoint.block(i + 1))) {
      const i = alphas.indexOf(alpha)
      stacks[i] = call(pr, tag('common/append'), [stacks[i], alpha])
    }
    for (const [v, st] of body) {
      const ex = st.expr
      if (ex instanceof Branch && ex.isreturn()) {
        const state = push(pr, xlist(...stacks))
        const back = push(pr, xclosure(pullbackMethod(sig), state))
        const result = push(pr, xlist(ex.args[0], back))
        pr.set(v, Branch.return(result))
      } else if (ex instanceof Branch && ex.target > 0)
        pr.set(v, new Branch(ex.target, [...ex.args, ...stacks], ex.when))
    }
  }
  return [pr.finish(), alphas]
}

function reverseStacks(adj: Adjoint, alphas: number[]): MIR {
  const pr = new Pipe(adj.adjoint)
  const state = pr.argument()
  for (const bl of pr.blocks()) {
    const stacks = alphas.map((_, i) => bl.id === 1 ?
      push(pr, xpart(state, Type(BigInt(i + 1)))) :
      bl.argument())
    const loaded = new Map<number, Val<MIR>>()
    for (const alpha of alphaUses(adj.adjoint.block(bl.id))) {
      const i = alphas.indexOf(alpha)
      const result = call(pr, tag('common.list/pop'), [stacks[i]], true)
      loaded.set(alpha, push(pr, xpart(result, Type(1n))))
      stacks[i] = push(pr, xpart(result, Type(3n)))
    }
    for (const [v, st] of bl) {
      const ex = st.expr
      if (ex instanceof Alpha) {
        pr.replace(v, some(loaded.get(ex.value)))
      } else if (ex instanceof Branch && ex.target > 0)
        pr.set(v, new Branch(ex.target, [...ex.args, ...stacks], ex.when))
    }
  }
  return pr.finish()
}

function stacks(adj: Adjoint, sig: Sig): { primal: MIR, back: MIR } {
  const [primal, saved] = forwardStacks(adj, sig)
  return { primal, back: reverseStacks(adj, saved) }
}

function reverse(code: Lowering, [meth, Ts, swaps]: Sig): { primal: MIR, back: MIR } {
  if (!meth && Ts.length !== 2) throw new Error('reverse expects a function and its arguments')
  const source = meth
    ? code.ir(meth, ...Ts)
    : code.ir(new Dispatch(calltarget(Ts[0]), swaps), ...Ts)
  const pr = Primal(source)
  return stacks(adjoint(pr, !!meth), [meth, Ts, swaps])
}

function autograd(code: Lowering, diff: Method, ...Ts: Type[]): MIR {
  const sig: Sig = [diff.wrapped, Ts, isEqual(diff.params[0], Type(true))]
  const { primal, back } = reverse(code, sig)
  code.define(pullbackMethod(sig), back)
  return primal
}

function pullback(_: Lowering, diff: Method, ...__: Type[]): MIR {
  throw new Error(`Pullback method was not generated: ${diff}`)
}
