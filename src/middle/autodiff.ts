import { Type, tag } from '../frontend/types.js'
import {
  Branch, Expr, Fragment, Statement, StmtOpts, Val,
} from '../utils/ir.js'
import { IRValue, Call, Invoke, MIR, Method, Dispatch, calltarget } from '../frontend/modules.js'
import { xlist, xpart } from '../frontend/lower.js'
import { Def } from '../dwarf/index.js'
import { Lowering } from './prim_map.js'
import {
  forward_method, notnil_method, pack_method, packcat_method, part_method, tagcast_method,
} from './primitives.js'
import { some } from '../utils/map.js'
import { isEqual } from '../utils/isEqual.js'

export { autodiff }

type Stmt = Statement<IRValue, Type>

function push(code: Fragment<MIR>, ex: Expr<IRValue>, meta?: StmtOpts<Type>): Val<MIR> {
  return code.push(code.stmt(ex, meta))
}

function call(code: Fragment<MIR>, f: Val<MIR>, args: Val<MIR>[], swap = false, meta?: StmtOpts<Type>): Val<MIR> {
  const xs = push(code, xlist<IRValue>(...args))
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
            const result = push(ir, xlist<IRValue>(x, dx))
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
