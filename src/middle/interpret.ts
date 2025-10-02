import * as types from '../frontend/types'
import { Tag, Type } from '../frontend/types'
import { Method, Definitions, Binding, IRValue, WIntrinsic, MIR } from '../frontend/modules'
import { IR, unreachable, Branch } from '../utils/ir'
import { CycleCache } from '../utils/cache'
import { partial_match } from './patterns'
import { wasmPartials } from '../backend/wasm'
import { invoke_method } from './primitives'
import isEqual from 'lodash/isEqual'
import { some } from '../utils/map'

export { Interpreter, interpret, interpreter }

type Func = Tag | Method

class Interpreter {
  readonly results: { get: (k: [Func, ...Type[]]) => Type | undefined }

  constructor(readonly defs: Definitions, results?: { get: (k: [Func, ...Type[]]) => Type | undefined }) {
    if (results) {
      this.results = results
    } else {
      this.results = new CycleCache<[Func, ...Type[]], Type | undefined>(_ => undefined, (self, sig) => {
        const int = new Interpreter(defs, self)
        return interpret(int, ...sig)
      })
    }
  }

  get(func: Func, args: Type[]): Type | undefined { return this.results.get([func, ...args]) }
}

function interpret(int: Interpreter, x: Func, ...args: Type[]): Type | undefined {
  if (x instanceof IR) return interpretIR(int, x, ...args)
  if (x instanceof Method) return interpretMethod(int, x, ...args)
  if (args.length !== 1) throw new Error(`Expected 1 argument, got ${args.length}`)
  return interpretFunc(int, x, args[0])
}

function interpretIR(int: Interpreter, ir: MIR, ...args: Type[]): Type | undefined {
  const env = new Map<number, Type>()
  const resolve = (x: number | IRValue): IRValue => {
    if (typeof x === 'number') return some(env.get(x))
    if (x instanceof Binding) return int.defs.resolve_static(x)
    return x
  }
  for (const [v, x] of ir.block(1).args.map((v, i) => [v, args[i]] as const)) env.set(v, x)
  let bl = 1
  while (true) {
    for (const [v, st] of ir.block(bl)) {
      const xs = st.expr.body.map(resolve)
      if (xs.includes(unreachable)) return
      if (st.expr.head === 'call') {
        const op = xs[0]
        if (op instanceof WIntrinsic) {
          if (!wasmPartials.has(op.name)) return
          const args = xs.slice(1).map(x => types.asType(x))
          const allValues = args.every(types.isValue)
          env.set(v, allValues ? wasmPartials.get(op.name)!(...args) : types.asType(st.type))
        } else {
          if (!(op instanceof Tag || op instanceof Method)) throw new Error(`Expected function or method, got ${op}`)
          const result = int.get(op, xs.slice(1).map(x => types.asType(x)))
          if (result === undefined) return
          env.set(v, result)
        }
      } else if (st.expr.head === 'pack') {
        env.set(v, types.pack(...xs.map(x => types.asType(x))))
      } else if (st.expr instanceof Branch) {
        if (st.expr.isreturn()) return types.asType(xs[0])
        if (st.expr.isunreachable()) return
        if (st.expr.isconditional()) {
          const cond = types.asType(xs[0])
          if (!types.issubset(cond, types.bool())) throw new Error(`Expected boolean condition`)
          if (!types.isValue(cond)) return
          if (!isEqual(cond, types.bool(true))) continue
        }
        bl = st.expr.target
        let as = st.expr.args.map(x => types.asType(resolve(x)))
        ir.block(st.expr.target).args.forEach((arg, i) => env.set(arg, as[i]))
        break
      } else if (st.expr.head === 'tuple') {
        const T = types.asType(st.type)
        if (!types.isValue(T)) throw new Error('assert isvalue(st.type)')
        env.set(v, T)
      } else if (st.expr.head === 'set') {
        return
      } else {
        throw new Error(`Unknown expr type ${st.expr.head}`)
      }
    }
  }
}

function interpretMethod(int: Interpreter, meth: Method, ...args: Type[]): Type | undefined {
  if (meth === invoke_method) return
  if (meth.func instanceof IR) return interpretIR(int, meth.func, ...args)
  const result = meth.func(...args)
  return result === unreachable ? undefined : result
}

function interpretFunc(int: Interpreter, func: Tag, args: Type): Type | undefined {
  const methods = int.defs.methods.get(func)
  for (const meth of methods.slice().reverse()) {
    const m = partial_match(int, meth.sig.pattern, args)
    if (m === null) continue
    if (m === undefined) return
    const as = meth.sig.args.map(a => m.get(a)![0])
    const result = interpret(int, meth, ...as)
    if (result === undefined) return
    return meth.sig.swap.size === 0 ? types.list(result) : result
  }
}

function interpreter(defs: Definitions): Interpreter { return new Interpreter(defs) }
