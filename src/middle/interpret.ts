import * as types from '../frontend/types.js'
import { Tag, Type } from '../frontend/types.js'
import { Method, Definitions, IRValue, MIR, Global, SetGlobal, Invoke, Wasm, Value } from '../frontend/modules.js'
import { IR, unreachable, Branch, asType } from '../utils/ir.js'
import { CycleCache } from '../utils/cache.js'
import { partial_match } from './patterns.js'
import { wasmPartials } from '../backend/wasm.js'
import { invoke_method } from './primitives.js'
import isEqual from 'lodash/isEqual.js'
import { some } from '../utils/map.js'
import { Instruction } from '../wasm/wasm.js'

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
  const resolve = (x: number | IRValue): Type => {
    if (x instanceof Value) throw new Error('unsupported')
    return typeof x === 'number' ? some(env.get(x)) : x
  }
  for (const [v, x] of ir.block(1).args.map((v, i) => [v, args[i]] as const)) env.set(v, x)
  let bl = 1
  while (true) {
    for (const [v, st] of ir.block(bl)) {
      const xs = st.expr.body.map(resolve)
      if (st.expr instanceof Invoke) {
        const result = int.get(st.expr.method, xs)
        if (result === undefined) return
        env.set(v, result)
      } else if (st.expr instanceof Wasm) {
        if (st.expr.isImport()) return
        const instr = st.expr.callee as Instruction
        const op = instr.kind === 'op' ? instr.name : ''
        if (!wasmPartials.has(op)) return
        const args = xs
        const allValues = args.every(types.isValue)
        env.set(v, allValues ? wasmPartials.get(op)!(...args) : asType(st.type))
      } else if (st.expr.head === 'call') {
        const op = xs[0]
        if (!(op instanceof Tag)) throw new Error(`Expected function or method, got ${op}`)
        const result = int.get(op, xs.slice(1))
        if (result === undefined) return
        env.set(v, result)
      } else if (st.expr.head === 'pack') {
        env.set(v, types.pack(...xs))
      } else if (st.expr instanceof Branch) {
        if (st.expr.isreturn()) return xs[0]
        if (st.expr.isunreachable()) return
        if (st.expr.isconditional()) {
          const cond = xs[0]
          if (!types.issubset(cond, types.bool())) throw new Error(`Expected boolean condition`)
          if (!types.isValue(cond)) return
          if (!isEqual(cond, types.bool(true))) continue
        }
        bl = st.expr.target
        let as = st.expr.args.map(x => resolve(x))
        ir.block(st.expr.target).args.forEach((arg, i) => env.set(arg, as[i]))
        break
      } else if (st.expr.head === 'tuple') {
        const T = asType(st.type)
        if (!types.isValue(T)) throw new Error('assert isvalue(st.type)')
        env.set(v, T)
      } else if (st.expr instanceof Global) {
        env.set(v, asType(int.defs.resolve_static(st.expr.binding)))
      } else if (st.expr instanceof SetGlobal) {
        return
      } else {
        throw new Error(`Unknown expr type ${st.expr.head}`)
      }
    }
  }
}

function interpretMethod(int: Interpreter, meth: Method, ...args: Type[]): Type | undefined {
  if (meth === invoke_method) return
  if (meth.func) {
    const result = meth.func(...args)
    return result === unreachable ? undefined : result
  }
  const ir = some(int.defs.ir(meth))
  return interpretIR(int, ir, ...args)
}

function interpretFunc(int: Interpreter, func: Tag, args: Type): Type | undefined {
  const methods = int.defs.methods(func)
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
