import { tag, Type } from '../frontend/types.js'
import { IRValue, Invoke, MIR, MethodKey, Method, Signature } from '../frontend/modules.js'
import { Anno, Fragment, Statement, Val } from '../utils/ir.js'
import * as parse from '../frontend/parse.js'
import * as ast from '../frontend/ast.js'
import { callpattern } from '../frontend/lower.js'

export {
  InvokeSt, partialPrimitives, inlinePrimitives, outlinePrimitives,
  partialPrimitive, inlinePrimitive, outlinePrimitive, primitive
}

type InvokeSt = Statement<IRValue, Type> & { expr: Invoke<IRValue> }

const partialPrimitives = new Map<bigint, (...args: Type[]) => Anno<Type>>()
const inlinePrimitives = new Map<bigint, (code: Fragment<MIR>, st: InvokeSt) => Val<MIR>>()
const outlinePrimitives = new Map<bigint, (...Ts: Type[]) => MIR>()

function partialPrimitive(method: Method) {
  if (method.isSig) return
  return partialPrimitives.get(method.id)
}

function inlinePrimitive(method: Method) {
  if (method.isSig) return
  return inlinePrimitives.get(method.id)
}

function outlinePrimitive(method: Method) {
  if (method.isSig) return
  return outlinePrimitives.get(method.id)
}

function primitive(name: string, pattern: string, func?: (...args: Type[]) => Anno<Type>): Method {
  func ??= (...args) => { throw new Error(`no partial for ${name}`) }
  const ex = ast.asExpr(parse.expr(pattern), 'List')
  let signature: Signature | undefined
  const get = () => signature ??= callpattern(tag('common.core'), tag(name), ast.List(tag(name), ...ex.args))
  const lazy: Signature = { // TODO awful – store sigs in modules
    get pattern() { return get().pattern },
    get args() { return get().args },
    get swap() { return get().swap }
  }
  const method = new MethodKey(tag('common.core'), tag(name), lazy)
  partialPrimitives.set(method.id, func)
  return new Method(method)
}
