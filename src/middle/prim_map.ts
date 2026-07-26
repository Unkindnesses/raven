import { tag, Type } from '../frontend/types.js'
import { IRValue, Invoke, MIR, MethodKey, Method } from '../frontend/modules.js'
import { Anno, Fragment, Statement, Val } from '../utils/ir.js'
import * as parse from '../frontend/parse.js'
import { callpattern } from '../frontend/patterns.js'

export {
  InvokeSt, partialPrimitives, inlinePrimitives, outlinePrimitives,
  partialPrimitive, inlinePrimitive, outlinePrimitive, primitive
}

type InvokeSt = Statement<IRValue, Type> & { expr: Invoke<IRValue> }

const partialPrimitives = new Map<bigint, (...args: Type[]) => Anno<Type>>()
const inlinePrimitives = new Map<bigint, (code: Fragment<MIR>, st: InvokeSt) => Val<MIR>>()
const outlinePrimitives = new Map<bigint, (...Ts: Type[]) => MIR>()

function partialPrimitive(method: Method) {
  return partialPrimitives.get(method.id)
}

function inlinePrimitive(method: Method) {
  return inlinePrimitives.get(method.id)
}

function outlinePrimitive(method: Method) {
  return outlinePrimitives.get(method.id)
}

function primitive(name: string, pattern: string, func?: (...args: Type[]) => Anno<Type>): Method {
  func ??= (...args) => { throw new Error(`no partial for ${name}`) }
  const method = new MethodKey(tag('common.core'), tag(name), callpattern(tag(name), parse.expr(pattern)))
  partialPrimitives.set(method.id, func)
  return new Method(method)
}
