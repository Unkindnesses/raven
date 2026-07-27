import { tag, Type } from '../frontend/types.js'
import { IRValue, Invoke, MIR, MethodKey, Method, Signature, MethodIR } from '../frontend/modules.js'
import { Anno, Fragment, Statement, Val } from '../utils/ir.js'
import * as parse from '../frontend/parse.js'
import * as ast from '../frontend/ast.js'
import { callpattern } from '../frontend/lower.js'

export {
  InvokeSt, partialPrimitives, inlinePrimitives, outlinePrimitives,
  partialPrimitive, inlinePrimitive, outlinePrimitive, primitive, primitiveIR
}

type InvokeSt = Statement<IRValue, Type> & { expr: Invoke<IRValue> }

const partialPrimitives = new Map<bigint, (...args: Type[]) => Anno<Type>>()
const inlinePrimitives = new Map<bigint, (code: Fragment<MIR>, st: InvokeSt) => Val<MIR>>()
const outlinePrimitives = new Map<bigint, (...Ts: Type[]) => MIR>()
const primitivePatterns = new Map<bigint, () => MIR>()

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

function primitiveIR(method: Method): MethodIR {
  const pattern = primitivePatterns.get(method.id)
  if (!pattern) throw new Error(`Missing primitive pattern: ${method.name}`)
  return [undefined, pattern()]
}

function primitive(name: string, pattern: string, func?: (...args: Type[]) => Anno<Type>): Method {
  func ??= (...args) => { throw new Error(`no partial for ${name}`) }
  const ex = ast.asExpr(parse.expr(pattern), 'List')
  let call: [Signature, MIR] | undefined
  const get = () => call ??= callpattern(tag('common.core'), tag(name), ast.List(tag(name), ...ex.args))
  const lazy: Signature = { // TODO awful
    get args() { return get()[0].args },
    get swap() { return get()[0].swap }
  }
  const method = new MethodKey(tag('common.core'), tag(name), lazy)
  primitivePatterns.set(method.id, () => get()[1])
  partialPrimitives.set(method.id, func)
  return new Method(method)
}
