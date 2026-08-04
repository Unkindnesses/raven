import { tag, Type } from '../frontend/types.js'
import { IRValue, Invoke, MIR, MethodKey, Method, MethodSource, Dispatch } from '../frontend/modules.js'
import { Anno, Fragment, Statement, Val } from '../utils/ir.js'
import * as parse from '../frontend/parse.js'
import * as ast from '../frontend/ast.js'
import * as patterns from '../frontend/patterns.js'
import { Def } from '../dwarf/index.js'

export {
  InvokeSt, Lowering, Transform,
  partialPrimitives, inlinePrimitives, outlinePrimitives, transformPrimitives,
  partialPrimitive, inlinePrimitive, outlinePrimitive, transformPrimitive, primitive, sources
}

type InvokeSt = Statement<IRValue, Type> & { expr: Invoke<IRValue> }

interface Lowering {
  define(f: Method, ir: MIR): void
  ir(f: Dispatch | Method, ...Ts: Type[]): MIR
}

type Transform = (code: Lowering, f: Method, ...Ts: Type[]) => MIR

const sources = new Map<bigint, MethodSource>()
const partialPrimitives = new Map<bigint, (...args: Type[]) => Anno<Type>>()
const inlinePrimitives = new Map<bigint, (code: Fragment<MIR>, st: InvokeSt) => Val<MIR>>()
const outlinePrimitives = new Map<bigint, (...Ts: Type[]) => MIR>()
const transformPrimitives = new Map<bigint, Transform>()

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

function transformPrimitive(method: Method) {
  if (method.isSig) return
  return transformPrimitives.get(method.id)
}

function primitive(name: string, pattern: string, func?: (...args: Type[]) => Anno<Type>): Method {
  const ex = ast.asExpr(parse.expr(pattern), 'List')
  const sig = ast.List(tag(name), ...ex.args)
  const method = new MethodKey(tag('common.core'), tag(name))
  const body = ast.Call(tag('common/abort'), 'Primitive not implemented')
  sources.set(method.id, { body, sig, meta: Def(name) })
  if (func) partialPrimitives.set(method.id, func)
  return new Method(method, patterns.signature(sig))
}
