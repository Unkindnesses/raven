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
  partialPrimitive, inlinePrimitive, outlinePrimitive, transformPrimitive,
  primitive, closurePrimitive, sources
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

function define(name: string, sig: ast.Tree): Method {
  const method = new MethodKey(tag('common.core'), tag(name))
  const body = ast.Call(tag('common/abort'), 'Primitive not implemented')
  sources.set(method.id, { body, sig, meta: Def(name) })
  return new Method(method, patterns.signature(sig))
}

function args(pattern: string): readonly ast.Tree[] {
  return ast.asExpr(parse.expr(pattern), 'List').args
}

function primitive(name: string, pattern: string, func?: (...args: Type[]) => Anno<Type>): Method {
  const method = define(name, ast.List(tag(name), ...args(pattern)))
  if (func) partialPrimitives.set(method.id, func)
  return method
}

function closurePrimitive(name: string, pattern: string): Method {
  const self = ast.Operator(ast.symbol('_'), ast.symbol(':'),
    ast.Call(tag('common.core/pack'), ast.symbol('_'), ast.symbol('state')))
  return define(name, ast.List(self, ...args(pattern)))
}
