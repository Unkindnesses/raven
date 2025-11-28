import { tag, Type } from '../frontend/types.js'
import { IRValue, Invoke, MIR, Method } from '../frontend/modules.js'
import { Anno, Fragment, Statement, Val } from '../utils/ir.js'
import * as parse from '../frontend/parse.js'
import { lowerpattern } from '../frontend/patterns.js'

export { InvokeSt, inlinePrimitive, outlinePrimitive, primitive }

type InvokeSt = Statement<IRValue, Type> & { expr: Invoke<IRValue> }

const inlinePrimitive = new Map<bigint, (code: Fragment<MIR>, st: InvokeSt) => Val<MIR>>()
const outlinePrimitive = new Map<bigint, (...Ts: Type[]) => MIR>()

function primitive(name: string, pattern: string, func?: (...args: Type[]) => Anno<Type>): Method {
  func ??= (...args) => { throw new Error(`no partial for ${name}`) }
  return new Method(tag('common.core'), tag(name), lowerpattern(parse.expr(pattern)), func)
}
