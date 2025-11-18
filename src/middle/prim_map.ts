import { Type } from '../frontend/types.js'
import { IRValue, Invoke, MIR } from '../frontend/modules.js'
import { Fragment, Statement, Val } from '../utils/ir.js'

export { InvokeSt, inlinePrimitive, outlinePrimitive }

type InvokeSt = Statement<IRValue, Type> & { expr: Invoke<IRValue> }

const inlinePrimitive = new Map<bigint, (code: Fragment<MIR>, st: InvokeSt) => Val<MIR>>()
const outlinePrimitive = new Map<bigint, (...Ts: Type[]) => MIR>()
