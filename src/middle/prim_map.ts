import { Type } from '../frontend/types'
import { IRType, IRValue, Invoke, MIR } from '../frontend/modules'
import { Fragment, IR, Statement, Val } from '../utils/ir'

export { InvokeSt, inlinePrimitive, outlinePrimitive }

type InvokeSt = Statement<IRValue, IRType> & { expr: Invoke<IRValue> }

const inlinePrimitive = new Map<bigint, (code: Fragment<MIR>, st: InvokeSt) => Val<MIR>>()
const outlinePrimitive = new Map<bigint, (...Ts: Type[]) => MIR>()
