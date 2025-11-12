import { Type } from '../frontend/types'
import { IRValue, Invoke, MIR } from '../frontend/modules'
import { Fragment, Statement, Val } from '../utils/ir'

export { InvokeSt, inlinePrimitive, outlinePrimitive }

type InvokeSt = Statement<IRValue, Type> & { expr: Invoke<IRValue> }

const inlinePrimitive = new Map<bigint, (code: Fragment<MIR>, st: InvokeSt) => Val<MIR>>()
const outlinePrimitive = new Map<bigint, (...Ts: Type[]) => MIR>()
