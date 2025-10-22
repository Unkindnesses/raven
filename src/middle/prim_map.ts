import { Type } from '../frontend/types'
import { IRType, IRValue, MIR, Method } from '../frontend/modules'
import { Fragment, Statement, Val } from '../utils/ir'

export { inlinePrimitive, outlinePrimitive }

const inlinePrimitive = new Map<bigint, (code: Fragment<MIR>, st: Statement<IRValue, IRType>) => Val<MIR>>()
const outlinePrimitive = new Map<bigint, (...Ts: Type[]) => MIR>()
