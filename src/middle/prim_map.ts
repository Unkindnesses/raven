import { Type } from '../frontend/types'
import { MIR, Method } from '../frontend/modules'
import { Pipe } from '../utils/ir'

export { inlinePrimitive, outlinePrimitive }

// TODO remove code arg
const inlinePrimitive = new Map<Method, (pipe: Pipe<MIR>, code: MIR, v: number) => void>()
const outlinePrimitive = new Map<Method, (...Ts: Type[]) => MIR>()
