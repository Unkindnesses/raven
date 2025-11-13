import * as ir from '../utils/ir'
import { MIR, callargs } from '../frontend/modules'
import { Redirect, type Sig } from './abstract'
import { options } from '../utils/options'
import { CycleCache } from '../utils/cache'
import { layout } from './expand'
import { some, only } from '../utils/map'
import { Accessor } from '../utils/fixpoint'
import { Stack } from '../dwarf'

export { Inlined, opcount }

function opcount(code: MIR): number {
  let count = 0
  for (const [_, st] of code) {
    if (['tuple', 'ref', 'branch', 'cast', 'retain', 'release', 'string'].includes(st.expr.head)) {
    } else if (['global', 'setglobal'].includes(st.expr.head)) {
      count += layout(ir.asType(st.type)).length > 0 ? 1 : 0
    } else if (['call', 'invoke', 'wasm', 'func', 'call_indirect'].includes(st.expr.head)) {
      count += 1
    } else
      throw new Error(`unrecognised expr ${st.expr.head}`)
  }
  return count
}

function inlineable(cache: Accessor<Sig, Redirect | MIR>, sig: Sig): boolean {
  const ir = cache.get(sig)
  if (ir instanceof Redirect) return inlineable(cache, ir.to)
  return ir.blockCount === 1 && opcount(ir) <= 3
}

function inlineHere(pr: ir.Pipe<MIR>, callee: MIR, src: Stack, args: ir.Val<MIR>[]): ir.Val<MIR> | undefined {
  if (callee.blockCount !== 1) throw new Error('inlineHere: expected single-block IR')
  const bl = callee.block(1)
  if (bl.args.length !== args.length) throw new Error('argument length mismatch')
  const env = new Map<number, ir.Val<MIR>>()
  let rename = (x: ir.Val<MIR>) => typeof x === 'number' ? some(env.get(x)) : x
  bl.args.forEach((a, i) => env.set(a, args[i]))
  for (const [v, st] of bl) {
    if (st.expr.head === 'branch') continue
    env.set(v, pr.push({ ...st, expr: st.expr.map(rename), src: [...src, ...st.src] }))
  }
  const br = only(bl.branches())
  if (br.isunreachable()) return undefined
  return rename(some(br.args[0]))
}

function inline(code: MIR, inlined: Accessor<Sig, Redirect | MIR>): MIR {
  const pr = new ir.Pipe(code)
  for (const [v, st] of pr) {
    if (!['call', 'invoke'].includes(st.expr.head)) continue
    const [F, args] = callargs(pr, st.expr)
    const sig: Sig = [F, ...args.map(x => ir.asType(pr.type(x)))]
    if (!inlineable(inlined, sig)) continue
    pr.delete(v)
    const ret = inlineHere(pr, ir.asIR(inlined.get(sig)), st.src, args)
    if (ret === undefined && st.type !== ir.unreachable) throw new Error('nope')
    if (ret !== undefined) pr.replace(v, ret)
  }
  return pr.finish()
}

function Inlined(cache: Accessor<Sig, Redirect | MIR>): CycleCache<Sig, Redirect | MIR> {
  const init = (sig: Sig) => cache.get(sig)
  return new CycleCache<Sig, Redirect | MIR>(init, (self, sig) => {
    let res = self.get(sig)
    if (!options().inline) return res
    if (res instanceof Redirect) return res
    return inline(res, self)
  })
}
