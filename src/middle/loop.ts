import { some } from '../utils/map'
import { Type, issubset, union, asType } from '../frontend/types'
import { IRValue, MIR } from '../frontend/modules'
import { IR, Block, Branch, CFG, Component, components, entry, rename, Expr, unreachable, getIndent, withIndent } from '../utils/ir'

export { LoopIR, loop, looped, unloop, Path, tail, block, nextpath, nextpathTo, pin, reroll, blockargs }

function copyblock<T, A>(to: Block<IR<T, A>>, from: Block<IR<T, A>>) {
  const env = new Map<number, T | number>()
  for (let i = 0; i < from.args.length; i++)
    env.set(from.args[i], to.argument(from.argtypes[i]))
  for (const [v, st] of from)
    env.set(v, to.push(rename(env, st)))
}

class LoopIR<T, A> {
  constructor(readonly ir: IR<T, A>, readonly bls: number[], readonly body: IR<T, A>[], public max: number) { }
}

function showLoop<T, A>(l: LoopIR<T, A>, args: (T | number)[], pr: (x: T | number) => string): string {
  const show = (x: T | number) => typeof x === 'number' ? `%${x}` : pr(x)
  let s = args.length ? `loop ${args.map(show).join(', ')}:` : 'loop:'
  l.body.forEach((b, i) => {
    const head = '  '.repeat(getIndent() + 2) + `#${i + 1}:`
    const body = withIndent(3, () => b.toString(false))
    s += `\n${head}\n${body}`
  })
  return s
}

class Loop<T, A> extends Expr<T> {
  constructor(readonly loop: LoopIR<T, A>, args: (T | number)[]) {
    super('loop', args)
  }
  map(f: (x: T | number) => T | number): Loop<T, A> {
    return new Loop(this.loop, this.body.map(f))
  }
  show(pr: (x: T | number) => string): string {
    return showLoop(this.loop, this.body, pr)
  }
}

function loop<T, A>(b: Block<IR<T, A>>): LoopIR<T, A> | null {
  if (b.length === 0) return null
  const [, first] = [...b][0]
  if (first.expr.head !== 'loop') return null
  const e = first.expr as { head: 'loop', loop?: LoopIR<T, A> }
  return e.loop ?? null
}

function looped<T, A>(ir: IR<T, A>, cs?: Component): LoopIR<T, A> {
  cs ??= components(new CFG(ir))
  if (!Array.isArray(cs)) cs = [cs]
  const out = ir.empty()
  const blocks: number[] = []
  cs.forEach((ch, i) => {
    const bl = i === 0 ? out.block(1) : out.newBlock()
    if (typeof ch === 'number') {
      blocks.push(ch)
      copyblock(bl, ir.block(ch))
    } else {
      blocks.push(entry(ch))
      const argts = ir.block(entry(ch)).argtypes
      const args = argts.map(T => bl.argument(T))
      bl.push(bl.stmt(new Loop(looped(ir, ch), args)))
    }
  })
  return new LoopIR(out, blocks, [out.clone()], 8)
}

function nblocksBlock<T, A>(b: Block<IR<T, A>>): number {
  const l = loop(b)
  return l ? nblocksLoop(l) : 1
}

function nblocksIR<T, A>(ir: IR<T, A>): number {
  return Array.from(ir.blocks()).reduce((a, b) => a + nblocksBlock(b), 0)
}

function nblocksLoop<T, A>(l: LoopIR<T, A>): number {
  return l.body.reduce((acc, x) => acc + nblocksIR(x), 0)
}

function blockmap<T, A>(l: LoopIR<T, A>, offset = 1): Map<number, number> {
  const map = new Map<number, number>()
  for (const b of l.ir.blocks()) {
    map.set(l.bls[b.id], offset)
    const l2 = loop(b)
    offset += l2 ? nblocksLoop(l2) : 1
  }
  return map
}

function unloop<T, A>(l: LoopIR<T, A>): IR<T, A> {
  const out = l.ir.empty()
  out.deleteBlock(1)
  const unloopRec = (l: LoopIR<T, A>, bs: Map<number, number>) => {
    for (let iter = 0; iter < l.body.length; iter++) {
      const lir = l.body[iter]
      const entryId = out.blockCount + 1
      const local = new Map([...blockmap(l, entryId), ...bs])
      const nextEntry = entryId + nblocksIR(lir)
      local.set(l.bls[0], iter === l.body.length - 1 ? entryId : nextEntry)
      for (const b of lir.blocks()) {
        const inner = loop(b)
        if (inner) unloopRec(inner, local)
        else {
          const c = out.newBlock()
          copyblock(c, b)
          for (const [v, st] of c) if (st.expr instanceof Branch) {
            if (st.expr.isreturn() || st.expr.isunreachable()) continue
            const target = some(local.get(st.expr.target))
            c.ir.set(v, new Branch(target, st.expr.args, st.expr.when))
          }
        }
      }
    }
  }
  unloopRec(l, new Map())
  return out
}

// Navigation during inference

class Path {
  constructor(readonly parts: [number, number][] = [[1, 1]]) { } // iter, block
  lt(other: Path): boolean {
    const A = this.parts, B = other.parts
    for (let i = 0; i < Math.min(A.length, B.length); i++) {
      if (A[i][0] !== B[i][0]) return A[i][0] < B[i][0]
      if (A[i][1] !== B[i][1]) return A[i][1] < B[i][1]
    }
    return A.length < B.length
  }
}

function tail(p: Path): Path { return new Path(p.parts.slice(1)) }

function block<T, A>(ir: LoopIR<T, A>, p: Path): Block<IR<T, A>> {
  for (let i = 0; i < p.parts.length - 1; i++) {
    const [itr, b] = p.parts[i]
    const l = loop(ir.body[itr - 1].block(b))
    if (!l) throw new Error('Invalid loop path')
    ir = l
  }
  const [itr, b] = p.parts[p.parts.length - 1]
  return ir.body[itr - 1].block(b)
}

function blockargs(x: MIR | Block<MIR>, args: Type[]): boolean {
  const bl = x instanceof IR ? x.block(1) : x
  let changed = false
  for (let i = 0; i < bl.argtypes.length; i++) {
    if (bl.argtypes[i] !== unreachable && issubset(args[i], asType(bl.argtypes[i]))) continue
    bl.bb.args[i][1] = bl.argtypes[i] === unreachable ? args[i] : union(asType(bl.argtypes[i]), args[i])
    changed = true
  }
  return changed
}

function reroll(ir: LoopIR<IRValue, any>): boolean {
  ir.max = 1
  if (ir.body.length === 1) return false
  let changed = false
  const first = ir.body[0]
  for (const x of ir.body.slice(1)) {
    changed ||= blockargs(first, x.block(1).argtypes.map(a => asType(a)))
  }
  ir.body.length = 1
  return changed
}

function nextpath<T, A>(ir: LoopIR<T, A>, p: Path): Path | null {
  const [itr, bl] = p.parts[0]
  const inner = loop(ir.body[itr - 1].block(bl))
  const next = p.parts.length === 1 || !inner ? null : nextpath(inner, tail(p))
  if (next) return new Path([p.parts[0], ...next.parts])
  return bl < ir.bls.length ? new Path([[itr, bl + 1]]) :
    itr < ir.body.length ? new Path([[itr + 1, 1]]) : null
}

function nextpathTo(ir: LoopIR<IRValue, unknown>, p: Path, target: number): [Path, boolean] {
  const q: [number, number][] = []
  for (const [itr, b] of p.parts) {
    const c = ir.bls.indexOf(target) + 1
    if (c === 0 || c === b) { // back edge
      ir = some(loop(ir.body[itr - 1].block(b)))
      q.push([itr, b])
      continue
    }
    if (c === 1) {
      if (itr >= ir.max) {
        q.push([1, 1])
        return [new Path(q), reroll(ir)]
      }
      if (itr === ir.body.length) ir.body.push(ir.ir.clone())
      q.push([itr + 1, 1])
      return [new Path(q), false]
    } else {
      q.push([itr, c])
      return [new Path(q), false]
    }
  }
  throw new Error(`Invalid block target ${target}`)
}

// If a loop iteration breaks out, don't unroll it further.
function pin(ir: LoopIR<IRValue, unknown>, p: Path, depth: number): boolean {
  let rr = false
  for (let i = 0; i < p.parts.length; i++) {
    const [itr, b] = p.parts[i]
    if (i + 1 > depth) {
      ir.max = itr
      if (ir.body.length > ir.max) {
        rr ||= reroll(ir)
        if (itr !== 1) return rr
      }
    }
    if (i < p.parts.length - 1)
      ir = some(loop(ir.body[itr - 1].block(b)))
  }
  return rr
}
