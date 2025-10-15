import { unreachable, IR, expand, Anno, Block, Expr, Branch, asAnno, prune } from '../utils/ir'
import { LoopIR, looped, Path, block, nextpath, nextpathTo, pin, blockargs, loop, unloop } from './loop'
import { MatchMethods, dispatcher } from './patterns'
import { Tag, Type, repr, union, asType, issubset as iss, isValue, pack, tag, tagOf } from '../frontend/types'
import { wasmPartials } from '../backend/wasm'
import { MIR, IRValue, IRType, Binding, Method, Definitions, FuncInfo, WIntrinsic, WImport, StringRef } from '../frontend/modules'
import { WorkQueue } from '../utils/fixpoint'
import { HashMap, HashSet, some } from '../utils/map'
import { trackdeps, Map as CacheMap, fingerprint, Caching, time } from '../utils/cache'
import isEqual from 'lodash/isEqual'

const recursionLimit = 10

export { type Sig, Inference, Inferred, Redirect, sig, inferexpr, infercall, issubset, maybe_union }

function maybe_union(x: Anno<Type>, y: Anno<Type>): Anno<Type> {
  if (x === unreachable) return y
  if (y === unreachable) return x
  return union(x, y)
}

type Func = Tag | Method
type Sig = [Func, ...Type[]]
type AIR = LoopIR<IRValue, IRType, FuncInfo | undefined>

function prepare_ir(ir: MIR): AIR {
  return looped(expand(ir.clone()))
}

class Parent { constructor(readonly sig: Sig | null, readonly depth: number) { } }

class Redirect { constructor(readonly to: Sig) { } }

class GlobalFrame {
  constructor(
    public type: Anno<Type>,
    readonly deps = new HashSet<never>(),
    readonly edges = new HashSet<Sig | Binding>()) { }
}

class Frame {
  deps = new HashSet<Sig | Binding>()
  edges = new HashSet<Sig>()
  rettype: Anno<Type> = unreachable
  constructor(readonly parent: Parent, public ir: AIR) { }
  static create(P: Parent, ir: MIR, ...args: Type[]): Frame {
    const l = prepare_ir(ir.clone())
    if (l.ir.block(1).args.length !== args.length) throw new Error('argument length mismatch')
    const b = l.body[0].block(1)
    for (let i = 0; i < args.length; i++) b.bb.args[i][1] = args[i]
    return new Frame(P, l)
  }
}

function asFrame(fr: unknown): Frame {
  if (!(fr instanceof Frame)) throw new Error('Expected Frame')
  return fr
}

class Inference {
  deps = new HashMap<Sig | Binding, Set<bigint>>()
  frames = new HashMap<Sig, Frame | Redirect>()
  globals = new HashMap<Binding, GlobalFrame>()
  queue = new WorkQueue<Sig>()
  constructor(readonly defs: Definitions, readonly meths: MatchMethods) { }

  frame(T: Sig): Frame
  frame(T: Binding): GlobalFrame
  frame(T: Sig | Binding): Frame | GlobalFrame {
    const fr = some(Array.isArray(T) ? this.frames.get(T) : this.globals.get(T))
    if (fr instanceof Redirect) return this.frame(fr.to)
    return fr
  }
}

function sig(inf: Inference, T: Sig): Sig {
  const fr = some(inf.frames.get(T))
  if (fr instanceof Redirect) return sig(inf, fr.to)
  return T
}

function parent(inf: Inference, T: Sig): Sig | null {
  const P = inf.frame(T).parent.sig
  return P ? sig(inf, P) : null
}

function recursionDepth(inf: Inference, T: Sig | null, F: Func): number {
  while (T) {
    if (isEqual(T[0], F)) return inf.frame(T).parent.depth + 1
    T = parent(inf, T)
  }
  return 1
}

function globalFrame(inf: Inference, name: Binding): GlobalFrame {
  const existing = inf.globals.get(name)
  if (existing) return existing
  const [T, deps] = trackdeps(() => inf.defs.global(name))
  inf.deps.set(name, deps)
  let type = T
  if (type instanceof Binding) {
    const parentFrame = globalFrame(inf, type)
    parentFrame.edges.add(name)
    type = parentFrame.type
  }
  const frame = new GlobalFrame(type)
  inf.globals.set(name, frame)
  return frame
}

function irframe(inf: Inference, P: Parent, ir: MIR, F: Func, ...Ts: Type[]): Frame {
  const sig: Sig = [F, ...Ts]
  inf.frames.set(sig, Frame.create(P, ir, ...Ts))
  update(inf, sig)
  return inf.frame(sig)
}

function methodFrame(inf: Inference, P: Parent, meth: Method, ...Ts: Type[]): Frame | Anno<Type> {
  if (meth.func) return meth.func(...Ts)
  const sig: Sig = [meth, ...Ts]
  if (inf.frames.has(sig)) return inf.frame(sig)
  if (P.depth > recursionLimit) return mergeFrames(inf, some(P.sig), sig)
  const [ir, deps] = trackdeps(() => some(inf.defs.ir(meth)))
  inf.deps.set([meth, ...Ts], deps)
  return irframe(inf, P, ir, meth, ...Ts)
}

function tagFrame(inf: Inference, P: Parent, F: Tag, T: Type): Frame {
  const sig: Sig = [F, T]
  if (inf.frames.has(sig)) return inf.frame(sig)
  inf.frames.set(sig, new Frame(P, looped(MIR(undefined))))
  update(inf, sig)
  return inf.frame(sig)
}

function frame(inf: Inference, P: Parent, sig: Sig): Frame | Anno<Type> {
  const [f, ...Ts] = sig
  return f instanceof Method ? methodFrame(inf, P, f, ...Ts) : tagFrame(inf, P, f, Ts[0])
}

// TODO some methods become unreachable, remove them somewhere?
function mergeFrames(inf: Inference, T: Sig, F: Sig): Frame {
  const sigs = [...stack(inf, T).frames.filter(t => isEqual(t[0], F[0])), F]
  if (sigs.length <= 1) throw new Error('Expected multiple signatures')
  const sig = sigs.reduce((a, b) => {
    const [fa, ...as] = a
    const [_, ...bs] = b
    return [fa, ...as.map((x, i) => union(x, bs[i]))]
  })
  const firstFrame = asFrame(inf.frames.get(sigs[0]))
  const P = firstFrame.parent.sig
  const fr = frame(inf, new Parent(P, recursionLimit), sig)
  if (!(fr instanceof Frame)) throw new Error('Expected Frame')
  for (const s of sigs) {
    if (isEqual(s, sig)) continue
    if (inf.frames.has(s)) {
      const existing = some(inf.frames.get(s))
      if (existing instanceof Redirect) {
        if (!isEqual(sig, existing.to)) throw new Error('Redirect mismatch') // TODO figure out whether to move backedges here
      } else {
        for (const edge of existing.edges) fr.edges.add(edge)
        fr.edges.add(s) // propagate deletions
      }
    }
    inf.frames.set(s, new Redirect(sig))
  }
  return fr
}

function infercall(inf: Inference, P: Sig, F: Func, ...Ts: Anno<Type>[]): Anno<Type> | undefined {
  if (Ts.some(t => t === unreachable)) return unreachable
  const parent = new Parent(P, recursionDepth(inf, P, F))
  const argTypes: Type[] = Ts.map(t => asType(t))
  const s: Sig = [F, ...argTypes]
  const fr = frame(inf, parent, s)
  if (!(fr instanceof Frame)) return fr
  const psig = some(parent.sig)
  const pf = some(inf.frames.get(psig))
  if (pf instanceof Redirect) return
  pf.deps.add(s)
  fr.edges.add(psig)
  return fr.rettype
}

function inferexpr(inf: Inference, P: Sig, ir: MIR | Block<MIR>, ex: Expr<IRValue>): Anno<Type> | undefined {
  const [F, ...rest] = ex.body.map(x => ir.type(x))
  if (!(F instanceof Tag) && !(F instanceof Method)) throw new Error('call head must be a Tag or Method')
  const Ts = rest.map(T => asAnno(asType, T))
  return infercall(inf, P, F, ...Ts)
}

function cleardeps(inf: Inference, sig: Sig): void {
  const fr = asFrame(inf.frames.get(sig))
  for (const dep of fr.deps) {
    if (Array.isArray(dep)) {
      const df = asFrame(inf.frames.get(dep))
      df.edges.delete(sig)
    } else {
      const gf = some(inf.globals.get(dep))
      gf.edges.delete(sig)
    }
  }
  fr.deps.clear()
}

function issubset(x: Anno<Type>, y: Anno<Type>): boolean {
  if (x === unreachable) return true
  if (y === unreachable) return false
  return iss(x, y)
}

function update_dispatcher(inf: Inference, func: Tag, Ts: Type) {
  const [[ir, ret], deps] = trackdeps(() => dispatcher(inf, func, Ts))
  inf.deps.set([func, Ts], deps)
  const fr = asFrame(inf.frames.get([func, Ts]))
  fr.ir = looped(expand(ir))
  if (!issubset(ret, fr.rettype)) {
    fr.rettype = ret
    for (const s of fr.edges) inf.queue.push(s)
  }
}

function update(inf: Inference, sig: Sig): void {
  cleardeps(inf, sig)
  if (!(sig[0] instanceof Method)) { return update_dispatcher(inf, sig[0], sig[1]) }
  const fr = asFrame(inf.frames.get(sig))
  let ret: Anno<Type> = unreachable
  let path: Path | null = new Path()
  const reachable = new HashSet<Path>([path])
  loop: while (path) {
    const bl = block(fr.ir, path)
    for (const [v, st] of bl) {
      const ex = st.expr
      if (ex.head === 'call' && ex.body[0] instanceof WIntrinsic) {
        const op = ex.body[0]
        const Ts = ex.body.slice(1).map(x => asAnno(asType, bl.type(x)))
        if (Ts.every(t => t !== unreachable) && Ts.every(t => isValue(t)) && wasmPartials.has(op.name)) {
          const T = some(wasmPartials.get(op.name))(...Ts)
          bl.ir.setType(v, T)
        }
      } else if (ex.head === 'call' && ex.body[0] instanceof WImport) {
      } else if (ex.head === 'call') {
        const T = inferexpr(inf, sig, bl, ex)
        if (T === undefined) return
        if (T === unreachable) break
        bl.ir.setType(v, T)
      } else if (ex.head === 'pack') {
        const Ts = ex.body.map(x => asAnno(asType, bl.type(x)))
        if (Ts.some(t => t === unreachable)) break
        bl.ir.setType(v, pack(...Ts as Type[]))
      } else if (ex.head === 'global') {
        const b = ex.body[0]
        if (!(b instanceof Binding)) throw new Error('invalid global')
        const g = globalFrame(inf, b)
        fr.deps.add(b)
        g.edges.add(sig)
        if (g.type === unreachable) break
        bl.ir.setType(v, g.type)
      } else if (ex.head === 'set' && (ex.body[0] instanceof Binding)) {
        const T = bl.type(ex.body[1])
        if (T === unreachable) break
        bl.ir.setType(v, asType(T))
      } else if (ex.head === 'loop') {
        const inner = some(loop(bl))
        blockargs(inner.body[0].block(1), bl.argtypes.map(t => asType(t)))
        path = new Path([...path.parts, [1, 1]])
        continue loop
      } else if (st.expr instanceof Branch) {
        const br = st.expr
        if (br.isreturn()) {
          ret = maybe_union(ret, asAnno(asType, bl.type(br.args[0])))
        } else if (br.isunreachable()) {
          break
        } else {
          const condT = br.isconditional() ? asType(bl.type(br.when)) : Type(true)
          if (!isEqual(tagOf(condT), tag('common.Bool'))) throw new Error('branch condition must be Bool')
          if (isEqual(condT, Type(false))) continue
          let [p, rr] = nextpathTo(fr.ir, path, br.target)
          rr ||= pin(fr.ir, path, p.parts.length)
          if (rr && !p.lt(path)) throw new Error('unimplemented')
          const args = br.args.map(a => asType(bl.type(a)))
          reachable.add(p)
          if ((blockargs(block(fr.ir, p), args) || rr) && p.lt(path)) {
            path = p
            continue loop
          }
          if (isEqual(condT, Type(true))) break
        }
      } else if (ex.head === 'tuple') {
        if (!isValue(asType(st.type))) throw new Error('tuple without type')
      } else if (ex instanceof StringRef) {
        asType(st.type)
      } else throw new Error(`Unknown expr type ${ex.head}`)
    }
    while (true) {
      path = nextpath(fr.ir, path)
      if (!path || reachable.has(path)) break
    }
  }
  if (!issubset(ret, fr.rettype)) {
    fr.rettype = ret
    for (const s of fr.edges) inf.queue.push(s)
  }
}

// TODO remove backedges, so we don't do redundant work
function remove(inf: Inference, sig: Sig | Binding) {
  const inFrames = Array.isArray(sig)
  if (inFrames ? !inf.frames.has(sig) : !inf.globals.has(sig)) return
  const fr = inFrames ? some(inf.frames.get(sig)) : some(inf.globals.get(sig))
  // cleardeps(inf, sig)
  if (inFrames) inf.frames.delete(sig)
  else inf.globals.delete(sig)
  inf.deps.delete(sig)
  if (fr instanceof Redirect) return
  for (const loc of fr.edges) remove(inf, loc)
}

// Virtual stack traces

class Stack {
  constructor(readonly frames: Sig[] = []) { }
  toString(): string {
    const lines = [
      'Abstract stack trace:',
      ...this.frames.map(([f, ...Ts]) => {
        const head = `${f.toString()}: `
        const body = f instanceof Method
          ? `(${Ts.map(repr).join(', ')})`
          : repr(Ts[0])
        return head + body
      })
    ]
    return lines.join('\n')
  }
}

function stack(inf: Inference, T: Sig | null): Stack {
  const st = new Stack()
  while (T !== null) {
    st.frames.unshift(T)
    T = parent(inf, T)
  }
  return st
}

class CompileError extends Error {
  constructor(readonly error: unknown, readonly vstack: Stack) {
    super('CompileError')
    Object.setPrototypeOf(this, new.target.prototype)
  }
  toString(): string {
    return `Compiler error at\n${this.vstack.toString()}\n${String(this.error)}`
  }
}

// Inference Loop

function infer(inf: Inference, { partial = false }: { partial?: boolean } = {}): Inference {
  while (!inf.queue.empty) {
    const sig = inf.queue.pop()
    try {
      update(inf, sig)
    } catch (e) {
      if (partial) break
      throw new CompileError(e, stack(inf, sig))
    }
  }
  return inf
}

// Results and caching

class Inferred implements Caching {
  readonly inf: Inference
  readonly results: CacheMap<Sig, [MIR, Anno<Type>] | Redirect>
  time = 0n

  constructor(defs: Definitions, meths: MatchMethods) {
    this.inf = new Inference(defs, meths)
    this.results = new CacheMap()
  }

  iscached(k: Sig): boolean { return this.results.iscached(k) }

  _get(sig: Sig): [MIR, Anno<Type>] | Redirect {
    if (this.iscached(sig)) return this.results.get(sig)!
    // Don't let inference dependencies leak
    const [_, deps] = trackdeps(() => {
      frame(this.inf, new Parent(null, 1), sig)
      infer(this.inf)
    })
    if (deps.size !== 0) throw new Error('assertion')
    for (const [k, fr] of this.inf.frames) {
      if (this.iscached(k)) continue
      if (fr instanceof Redirect) this.results.set(k, fr)
      else this.results.set(k, [prune(unloop(fr.ir)), fr.rettype])
    }
    return this.results.get(sig)!
  }

  get(sig: Sig): [MIR, Anno<Type>] | Redirect {
    const [res, t] = time(() => this._get(sig))
    this.time += t
    return res
  }

  fingerprint(): Set<bigint> { return fingerprint(this.results) }
  reset(deps: Set<bigint>) {
    for (const [x, d] of this.inf.deps) {
      const sub = Array.from(d).every(id => deps.has(id))
      if (!sub) remove(this.inf, x)
    }
    for (const k of this.results.keys())
      if (!this.inf.frames.has(k)) this.results.delete(k)
  }
}
