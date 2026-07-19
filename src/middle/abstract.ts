import { unreachable, expand, Anno, Block, Expr, Branch, prune, asType } from '../utils/ir.js'
import { LoopIR, looped, Path, block, nextpath, nextpathTo, blockargs, loop, unloop } from './loop.js'
import { MatchMethods, dispatcher } from './patterns.js'
import { Tag, Type, repr, union, issubset as iss, isValue, pack, tag, tagOf, parts, String as RString, Ref, nil, Any, disjuncts } from '../frontend/types.js'
import { wasmPartials } from '../backend/wasm.js'
import { MIR, IRValue, Binding, Method, Definitions, StringRef, JS, Global, SetGlobal, Wasm, callargs } from '../frontend/modules.js'
import { Lowered } from '../frontend/lower.js'
import { Def } from '../dwarf/index.js'
import { WorkQueue } from '../utils/fixpoint.js'
import { hash, HashSet, only, some } from '../utils/map.js'
import { trackdeps, Map as CacheMap, fingerprint, Caching, withtime } from '../utils/cache.js'
import { isEqual } from '../utils/isEqual.js'
import { Instruction } from '../wasm/wasm.js'
import { Traced } from './tracer.js'
import { binding } from '../utils/options.js'

const recursionLimit = 10

export { key, Sig, Inference, Inferred, Redirect, sig, inferexpr, infercall, issubset, maybe_union, traitType, withTraits }

function key(sig: Sig): string {
  const [f, ...Ts] = sig
  return `${f[hash]}:${Ts.map(repr).join(', ')}`
}

function maybe_union(x: Anno<Type>, y: Anno<Type>): Anno<Type> {
  if (x === unreachable) return y
  if (y === unreachable) return x
  return union(x, y)
}

type Func = Tag | Method
type Sig = [Tag, Type, Type] | [Method, ...Type[]]
type AIR = LoopIR<IRValue, Type>

function prepare_ir(ir: MIR): AIR {
  return looped(expand(ir.clone()))
}

class Parent { constructor(readonly sig: Sig | null, readonly depth: number) { } }

class Redirect { constructor(readonly to: Sig) { } }

class GlobalFrame {
  constructor(
    public type: Anno<Type>,
    readonly deps = new Set<never>(),
    readonly edges = new Set<string>()) { }
}

class Frame {
  deps = new Set<string>()
  edges = new Set<string>()
  rettype: Anno<Type> = unreachable
  constructor(readonly sig: Sig, readonly parent: Parent, public ir: AIR) { }
  static create(P: Parent, ir: MIR, f: Func, ...args: Type[]): Frame {
    const l = prepare_ir(ir.clone())
    if (l.ir.block(1).args.length !== args.length) throw new Error('argument length mismatch')
    const b = l.body[0].block(1)
    for (let i = 0; i < args.length; i++) b.bb.args[i][1] = args[i]
    return new Frame([f, ...args] as Sig, P, l)
  }
}

function asFrame(fr: unknown): Frame {
  if (!(fr instanceof Frame)) throw new Error('Expected Frame')
  return fr
}

class Inference {
  deps = new Map<string, Set<bigint>>()
  frames = new Map<string, Frame | Redirect>()
  globals = new Map<string, GlobalFrame>()
  queue = new WorkQueue<string>()
  constructor(readonly defs: Definitions, readonly lowered: Lowered, readonly meths: MatchMethods, readonly traced: Traced) { }

  frame(T: Sig): Frame
  frame(T: Binding): GlobalFrame
  frame(T: Sig | Binding): Frame | GlobalFrame {
    const fr = some(Array.isArray(T) ? this.frames.get(key(T)) : this.globals.get(T[hash]))
    if (fr instanceof Redirect) return this.frame(fr.to)
    return fr
  }
}

function sig(inf: Inference, T: Sig): Sig {
  const fr = some(inf.frames.get(key(T)))
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
  const key = name[hash]
  const existing = inf.globals.get(key)
  if (existing) return existing
  const [T, deps] = trackdeps(() => inf.defs.global(name))
  inf.deps.set(key, deps)
  let type = T
  if (type instanceof Binding) {
    const parentFrame = globalFrame(inf, type)
    parentFrame.edges.add(key)
    type = parentFrame.type
  }
  const frame = new GlobalFrame(type)
  inf.globals.set(key, frame)
  return frame
}

function frame(inf: Inference, P: Parent, sig: Sig): Frame | Anno<Type> {
  const [f, ...Ts] = sig
  const k = key(sig)
  if (inf.frames.has(k)) return inf.frame(sig)
  if (f instanceof Method) {
    if (f.func) return withTraits(T =>
      traitResult(some(infercall(inf, some(P.sig), ...traitSig(T)))),
      () => f.func!(...Ts))
    if (P.depth > recursionLimit) return mergeFrames(inf, some(P.sig), sig)
  }
  const [trace, deps] = trackdeps(() => inf.traced.trace(f, ...Ts))
  if (trace) {
    const [ir, ret] = trace
    const fr = Frame.create(P, ir, f, ...Ts)
    fr.rettype = ret
    inf.deps.set(k, deps)
    inf.frames.set(k, fr)
    return inf.frame(sig)
  }
  if (f instanceof Method) {
    const [ir, ideps] = trackdeps(() => inf.lowered.ir(f))
    for (const dep of ideps) deps.add(dep)
    inf.deps.set(k, deps)
    inf.frames.set(k, Frame.create(P, ir, f, ...Ts))
    update(inf, k)
    return inf.frame(sig)
  } else {
    inf.frames.set(k, new Frame(sig, P, looped(MIR(Def(f.path)))))
    inf.deps.set(k, deps)
    update(inf, k)
    return inf.frame(sig)
  }
}

// TODO some methods become unreachable, remove them somewhere?
function mergeFrames(inf: Inference, T: Sig, F: Sig): Frame {
  const sigs = [...stack(inf, T).frames.filter(t => isEqual(t[0], F[0])), F]
  if (sigs.length <= 1) throw new Error('Expected multiple signatures')
  const sig = sigs.reduce((a, b) => {
    const [fa, ...as] = a
    const [_, ...bs] = b
    return [fa, ...as.map((x, i) => union(x, bs[i]))] as Sig
  })
  const firstFrame = asFrame(inf.frames.get(key(sigs[0])))
  const P = firstFrame.parent.sig
  const fr = frame(inf, new Parent(P, recursionLimit), sig)
  if (!(fr instanceof Frame)) throw new Error('Expected Frame')
  for (const s of sigs) {
    const k = key(s)
    if (isEqual(s, sig)) continue
    if (inf.frames.has(k)) {
      const existing = some(inf.frames.get(k))
      if (existing instanceof Redirect) {
        if (!isEqual(sig, existing.to)) throw new Error('Redirect mismatch') // TODO figure out whether to move backedges here
      } else {
        for (const edge of existing.edges) fr.edges.add(edge)
        fr.edges.add(k) // propagate deletions
      }
    }
    inf.frames.set(k, new Redirect(sig))
  }
  return fr
}

function infercall(inf: Inference, P: Sig, F: Func, ...Ts: Anno<Type>[]): Anno<Type> | undefined {
  if (Ts.some(t => t === unreachable)) return unreachable
  const parent = new Parent(P, recursionDepth(inf, P, F))
  const argTypes: Type[] = Ts.map(t => asType(t))
  const s = [F, ...argTypes] as Sig
  const fr = frame(inf, parent, s)
  if (!(fr instanceof Frame)) return fr
  const psig = key(some(parent.sig))
  const pf = some(inf.frames.get(psig))
  if (pf instanceof Redirect) return
  pf.deps.add(key(s))
  fr.edges.add(psig)
  return fr.rettype
}

function inferexpr(inf: Inference, P: Sig, ir: MIR | Block<MIR>, ex: Expr<IRValue>): Anno<Type> | undefined {
  if (ex.head === 'call') {
    const calleeT = ir.type(ex.body[0])
    if (calleeT === unreachable) return unreachable
  }
  let [F, Ts] = callargs(ir, ex)
  return infercall(inf, P, F, ...Ts.map(x => ir.type(x)))
}

function cleardeps(inf: Inference, sig: string): void {
  const fr = asFrame(inf.frames.get(sig))
  for (const dep of fr.deps) {
    const f = some(inf.frames.get(dep) ?? inf.globals.get(dep))
    if (f instanceof Redirect) throw new Error('Redirect in cleardeps')
    f.edges.delete(sig)
  }
  fr.deps.clear()
}

function issubset(x: Anno<Type>, y: Anno<Type>): boolean {
  if (x === unreachable) return true
  if (y === unreachable) return false
  return iss(x, y)
}

function update_dispatcher(inf: Inference, func: Tag, F: Type, Ts: Type) {
  const [[ir, ret], deps] = trackdeps(() => dispatcher(inf, func, F, Ts))
  const k = key([func, F, Ts])
  inf.deps.set(k, deps)
  const fr = asFrame(inf.frames.get(k))
  fr.ir = looped(expand(ir))
  if (!issubset(ret, fr.rettype)) {
    fr.rettype = ret
    for (const s of fr.edges) inf.queue.push(s)
  }
}

function update(inf: Inference, k: string): void {
  cleardeps(inf, k)
  const fr = asFrame(inf.frames.get(k))
  if (!(fr.sig[0] instanceof Method)) return update_dispatcher(inf, fr.sig[0], fr.sig[1], fr.sig[2])
  let ret: Anno<Type> = unreachable
  let path: Path | null = new Path()
  const reachable = new HashSet<Path>([path])
  update: while (path) {
    const bl = block(fr.ir, path)
    for (const [v, st] of bl) {
      const ex = st.expr
      if (ex instanceof Wasm) {
        if (ex.isImport()) continue
        const instr = ex.callee as Instruction
        const op = instr.kind === 'op' ? instr.name : ''
        const Ts = ex.body.map(x => bl.type(x))
        if (Ts.every(t => t !== unreachable) && Ts.every(t => isValue(t)) && wasmPartials.has(op)) {
          const T = some(wasmPartials.get(op))(...Ts)
          bl.ir.setType(v, T)
        }
      } else if (['call', 'invoke'].includes(ex.head)) {
        const T = inferexpr(inf, fr.sig, bl, ex)
        if (T === undefined) return
        if (T === unreachable) break
        bl.ir.setType(v, T)
      } else if (ex.head === 'pack') {
        const Ts = ex.body.map(x => bl.type(x))
        if (Ts.some(t => t === unreachable)) break
        bl.ir.setType(v, pack(...Ts as Type[]))
      } else if (ex instanceof Global) {
        const g = globalFrame(inf, ex.binding)
        fr.deps.add(ex.binding[hash])
        g.edges.add(k)
        if (g.type === unreachable) break
        bl.ir.setType(v, g.type)
      } else if (ex instanceof SetGlobal) {
        if (bl.type(ex.value) === unreachable) break
        bl.ir.setType(v, nil)
      } else if (ex.head === 'loop') {
        const inner = some(loop(bl))
        blockargs(inner.body[0].block(1), bl.argtypes.map(t => asType(t)))
        path = new Path([...path.parts, [1, 1]])
        continue update
      } else if (st.expr instanceof Branch) {
        const br = st.expr
        if (br.isreturn()) {
          ret = maybe_union(ret, bl.type(br.args[0]))
        } else if (br.isunreachable()) {
          break
        } else {
          const condT = br.isconditional() ? asType(bl.type(br.when)) : Type(true)
          if (!tag('common.Bool').isEqual(tagOf(condT))) throw new Error('branch condition must be Bool')
          if (isEqual(condT, Type(false))) continue
          let [p, rr] = nextpathTo(fr.ir, path, br.target)
          if (rr && !p.lt(path)) throw new Error('unimplemented')
          const args = br.args.map(a => asType(bl.type(a)))
          reachable.add(p)
          if ((blockargs(block(fr.ir, p), args) || rr) && p.lt(path)) {
            path = p
            continue update
          }
          if (isEqual(condT, Type(true))) break
        }
      } else if (ex.head === 'tuple') {
        if (!isValue(asType(st.type))) throw new Error('tuple without type')
      } else if (ex instanceof StringRef) {
        bl.ir.setType(v, RString())
      } else if (ex instanceof JS) {
        bl.ir.setType(v, Ref)
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
function remove(inf: Inference, sig: string) {
  const fr = inf.frames.get(sig) ?? inf.globals.get(sig)
  if (!fr) return
  // cleardeps(inf, sig)
  inf.frames.delete(sig)
  inf.globals.delete(sig)
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
      const fr = inf.frames.get(sig)
      if (!fr || fr instanceof Redirect) throw e
      throw new CompileError(e, stack(inf, fr.sig))
    }
  }
  return inf
}

// Results and caching

class Inferred implements Caching {
  readonly inf: Inference
  readonly results: CacheMap<string, [MIR, Anno<Type>] | Redirect>
  time = 0n

  constructor(defs: Definitions, lowered: Lowered, meths: MatchMethods, traced: Traced) {
    this.inf = new Inference(defs, lowered, meths, traced)
    this.results = new CacheMap()
  }

  get size() { return this.results.size }
  iscached(k: string): boolean { return this.results.iscached(k) }

  _get(sig: Sig): [MIR, Anno<Type>] | Redirect {
    const k = key(sig)
    if (this.iscached(k)) return this.results.get(k)!
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
    return this.results.get(k)!
  }

  get(sig: Sig): [MIR, Anno<Type>] | Redirect {
    const [res, t] = withtime(() => this._get(sig))
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

  traitType(T: Type): Anno<Type> {
    const result = this.get(traitSig(T))
    if (result instanceof Redirect) throw new Error('Unexpected castTrait redirect')
    return traitResult(result[1])
  }
}

function traitSig(T: Type): [Tag, Type, Type] {
  return [tag('common.castTrait'), tag('common.castTrait'), pack(tag('common.List'), T, Any)]
}

function traitResult(ret: Anno<Type>): Anno<Type> {
  if (ret === unreachable) return unreachable
  const T = only(parts(ret))
  for (const option of disjuncts(T))
    if (tag('common.Some').isEqual(tagOf(option))) return only(parts(option))
  return unreachable
}

const [withTraits, getTraits] = binding<(T: Type) => Anno<Type>>('traits')

function traitType(T: Type): Anno<Type> { return getTraits()(T) }
