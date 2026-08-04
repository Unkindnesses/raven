import { unreachable, expand, Anno, Block, Expr, Branch, Pipe, expr, prune, asType } from '../utils/ir.js'
import { LoopIR, looped, Path, block, nextpath, nextpathTo, blockargs, loop, unloop } from './loop.js'
import { MatchMethods, dispatcher } from './patterns.js'
import {
  Type, repr, union, issubset as iss, isValue, pack, Closure,
  tag, tagOf, parts, String as RString, Ref, nil, Any, disjuncts
} from '../frontend/types.js'
import { wasmPartials } from '../backend/wasm.js'
import {
  MIR, IRValue, Binding, Dispatch, Method, Definitions, StringRef, JS, Invoke, Closure as XClosure,
  Global, SetGlobal, Wasm, callargs
} from '../frontend/modules.js'
import { Lowered } from '../frontend/lower.js'
import { Def } from '../dwarf/index.js'
import { WorkQueue } from '../utils/fixpoint.js'
import { hash, HashSet, only, some } from '../utils/map.js'
import { trackdeps, Map as CacheMap, fingerprint, Caching, withtime } from '../utils/cache.js'
import { isEqual } from '../utils/isEqual.js'
import { Instruction } from '../wasm/wasm.js'
import { Traced } from './tracer.js'
import { binding } from '../utils/options.js'
import { Lowering, Transform, partialPrimitive, transformPrimitive } from './prim_map.js'

const recursionLimit = 10

export { key, Sig, Inference, Inferred, inferexpr, infercall, issubset, maybe_union, traitType, withTraits }

function key(sig: Sig): string {
  const [f, ...Ts] = sig
  return `${f[hash]}:${Ts.map(repr).join(', ')}`
}

function maybe_union(x: Anno<Type>, y: Anno<Type>): Anno<Type> {
  if (x === unreachable) return y
  if (y === unreachable) return x
  return union(x, y)
}

type Func = Dispatch | Method
type Sig = [Dispatch, Type, Type] | [Method, ...Type[]]
type AIR = LoopIR<IRValue, Type>

function prepare_ir(ir: MIR, args: Type[]): AIR {
  const l = looped(expand(ir.clone()))
  if (l.ir.block(1).args.length !== args.length) throw new Error('argument length mismatch')
  const b = l.body[0].block(1)
  for (let i = 0; i < args.length; i++) b.bb.args[i][1] = args[i]
  return l
}

function striptypes(code: MIR): MIR {
  for (const [v, st] of code)
    if (!(st.expr instanceof Wasm)) code.setType(v, unreachable)
  for (const bl of code.blocks())
    for (const arg of bl.bb.args) arg[1] = unreachable
  return code
}

class Stack {
  constructor(readonly sigs: readonly Sig[] = []) { }
  push(sig: Sig): Stack { return new Stack([...this.sigs, sig]) }
  select(f: Func): Sig[] { return this.sigs.filter(([g]) => isEqual(g, f)) }
  toString(): string {
    return ['Abstract stack trace:', ...this.sigs.map(([f, ...Ts]) =>
      `${f.toString()}: ${f instanceof Method ? `(${Ts.map(repr).join(', ')})` : repr(Ts[0])}`)].join('\n')
  }
}

class GlobalFrame {
  constructor(
    public type: Anno<Type>,
    readonly deps = new Set<never>(),
    readonly edges = new Set<string>()) { }
  clone(): GlobalFrame { return new GlobalFrame(this.type, new Set(this.deps), new Set(this.edges)) }
}

class Frame {
  readonly key: string
  deps = new Set<string>()
  edges = new Set<string>()
  rettype: Anno<Type> = unreachable
  constructor(readonly sig: Sig, readonly stack: Stack, private _ir?: AIR) { this.key = key(sig) }
  get ir(): AIR { return some(this._ir, `Missing IR for inference frame ${this.key}`) }
  set ir(ir: AIR) { this._ir = ir }
  static create(stack: Stack, ir: MIR, f: Func, ...args: Type[]): Frame {
    return new Frame([f, ...args] as Sig, stack, prepare_ir(ir, args))
  }
  ghost(): Frame {
    const out = new Frame(this.sig, this.stack)
    out.deps = new Set(this.deps)
    out.edges = new Set(this.edges)
    out.rettype = this.rettype
    return out
  }
}

class Inference {
  deps = new Map<string, Set<bigint>>()
  frames = new Map<string, Frame>()
  redirects = new Map<string, Sig>()
  globals = new Map<string, GlobalFrame>()
  queue = new WorkQueue<string>()
  constructor(readonly defs: Definitions, readonly lowered: Lowered, readonly meths: MatchMethods,
    readonly traced?: Traced, readonly transforms?: Inferred) { }
  reuse(ch: this): this {
    this.deps = new Map(Array.from(ch.deps, ([k, deps]) => [k, new Set(deps)]))
    this.frames = new Map(Array.from(ch.frames, ([k, fr]) => [k, fr.ghost()]))
    this.redirects = new Map(ch.redirects)
    this.globals = new Map(Array.from(ch.globals, ([k, fr]) => [k, fr.clone()]))
    this.queue = new WorkQueue<string>()
    return this
  }
}

function resolve(inf: Inference, T: Sig): Sig {
  const to = inf.redirects.get(key(T))
  return to ? resolve(inf, to) : T
}

function lookup(inf: Inference, T: Sig): Frame | undefined {
  return inf.frames.get(key(resolve(inf, T)))
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

function call(inf: Inference, stack: Stack, sig: Sig): Frame {
  const T = resolve(inf, sig)
  const existing = inf.frames.get(key(T))
  if (existing) return existing
  if (T[0] instanceof Method && stack.select(T[0]).length >= recursionLimit)
    return widen(inf, stack, T)
  return frame(inf, stack, T)
}

function frame(inf: Inference, stack: Stack, sig: Sig): Frame {
  const existing = lookup(inf, sig)
  if (existing) return existing
  const [f, ...Ts] = sig
  const k = key(sig)
  const [result, deps] = trackdeps(() => {
    if (f instanceof Method && transformPrimitive(f))
      return inf.transforms?.get([f, ...Ts] as Sig)
    return inf.traced?.trace(f, ...Ts)
  })
  if (result) {
    const [ir, ret] = result
    const fr = Frame.create(stack, ir, f, ...Ts)
    fr.rettype = ret
    inf.deps.set(k, deps)
    inf.frames.set(k, fr)
    return fr
  }
  if (f instanceof Method && !transformPrimitive(f)) {
    const [ir, ideps] = trackdeps(() => inf.lowered.ir(f))
    for (const dep of ideps) deps.add(dep)
    inf.frames.set(k, Frame.create(stack, ir, f, ...Ts))
  } else {
    const tag = f instanceof Method ? f.name : f
    inf.frames.set(k, new Frame(sig, stack, looped(MIR(Def(tag.path)))))
  }
  inf.deps.set(k, deps)
  update(inf, k)
  return some(lookup(inf, sig))
}

function redirect(inf: Inference, from: Sig, to: Frame): void {
  const k = key(from)
  const fr = inf.frames.get(k)
  inf.redirects.set(k, to.sig)
  if (!fr) return
  cleardeps(inf, k)
  inf.frames.delete(k)
  for (const c of fr.edges) {
    const caller = inf.frames.get(c)
    if (!caller) continue
    caller.deps.delete(k)
    caller.deps.add(to.key)
    to.edges.add(c)
    inf.queue.push(c)
  }
}

function unionSig(a: Sig, b: Sig): Sig {
  const [f, ...as] = a
  const [, ...bs] = b
  return [f, ...as.map((x, i) => union(x, bs[i]))] as Sig
}

// TODO some methods become unreachable
function widen(inf: Inference, stack: Stack, F: Sig): Frame {
  const sigs = [...stack.select(F[0]).map(s => resolve(inf, s)), F]
  const widened = sigs.reduce(unionSig)
  const fr = frame(inf, stack, widened)
  for (const s of sigs) if (key(s) !== fr.key) redirect(inf, s, fr)
  return fr
}

function infercall(inf: Inference, P: Sig, F: Func, ...Ts: Anno<Type>[]): Anno<Type> | undefined {
  if (Ts.some(t => t === unreachable)) return unreachable
  if (F instanceof Method) {
    const partial = partialPrimitive(F)
    if (partial) return withTraits(T => traitResult(some(infercall(inf, P, ...traitSig(T)))),
      () => partial(...Ts as Type[]))
  }
  const stack = some(inf.frames.get(key(P))).stack.push(P)
  const fr = call(inf, stack, [F, ...Ts as Type[]] as Sig)
  const caller = inf.frames.get(key(P))
  if (!caller) return
  caller.deps.add(fr.key)
  fr.edges.add(caller.key)
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

function cleardeps(inf: Inference, k: string): void {
  const fr = some(inf.frames.get(k))
  for (const dep of fr.deps) (inf.frames.get(dep) ?? inf.globals.get(dep))?.edges.delete(k)
  fr.deps.clear()
}

function issubset(x: Anno<Type>, y: Anno<Type>): boolean {
  if (x === unreachable) return true
  if (y === unreachable) return false
  return iss(x, y)
}

function settype(inf: Inference, fr: Frame, ret: Anno<Type>): void {
  if (issubset(ret, fr.rettype)) return
  fr.rettype = ret
  for (const s of fr.edges) inf.queue.push(s)
}

function framecode(inf: Inference, fr: Frame, sig: Sig): MIR {
  const callee = call(inf, fr.stack.push(fr.sig), sig)
  fr.deps.add(callee.key)
  callee.edges.add(fr.key)
  return prune(unloop(callee.ir))
}

function lowering(inf: Inference, fr: Frame): Lowering {
  const code: Lowering = {
    ir(f: Func, ...Ts: Type[]): MIR {
      if (!(f instanceof Method)) return striptypes(framecode(inf, fr, [f, ...Ts] as Sig))
      const transform = transformPrimitive(f)
      return transform ? transform(code, f, ...Ts) : inf.lowered.ir(f).clone()
    }
  }
  return code
}

function update_transform(inf: Inference, fr: Frame, f: Method, Ts: Type[], transform: Transform) {
  const [code, deps] = trackdeps(() => transform(lowering(inf, fr), f, ...Ts))
  inf.deps.set(fr.key, deps)
  fr.ir = prepare_ir(code, Ts)
}

function update_dispatcher(inf: Inference, fr: Frame, func: Dispatch, F: Type, Ts: Type) {
  const [[ir, ret], deps] = trackdeps(() => dispatcher(inf, func, F, Ts))
  inf.deps.set(fr.key, deps)
  fr.ir = looped(expand(ir))
  settype(inf, fr, ret)
}

function update(inf: Inference, k: string): void {
  const fr = inf.frames.get(k)
  if (!fr) return
  cleardeps(inf, k)
  const [f, ...Ts] = fr.sig
  if (!(f instanceof Method)) return update_dispatcher(inf, fr, f, Ts[0], Ts[1])
  const transform = transformPrimitive(f)
  if (transform) update_transform(inf, fr, f, Ts, transform)
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
      } else if (ex instanceof XClosure) {
        const Ts = ex.args.map(x => bl.type(x))
        if (Ts.some(t => t === unreachable)) break
        bl.ir.setType(v, Closure(ex.method, ...Ts as Type[]))
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
          if (!tag('common.integer/Bool').isEqual(tagOf(condT))) throw new Error('branch condition must be Bool')
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
  settype(inf, fr, ret)
}

// Invalidate a signature and everything that depends on it.
function remove(inf: Inference, k: string) {
  const fr = inf.frames.get(k) ?? inf.globals.get(k)
  if (fr instanceof Frame) cleardeps(inf, k)
  inf.frames.delete(k)
  inf.globals.delete(k)
  inf.redirects.delete(k)
  inf.deps.delete(k)
  for (const loc of fr?.edges ?? []) remove(inf, loc)
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
    const k = inf.queue.pop()
    try {
      update(inf, k)
    } catch (e) {
      if (partial) break
      const fr = inf.frames.get(k)
      if (!fr) throw e
      throw new CompileError(e, fr.stack.push(fr.sig))
    }
  }
  return inf
}

// Results and caching

function redirectCalls(inf: Inference, code: MIR): MIR {
  const pr = new Pipe(code)
  for (const [v, st] of pr) {
    if (!(st.expr instanceof Invoke)) continue
    if (partialPrimitive(st.expr.method)) continue
    const S = st.expr.body.map(x => asType(pr.type(x)))
    const from: Sig = [st.expr.method, ...S]
    const [_, ...T] = resolve(inf, from)
    if (isEqual(S, T)) continue
    pr.delete(v)
    const args = st.expr.body.map((x, i) =>
      isEqual(S[i], T[i]) ? x : pr.push(pr.stmt(expr('cast', x), { type: T[i], src: st.src })))
    const redirected = pr.push({ ...st, expr: new Invoke(st.expr.method, args) })
    pr.replace(v, redirected)
  }
  return pr.finish()
}

function forward(inf: Inference, sig: Sig): [MIR, Anno<Type>] {
  const [, ...S] = sig
  const [F, ...T] = resolve(inf, sig)
  if (!(F instanceof Method)) throw new Error(`Cannot forward dispatcher: ${key(sig)}`)
  const ret = some(lookup(inf, sig)).rettype
  const code = MIR(Def(`${F.name.path} (forwarder)`))
  const args = S.map((S, i) =>
    code.push(code.stmt(expr<IRValue>('cast', code.argument(S)), { type: T[i] })))
  code.return(code.push(code.stmt(new Invoke(F, args), { type: ret })))
  return [code, ret]
}

class Inferred implements Caching {
  readonly inf: Inference
  results: CacheMap<string, [MIR, Anno<Type>]>
  time = 0n

  constructor(defs: Definitions, lowered: Lowered, meths: MatchMethods, traced?: Traced, transforms?: Inferred) {
    this.inf = new Inference(defs, lowered, meths, traced, transforms)
    this.results = new CacheMap()
  }

  get size() { return this.results.size }
  iscached(k: string): boolean { return this.results.iscached(k) }

  _get(sig: Sig): [MIR, Anno<Type>] {
    const k = key(sig)
    if (this.iscached(k)) return this.results.get(k)!
    // Don't let inference dependencies leak
    const [_, deps] = trackdeps(() => {
      call(this.inf, new Stack(), sig)
      infer(this.inf)
    })
    if (deps.size !== 0) throw new Error('assertion')
    for (const r of this.inf.redirects.keys()) this.results.delete(r)
    for (const [k, fr] of this.inf.frames) {
      if (this.iscached(k)) continue
      this.results.set(k, [redirectCalls(this.inf, prune(unloop(fr.ir))), fr.rettype])
    }
    if (this.inf.redirects.has(k)) this.results.set(k, forward(this.inf, sig))
    return some(this.results.get(k))
  }

  get(sig: Sig): [MIR, Anno<Type>] {
    const [res, t] = withtime(() => this._get(sig))
    this.time += t
    return res
  }

  fingerprint(): Set<bigint> { return fingerprint(this.results) }

  reuse(ch: this): this {
    this.inf.reuse(ch.inf)
    this.results = ch.results.clone()
    return this
  }

  reset(deps: Set<bigint>) {
    for (const [x, d] of this.inf.deps) {
      const sub = Array.from(d).every(id => deps.has(id))
      if (!sub) remove(this.inf, x)
    }
    for (const k of this.results.keys())
      if (!this.inf.frames.has(k)) this.results.delete(k)
  }

  traitType(T: Type): Anno<Type> {
    return traitResult(this.get(traitSig(T))[1])
  }
}

function traitSig(T: Type): [Dispatch, Type, Type] {
  const f = tag('common.patterns/castTrait')
  return [new Dispatch(f), f, pack(tag('common.list/List'), T, Any)]
}

function traitResult(ret: Anno<Type>): Anno<Type> {
  if (ret === unreachable) return unreachable
  for (const option of disjuncts(ret))
    if (tag('common.core/Optional.Some').isEqual(tagOf(option))) return only(parts(option))
  return unreachable
}

const [withTraits, getTraits] = binding<(T: Type) => Anno<Type>>('traits')

function traitType(T: Type): Anno<Type> { return getTraits()(T) }
