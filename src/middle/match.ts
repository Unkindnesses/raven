// Partial Pattern Matching

// Match types (partial values inferred by the interpreter) against user-
// specified patterns at compile time.
// Returns either a dictionary (if the match is certain to succeed), `nothing`
// (if the match is certain to fail) or `missing` (we can't handle the match
// statically).
//
// The returned dictionary contains a mapping from binding names to (type, path)
// pairs, where `path` tells the compiler where to find the matched value in the
// original data. `path` is a list of indexes; the last index might be a
// UnitRange, representing a splat.
//
// Ideally we'd think of dispatchers as a simple list of `if` clauses: for each
// method, check the arg list against the signature via `match(sig, args)`, and
// if the match succeeds call the method. Then we do type inference and remove
// redundant checks using the usual, generic optimisations.
//
// However, `match` is not available until it's defined in the stdlib, for which
// we need function calls (and thus dispatchers, and thus `match`) to work. To
// break the cycle we take a shortcut in some simple cases; if an input obviously
// matches the signature, we'll generate code for the dispatcher that behaves
// like `match`.

import * as types from '../frontend/types.js'
import * as ir from '../utils/ir.js'
import { MIR, IRValue, Dispatch, Method, Definitions } from '../frontend/modules.js'
import { Def } from '../dwarf/index.js'
import { Lowered, xlist, xpart, xcall, xtuple } from '../frontend/lower.js'
import { Pattern, pattern } from '../frontend/patterns.js'
import { Inference, Sig, inferexpr, infercall, issubset, maybe_union } from './abstract.js'
import { some } from '../utils/map.js'
import { options } from '../utils/options.js'
import { part_method, isnil_method, notnil_method, string } from './primitives.js'
import { Cache, Caching, EagerCache, pipe, reset } from '../utils/cache.js'
import { isEqual } from '../utils/isEqual.js'
import { repr } from '../frontend/types.js'
import { Traced } from './tracer.js'

export {
  Interpreter, Methods, matchMethods, MatchMethods, Path, Match, MatchResult, partial_match, indexer, icall,
  dispatch_arms, dispatcherDef, dispatcher
}

type Func = types.Tag | Method

type Path = (number | { start: number, end: number })[]

type Match = Map<string, [types.Type, Path]>
type MatchResult = Match | null | undefined // null is known failure, undefined is unknown

interface Interpreter {
  eval(func: Func, ...args: types.Type[]): types.Type | undefined
}

interface Methods {
  get(key: [Func, types.Type]): [Method, Match | undefined][]
}

function _assoc(as: Match, name: string, [val, path]: [types.Type, Path]): MatchResult {
  if (!as.has(name)) {
    const result = new Map<string, [types.Type, Path]>(as)
    result.set(name, [val, path])
    return result
  }
  const val_ = as.get(name)![0]
  if (types.isValue(types.list(val_, val))) {
    return isEqual(val_, val) ? as : null
  } else {
    return undefined // could reject more cases here
  }
}

function ishole(x: Pattern): boolean {
  if (x.kind === 'hole') return true
  if (x.kind === 'bind') return ishole(x.pattern)
  return false
}

function isslurp(x: Pattern): x is Pattern & { kind: 'repeat' } {
  return x.kind === 'repeat' && ishole(x.pattern)
}

function slurpName(x: Pattern): string | undefined {
  if (x.kind === 'repeat' && x.pattern.kind === 'bind') return x.pattern.name
}

function bound(env: Match | undefined, bs: Match): boolean {
  return bs.size > (env?.size ?? 0)
}

function lookup(env: Match | undefined, name: string): types.Type | undefined {
  const b = env?.get(name)
  return b !== undefined && types.isValue(b[0]) ? b[0] : undefined
}

function resolve(mod: Interpreter, env: Match | undefined, pat: Pattern): types.Type | undefined {
  if (pat.kind !== 'trait') return undefined
  const T = pat.trait.kind === 'const' ? pat.trait.value : lookup(env, pat.trait.name)
  if (T === undefined || pat.args.length === 0) return T
  const args = pat.args.map(x => resolve(mod, env, x))
  if (args.some(x => x === undefined)) return undefined
  const r = mod.eval(types.tag('common/get'), types.list(T, types.list(...args.map(x => some(x)))))
  return r !== undefined && types.isValue(r) ? r : undefined
}

// TODO assumes the value is unchanged by the match
function trivial_isa(int: Interpreter, val: types.Type, T: types.Type): boolean | undefined {
  const r = int.eval(types.tag('common.patterns/matchTrait'), types.list(T, val))
  if (r === undefined) return undefined
  const tag = types.tagOf(r)
  if (types.tag('common.core/Optional.Some').isEqual(tag)) return true
  if (types.tag('common.core/Optional.Nil').isEqual(tag)) return false
  return undefined
}

// TODO match results don't have to be identical, if
// bindings and paths are right we can merge types.
function partial_match_union(mod: Interpreter, env: Match | undefined, pat: Pattern, val: types.Type & { kind: 'union' }, path: Path): MatchResult {
  const ms = val.options.map(x => _partial_match(mod, env, pat, x, path))
  if (ms.some(x => x === undefined)) return undefined
  if (ms.every((m, i) => isEqual(m, ms[0]))) return ms[0]
  return undefined
}

function partial_match_pack(mod: Interpreter, bs: Match | undefined, pat: Pattern & { kind: 'pack' }, val: types.Type, path: Path): MatchResult {
  let i = 0
  while (true) {
    if (i > types.nparts(val)) break
    if (i > pat.parts.length - 1) return null
    if (isslurp(pat.parts[i])) {
      const name = slurpName(pat.parts[i])
      if (!name || bs === undefined) return bs
      const remaining = types.allparts(val).slice(i)
      const range = { start: i, end: types.nparts(val) }
      // TODO slurps may not be at the end
      return _assoc(bs, name, [types.list(...remaining), [...path, range]])
    }
    if (pat.parts[i].kind === 'repeat') return undefined
    const b = _partial_match(mod, bs, pat.parts[i], types.part(val, i), [...path, i])
    // continue on `missing`, since we might narrow to `nothing` later
    if (b === null) return null
    bs = b
    i += 1
  }
  if (pat.parts.length - 1 === i && isslurp(pat.parts[i])) {
    const name = slurpName(pat.parts[i])
    if (!name || bs === undefined) return bs
    const range = { start: i, end: 0 }
    return _assoc(bs, name, [types.list(), [...path, range]])
  } else if (pat.parts.length - 1 > types.nparts(val))
    return null
  return bs
}

function partial_match_vpack(mod: Interpreter, env: Match | undefined, pat: Pattern & { kind: 'pack' }, val: types.Type & { kind: 'vpack' }, path: Path): MatchResult {
  const bs = _partial_match(mod, env, pat.parts[0], types.tagOf(val), [...path, 0])
  if (bs === null || bs === undefined) return bs
  if (bound(env, bs)) return undefined
  if (pat.parts.length !== 2 || pat.parts[1].kind !== 'repeat') return undefined
  const innerPat = pat.parts[1].pattern
  const [b, r] = innerPat.kind === 'bind' ? [innerPat.name, innerPat.pattern] : [undefined, innerPat]
  const bs2 = _partial_match(mod, bs, r, types.partial_eltype(val), path)
  if (bs2 === null || bs2 === undefined) return bs2
  if (bound(bs, bs2)) return undefined
  return b === undefined ? bs : _assoc(bs, b, [val, path])
}

function _partial_match(mod: Interpreter, env: Match | undefined, pat: Pattern, val: types.Type, path: Path): MatchResult {
  switch (pat.kind) {
    case 'hole':
      return env

    case 'literal': // TODO use assoc
      if (types.isdisjoint(pat.value, val)) return null
      else if (types.isValue(val) && types.isValue(pat.value)) {
        return env
      } else {
        return undefined
      }

    case 'bind':
      const bs = _partial_match(mod, env, pat.pattern, val, path)
      if (bs === null || bs === undefined) return bs
      return _assoc(bs, pat.name, [val, path])

    case 'trait':
      const T = resolve(mod, env, pat)
      if (T === undefined) return undefined
      const r = trivial_isa(mod, val, T)
      return r === true ? env : r === false ? null : undefined

    case 'or':
      if (val.kind === 'recursive') return _partial_match(mod, env, pat, types.unroll(val), path)
      if (val.kind === 'union') return partial_match_union(mod, env, pat, val, path)
      for (const p of pat.patterns) {
        const m = _partial_match(mod, env, p, val, path)
        if (m === null) continue
        if (m === undefined) return undefined
        return m
      }
      return null

    case 'constructor': {
      const result = mod.eval(types.tag('common.patterns/constructorPattern'), types.list(...types.parts(pat.value)))
      if (!result || !types.isValue(result)) return undefined
      return _partial_match(mod, env, pattern(result), val, path)
    }

    case 'pack':
      if (types.isAtom(val) || val.kind === 'closure' || val.kind === 'pack') {
        return partial_match_pack(mod, env, pat, val, path)
      } else if (val.kind === 'vpack') {
        return partial_match_vpack(mod, env, pat, val, path)
      } else if (val.kind === 'union') {
        return partial_match_union(mod, env, pat, val, path)
      } else if (val.kind === 'recursive') {
        return _partial_match(mod, env, pat, types.unroll(val), path)
      } else if (val.kind === 'any') {
        return undefined
      } else {
        throw new Error('unimplemented')
      }

    case 'repeat':
    case 'and':
      return undefined

    default:
      pat satisfies never
      throw new Error('unreachable')
  }
}

function partial_match(mod: Interpreter, pat: Pattern, val: types.Type, path: Path = []): MatchResult {
  return _partial_match(mod, new Map(), pat, val, path)
}

// Filtered methods

function matchMethods(defs: Definitions, interp: Interpreter, [f, Ts]: [Func, types.Type]) {
  const result: [Method, Match | undefined][] = []
  const methods = f instanceof Method ? [f] : defs.methods(f)
  for (const meth of methods.slice().reverse()) {
    const P = interp.eval(meth.signature)
    const m = P !== undefined && types.isValue(P)
      ? partial_match(interp, pattern(P), Ts)
      : undefined
    if (m === null) continue
    result.push([meth, m])
    if (m !== undefined) break
  }
  return result
}

class MatchMethods implements Caching, Methods {
  readonly interp: Traced
  readonly cache: EagerCache<[types.Tag, types.Type], [Method, Match | undefined][]>
  readonly meths: Cache<[Method, types.Type], [Method, Match | undefined][]>

  constructor(defs: Definitions, lowered: Lowered) {
    this.interp = Traced.create(defs, lowered)
    this.cache = new EagerCache(key => matchMethods(defs, this.interp, key))
    this.meths = new Cache(key => matchMethods(defs, this.interp, key))
  }

  get subcaches(): Caching[] { return [this.interp, this.cache, this.meths] }
  reset(deps: Set<bigint>) { reset(pipe(this.interp, this.cache, this.meths), deps) }
  get(key: [Func, types.Type]) {
    return key[0] instanceof Method
      ? this.meths.get([key[0], key[1]])
      : this.cache.get([key[0], key[1]])
  }
}

// Generate dispatchers

function dispatch_arms(T: types.Type): types.Type[] {
  if (T.kind === 'union') return T.options
  if (T.kind === 'recursive') return dispatch_arms(types.unroll(T))
  if (T.kind === 'pack') {
    let result: types.Type[][] = [[]]
    for (const part of T.parts.map(dispatch_arms))
      result = result.flatMap(prefix => part.map(x => [...prefix, x]))
    return result.map(parts => types.pack(...parts))
  }
  return [T]
}

type Push = (code: ir.Fragment<MIR>, ex: ir.Expr<IRValue>, T: types.Type) => ir.Val<MIR>

function indexer(code: ir.Fragment<MIR>, T: types.Type, arg: ir.Val<MIR>, path: Path, push?: Push): ir.Val<MIR> {
  push ??= (code, ex, T) => code.push(code.stmt(ex, { type: T }))
  if (path.length === 0) return arg
  const [p, ...rest] = path
  if (typeof p !== 'number') {
    const ps: ir.Val<MIR>[] = []
    for (let i = p.start; i <= p.end; i++)
      ps.push(push(code, xpart(arg, types.Type(BigInt(i))), types.part(T, i)))
    const L = types.list(...ps.map(v => ir.asType(code.type(v))))
    arg = push(code, xlist<IRValue>(...ps), L)
  } else {
    T = types.part(T, p)
    arg = push(code, xpart(arg, types.Type(BigInt(p))), T)
  }
  return indexer(code, T, arg, rest, push)
}

function icall(inf: Inference, code: MIR, sig: Sig, f: IRValue | Method, ...args: (IRValue | number)[]): ir.Val<MIR> {
  if (!(f instanceof Method))
    args = [code.push(code.stmt(xlist(...args),
      { type: types.list(...args.map(a => ir.asType(code.type(a)))) }))]
  const ex = xcall(f, ...args)
  const T = inferexpr(inf, sig, code, ex)
  return code.push(code.stmt(ex, { type: T }))
}

function dispatcherDef(func: Dispatch) {
  return Def(`${func.path} (dispatcher)`)
}

function dispatcher(inf: Inference, func: Dispatch, F: types.Type, Ts: types.Type): [MIR, ir.Anno<types.Type>] {
  const code = MIR(dispatcherDef(func))
  let f: ir.Val<MIR> = code.argument(F)
  const args = code.argument(Ts)
  if (F.kind === 'closure') {
    const P = types.pack(F.method.name, ...F.parts)
    f = types.isValue(P) ? P : code.push(code.stmt(xtuple<IRValue>(f), { type: P }))
  }
  const fullType = types.list(ir.asType(code.type(f)), Ts)
  const full = code.push(code.stmt(xlist<IRValue>(f, args), { type: fullType }))
  let ret: ir.Anno<types.Type> = ir.unreachable
  let arms = dispatch_arms(fullType)
  const sig: Sig = [func, F, Ts]
  const call = (f: IRValue | Method, ...as: (IRValue | number)[]) => icall(inf, code, sig, f, ...as)
  const adapt = (meth: Method, result: ir.Val<MIR>): ir.Val<MIR> => {
    if (func.swap === meth.swaps || code.type(result) === ir.unreachable) return result
    return func.swap
      ? code.push(code.stmt(xlist(result), { type: types.list(ir.asType(code.type(result))) }))
      : call(part_method, result, types.Type(1n))
  }
  for (const [meth, m] of inf.meths.get([F.kind === 'closure' ? F.method : func.func, fullType])) {
    const pat = call(meth.signature)
    if (code.type(pat) === ir.unreachable) { code.block().unreachable(); return [code, ret] }
    if (m === undefined) {
      const P = ir.asType(code.type(pat))
      const match = types.tag('common.patterns/match')
      arms = arms.filter(T =>
        issubset(types.nil, some(infercall(inf, sig, new Dispatch(match), match, types.list(T, P)))))
      let m = call(match, full, pat)
      if (code.type(m) === ir.unreachable) { code.block().unreachable(); return [code, ret] }
      const cond = call(isnil_method, m)
      code.branch(code.blockCount + 2, [], { when: cond })
      code.branch(code.blockCount + 1)
      code.newBlock()
      m = call(notnil_method, m)
      if (code.type(m) !== ir.unreachable) {
        const as = meth.sig.args.map(arg => call(types.tag('common.record/getkey'), m, types.tag(arg)))
        const result = adapt(meth, call(meth, ...as))
        code.return(result)
        ret = maybe_union(ret, code.type(result))
      }
      code.newBlock()
    } else { // certain to match
      const as = meth.sig.args.map(x => indexer(code, fullType, full, some(m.get(x))[1]))
      const result = adapt(meth, call(meth, ...as))
      if (code.type(result) === ir.unreachable) {
        code.block().unreachable()
        return [code, ret]
      }
      code.return(result)
      ret = maybe_union(ret, code.type(result))
      return [code, ret]
    }
    if (arms.length === 0) {
      code.block().unreachable()
      return [code, ret]
    }
  }
  if (types.tag('common/abort').isEqual(func.func) && types.issubset(Ts, types.list(types.String())))
    throw new Error("Compiler fault: couldn't guarantee abort method matches")
  if (options().jspanic)
    call(types.tag('common/abort'), string(code, `No matching method: ${repr(F)}: ${types.repr(Ts)}`))
  code.block().unreachable()
  return [code, ret]
}
