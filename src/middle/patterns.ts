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

import * as types from '../frontend/types'
import * as ir from '../utils/ir'
import { MIR, IRValue, Method, Definitions } from '../frontend/modules'
import { Def } from '../dwarf'
import { xlist, xpart, xcall } from '../frontend/lower'
import { Pattern, patternType, pattern } from '../frontend/patterns'
import { Inference, Sig, inferexpr, infercall, issubset, maybe_union } from './abstract'
import { some } from '../utils/map'
import { options } from '../utils/options'
import { part_method, isnil_method, notnil_method, partial_isnil, string } from './primitives'
import { EagerCache } from '../utils/cache'
import isEqual from 'lodash/isEqual'

export { MatchMethods, partial_match, indexer, icall, dispatch_arms, dispatcher }

type Path = (number | { start: number, end: number })[]

type Match = Map<string, [types.Type, Path]>
type MatchResult = Match | null | undefined // null is known failure, undefined is unknown

interface Interpreter {
  get(func: types.Tag, args: types.Type[]): types.Type | undefined
}

function _assoc(as: Match, name: string, [val, path]: [types.Type, Path]): MatchResult {
  if (as === null || as === undefined) return as
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

function _merge(as: Match | undefined, bs: Match | undefined): MatchResult {
  if (as === undefined || bs === undefined) return undefined
  let m: MatchResult = as
  for (const [k, v] of bs) {
    m = _assoc(m, k, v)
    if (m === null || m === undefined) return m
  }
  return m
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

// TODO match results don't have to be identical, if
// bindings and paths are right we can merge types.
function partial_match_union(mod: Interpreter, pat: Pattern, val: types.Type & { kind: 'union' }, path: Path): MatchResult {
  const ms = val.options.map(x => partial_match(mod, pat, x, path))
  if (ms.some(x => x === undefined)) return undefined
  if (ms.every((m, i) => isEqual(m, ms[0]))) return ms[0]
  return undefined
}

function partial_match_pack(mod: Interpreter, pat: Pattern & { kind: 'pack' }, val: types.Type, path: Path): MatchResult {
  let bs: Match | undefined = new Map()
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
    const b = partial_match(mod, pat.parts[i], types.part(val, i), [...path, i])
    // continue on `missing`, since we might narrow to `nothing` later
    if (b === null) return null
    const cs = _merge(bs, b)
    if (cs === null) return null
    bs = cs
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

function partial_match_vpack(mod: Interpreter, pat: Pattern & { kind: 'pack' }, val: types.Type & { kind: 'vpack' }, path: Path): MatchResult {
  const bs = partial_match(mod, pat.parts[0], types.tagOf(val), [...path, 0])
  if (bs === null || bs === undefined) return bs
  if (bs.size > 0) return undefined
  if (pat.parts.length !== 2 || pat.parts[1].kind !== 'repeat') return undefined
  const innerPat = pat.parts[1].pattern
  const [b, r] = innerPat.kind === 'bind' ? [innerPat.name, innerPat.pattern] : [undefined, innerPat]
  const bs2 = partial_match(mod, r, types.partial_eltype(val), path)
  if (bs2 === null || bs2 === undefined) return bs2
  if (bs2.size > 0) return undefined
  return b === undefined ? bs : _assoc(bs, b, [val, path])
}

function partial_match(mod: Interpreter, pat: Pattern, val: types.Type, path: Path = []): MatchResult {
  switch (pat.kind) {
    case 'hole':
      return new Map()

    case 'literal': // TODO use assoc
      if (types.isValue(val) && types.isValue(pat.value)) {
        return isEqual(pat.value, val) ? new Map() : null
      } else {
        return undefined // could reject more cases here
      }

    case 'bind':
      const bs = partial_match(mod, pat.pattern, val, path)
      if (bs === null || bs === undefined) return bs
      return _assoc(bs, pat.name, [val, path])

    case 'trait':
      const r = trivial_isa(mod, val, pat.trait)
      return r === true ? new Map() : r === false ? null : undefined

    case 'or':
      if (val.kind === 'recursive') return partial_match(mod, pat, types.unroll(val), path)
      if (val.kind === 'union') return partial_match_union(mod, pat, val, path)
      for (const p of pat.patterns) {
        const m = partial_match(mod, p, val, path)
        if (m === null) continue
        if (m === undefined) return undefined
        return m
      }
      return null

    case 'constructor': {
      const patType = patternType(pat)
      const result = mod.get(types.tag('common.constructorPattern'), [types.list(...types.parts(patType))])
      if (!result || !types.isValue(result)) return undefined
      return partial_match(mod, pattern(types.part(result, 1)), val, path)
    }

    case 'pack':
      if (types.isAtom(val) || val.kind === 'pack') {
        return partial_match_pack(mod, pat, val, path)
      } else if (val.kind === 'vpack') {
        return partial_match_vpack(mod, pat, val, path)
      } else if (val.kind === 'union') {
        return partial_match_union(mod, pat, val, path)
      } else if (val.kind === 'recursive') {
        return partial_match(mod, pat, types.unroll(val), path)
      } else {
        throw new Error('unimplemented')
      }

    case 'repeat':
    case 'and':
      return undefined

    default:
      const _: never = pat
      throw new Error('unreachable')
  }
}

// TODO assumes the value is unchanged by the match
function trivial_isa(int: Interpreter, val: types.Type, T: types.Type): boolean | undefined {
  const r = int.get(types.tag('common.matchTrait'), [types.list(T, val)])
  if (r === undefined) return undefined
  const tag = types.tagOf(types.part(r, 1))
  if (isEqual(tag, types.tag('common.Some'))) return true
  if (isEqual(tag, types.tag('common.Nil'))) return false
  return undefined
}

// Filtered methods

function MatchMethods(defs: Definitions, interp: Interpreter) {
  return new EagerCache<[types.Tag, types.Type], [Method, Match | undefined][]>(([f, Ts]) => {
    const result: [Method, Match | undefined][] = []
    const methods = defs.methods(f)
    for (const meth of methods.slice().reverse()) {
      const m = partial_match(interp, meth.sig.pattern, Ts)
      if (m === null) continue
      result.push([meth, m])
      if (m !== undefined) break
    }
    return result
  })
}

type MatchMethods = ReturnType<typeof MatchMethods>

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

function indexer(code: MIR, T: types.Type, arg: number, path: Path): number {
  if (path.length === 0) return arg
  const [p, ...rest] = path
  if (typeof p !== 'number') {
    const ps: number[] = []
    for (let i = p.start; i <= p.end; i++)
      ps.push(code.push(code.stmt(xpart(arg, types.Type(BigInt(i))), { type: types.part(T, i) })))
    const L = types.list(...ps.map(v => code.type(v) as types.Type))
    arg = code.push(code.stmt(xlist(...ps), { type: L }))
  } else {
    T = types.part(T, p)
    arg = code.push(code.stmt(xpart(arg, types.Type(BigInt(p))), { type: T }))
  }
  return indexer(code, T, arg, rest)
}

function icall(inf: Inference, code: MIR, sig: Sig, f: IRValue, ...args: (IRValue | number)[]): number {
  if (!(f instanceof Method))
    args = [code.push(code.stmt(xlist(...args),
      { type: types.list(...args.map(a => types.asType(code.type(a)))) }))]
  const ex = xcall(f, ...args)
  const T = inferexpr(inf, sig, code, ex)
  return code.push(code.stmt(ex, { type: T }))
}

function dispatcher(inf: Inference, func: types.Tag, Ts: types.Type): [MIR, ir.Anno<types.Type>] {
  const code = MIR(Def(func.path, undefined, true))
  const args = code.argument(Ts)
  let ret: ir.Anno<types.Type> = ir.unreachable
  let arms = dispatch_arms(Ts)
  const call = (f: IRValue, ...as: (IRValue | number)[]) => icall(inf, code, [func, Ts], f, ...as)
  for (const [meth, m] of inf.meths.get([func, Ts])) {
    if (m === undefined) {
      const pat = patternType(meth.sig.pattern)
      arms = arms.filter(T =>
        issubset(types.list(types.nil), some(infercall(inf, [func, Ts], types.tag('common.match'), types.list(T, pat)))))
      let m = call(types.tag('common.match'), args, pat)
      if (code.type(m) === ir.unreachable) { code.block().unreachable(); return [code, ret] }
      m = call(part_method, m, types.Type(1n))
      // TODO use call?
      const cond = code.push(code.stmt(xcall(isnil_method, m), { type: partial_isnil(types.asType(code.type(m))) }))
      code.branch(code.blockCount + 2, [], { when: cond })
      code.branch(code.blockCount + 1)
      code.newBlock()
      m = call(notnil_method, m)
      const as: number[] = []
      for (const arg of meth.sig.args)
        as.push(call(part_method, call(types.tag('common.getkey'), m, types.tag(arg)), types.Type(1n)))
      let result = call(meth, ...as)
      if (meth.sig.swap.size === 0 && code.type(result) !== ir.unreachable)
        result = code.push(code.stmt(xlist(result), { type: types.list(types.asType(code.type(result))) }))
      code.return(result)
      ret = maybe_union(ret, ir.asAnno(types.asType, code.type(result)))
      code.newBlock()
    } else { // certain to match
      const as = meth.sig.args.map(x => indexer(code, Ts, args, some(m.get(x))[1]))
      let result = call(meth, ...as)
      if (code.type(result) === ir.unreachable) {
        code.block().unreachable()
        return [code, ret]
      }
      if (meth.sig.swap.size === 0)
        result = code.push(code.stmt(xlist(result), { type: types.list(types.asType(code.type(result))) }))
      code.return(result)
      ret = maybe_union(ret, ir.asAnno(types.asType, code.type(result)))
      return [code, ret]
    }
    if (arms.length === 0) {
      code.block().unreachable()
      return [code, ret]
    }
  }
  if (isEqual(func, types.tag('common.abort')) && types.issubset(Ts, types.list(types.String())))
    throw new Error("Compiler fault: couldn't guarantee abort method matches")
  if (options().jspanic)
    call(types.tag('common.abort'), string(code, `No matching method: ${func}: ${types.repr(Ts)}`))
  code.block().unreachable()
  return [code, ret]
}
