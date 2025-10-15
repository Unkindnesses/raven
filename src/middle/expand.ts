// After type inference, we expand to a form where reference and union types are
// represented with explicit pointers and tags, eg a `VPack` becomes a (size,
// pointer) tuple. This allows us to compile methods like `part`, `packcat` and
// casts, whose implementation needs to access those internals.
//
// It makes sense for those methods to be represented as functions, to avoid
// code duplication and so that casting can be recursive. And they can still
// participate in optimisations (mainly inlining).
//
// After this expansion all code works with primitive values (or flat tuples of
// primitives) and does explicit memory management, so the job of the backend
// code generator is simple.
//
// We also insert primitive retain/release instructions here. The RC algorithm
// will only release just after a variable is created, and then only if there are
// no uses. The `release` expression tells it to try again, and is ignored if the
// variable is used later; it should be used liberally by any code that works
// with internals. `retain` is mainly used by indexing.

import * as ir from '../utils/ir'
import * as types from '../frontend/types'
import { Type, asType } from '../frontend/types'
import { Type as WType, sizeof as wsizeof } from '../wasm/wasm'
import { MIR, WImport, WIntrinsic, Method, FuncInfo, Const, asBinding, asFunc, xstring } from '../frontend/modules'
import { Inferred, Redirect, type Sig, sig as resolveSig } from './abstract'
import { wasmPartials } from '../backend/wasm'
import isEqual from 'lodash/isEqual'
import { Pipe, Block, Fragment, stmt, expr, Branch, Val, Anno, unreachable } from '../utils/ir'
import { some } from '../utils/map'
import { xcall, xtuple } from '../frontend/lower'
import { isreftype } from './refcount'
import { partial_part, getIntValue, nparts, constValue } from './primitives'
import { inlinePrimitive, outlinePrimitive } from './prim_map'
import { Cache } from '../utils/cache'

export { abort, call, layout, sizeof, store, load, box, unbox, union_downcast, union_cases, cast, partir, packir, indexer, Expanded }

const i32 = Const.i32
const i64 = Const.i64

// Unions

function trim_unreachable(code: MIR): MIR {
  const pr = new Pipe(code)
  for (const bl of pr.blocks()) {
    let flag = false
    for (const [v, st] of bl) {
      if (flag) pr.delete(v)
      else if (st.expr instanceof Branch) {
        const br = st.expr
        if (!br.isconditional()) {
          flag = true
        } else {
          const cond = types.asType(pr.type(br.when))
          if (!types.issubset(cond, types.bool())) throw new Error('non-bool condition')
          if (isEqual(cond, types.bool(false))) {
            pr.delete(v)
          } else if (isEqual(cond, types.bool(true))) {
            flag = true
            pr.set(v, new Branch(br.target, br.args))
          }
        }
      } else if (st.type === ir.unreachable) {
        pr.push(stmt(Branch.unreachable()))
        flag = true
      }
    }
  }
  return pr.finish()
}

function union_downcast(pr: MIR | Pipe<MIR>, T: Type, i: number, x: Val<MIR>): number {
  const U = asType(T, 'union')
  const offset = 1 + U.options.slice(0, i - 1).reduce((n, t) => n + layout(t).length, 0)
  const regs = layout(U.options[i - 1]).length
  const parts: Val<MIR>[] = []
  for (let j = 1; j <= regs; j++) parts.push(pr.push(stmt(expr('ref', x, i64(offset + j)))))
  return pr.push(stmt(expr('tuple', ...parts), { type: U.options[i - 1] }))
}

// `f` is reponsible for freeing its argument value, but not for freeing `x`
// (since they are the same object)
function union_cases(code: MIR, T: Type & { kind: 'union' }, x: Val<MIR>, f: (S: Type, val: Val<MIR>) => Val<MIR>): MIR {
  const j = code.push(stmt(expr('ref', x, i64(1)), { type: types.bits(32) }))
  for (let caseIdx = 1; caseIdx <= T.options.length; caseIdx++) {
    const cond = code.push(stmt(xcall(new WIntrinsic('i32.eq'), j, i32(caseIdx)), { type: types.bool() }))
    const before = code.block()
    const body = code.newBlock()
    const val = union_downcast(code, T, caseIdx, x)
    const ret = f(T.options[caseIdx - 1], val)
    code.return(ret)
    const after = code.newBlock()
    before.branch(body, [], { when: cond })
    before.branch(after)
  }
  code.block().unreachable()
  return code
}

// Panic

function abort(code: Fragment<MIR>, s: string): number {
  const id = code.push(ir.stmt(xstring(s), { type: types.bits(32) }))
  return code.push(ir.stmt(xcall(new WImport('support', 'abort'), id)))
}

// We're a bit fast and loose with types here, because `T` and `[T]` have the
// same representation (for now).
function call(code: Fragment<MIR>, f: Val<MIR>, args: Val<MIR>[], type: Anno<Type>): number {
  const Ts = args.map(a => types.asType(code.type(a)))
  const arglist = code.push(ir.stmt(xtuple(...args), { type: types.list(...Ts) }))
  return code.push(ir.stmt(xcall(f, arglist), { type }))
}

// Pack primitives

function layout(T: Type): WType[] {
  if (types.isValue(T)) return []
  switch (T.kind) {
    case 'float32': return [WType.f32]
    case 'float64': return [WType.f64]
    case 'bits': {
      if (T.size <= 32) return [WType.i32]
      if (T.size <= 64) return [WType.i64]
      throw new Error(`Unsupported bit size ${T.size}`)
    }
    case 'pack': return T.parts.flatMap(layout)
    case 'vpack': return [ // size, pointer
      ...layout(types.tagOf(T)), WType.i32,
      ...(layout(some(types.partial_eltype(T))).length === 0 ? [] : [WType.i32])
    ]
    case 'recursive': return [WType.i32]
    case 'union': return [WType.i32, ...T.options.flatMap(layout)]
    case 'tag': return []
    case 'recurrence': throw new Error('unimplemented')
    default: { const _: never = T; throw new Error('unreachable') }
  }
}

function sublayout(T: Type, i: number): number[] {
  const before = types.pack(...types.allparts(T).slice(0, i))
  const offset = layout(before).length
  const len = layout(types.allparts(T)[i]).length
  const out: number[] = []
  for (let k = 1; k <= len; k++) out.push(offset + k)
  return out
}

function sizeof(T: Type): number {
  return layout(T).reduce((n, t) => n + wsizeof(t), 0)
}

// part
// ====

function partir_union(x: Type & { kind: 'union' }, i: Type): MIR {
  const code = MIR(new FuncInfo(types.tag('common.core.part')))
  const retT = partial_part(x, i)
  const vx = code.argument(x)
  const vi = code.argument(i)
  union_cases(code, x, vx, (T, val) => {
    // TODO possibly insert `part_method` calls and redo lowering
    let ret = indexer(code, T, i, val, vi)
    if (partial_part(T, i) === unreachable) return ret // TODO insert unreachable?
    if (isreftype(asType(partial_part(T, i)))) code.push(stmt(expr('retain', ret)))
    if (isreftype(T)) code.push(stmt(expr('release', val)))
    ret = cast(code, partial_part(T, i), retT, ret)
    return ret
  })
  return code
}

// Create a `part` method to dynamically index tuples allocated as registers.
// TODO: should make sure this comes out as a switch / branch table.
function partir(x: Type, i: Type): MIR {
  if (x.kind === 'union') return partir_union(x, i)
  if (!isEqual(types.tagOf(i), types.tag('common.Int'))) throw new Error('partir: expected Int index')
  const T = partial_part(x, i)
  const code = MIR(new FuncInfo(types.tag('common.core.part')))
  const vx = code.argument(x)
  const vi = code.argument(i)
  const xlayout = layout(x)
  const part = (k: number): Val<MIR> =>
    code.push(stmt(expr('ref', vx, i64(k)), { type: [xlayout[k - 1]] }))
  for (let idx = 1; idx <= types.nparts(x); idx++) {
    const cond = call(code, types.tag('common.=='), [i64(idx), vi], types.bool())
    const before = code.block()
    const body = code.newBlock()
    // TODO: recurse to `indexer` here, let casting happen later
    const range = sublayout(x, idx)
    const caseT = partial_part(x, types.int64(idx))
    if (caseT === unreachable) throw new Error('partir: unreachable case')
    let y = code.push(stmt(expr('tuple', ...range.map(part)), { type: caseT }))
    code.return(cast(code, caseT, T, y))
    const after = code.newBlock()
    before.branch(body, [], { when: cond })
    before.branch(after)
  }
  abort(code, `Invalid index for ${types.repr(x)}`)
  code.block().unreachable()
  return code
}

function vpack_indexer(pr: Fragment<MIR>, Ts: Type, I: Type, x: Val<MIR>, i: Val<MIR>): Val<MIR> {
  const T = some(types.partial_eltype(Ts))
  const idx = getIntValue(I)
  if (idx === 0 || layout(T).length === 0) return pr.push(stmt(expr('tuple')))
  if (idx !== undefined)
    i = i32((idx - 1) * sizeof(T))
  else {
    i = call(pr, types.tag('common.Int32'), [i], types.int32())
    i = call(pr, types.tag('common.-'), [i, i32(1)], types.int32())
    i = call(pr, types.tag('common.*'), [i, i32(sizeof(T))], types.int32())
  }
  // TODO bounds check
  let p = pr.push(stmt(expr('ref', x, i64(2)), { type: types.int32() }))
  p = call(pr, types.tag('common.+'), [p, i], types.int32())
  return load(pr, T, p, { count: false })
}

function indexer(pr: Fragment<MIR>, T: Type, I: Type, x: Val<MIR>, i: Val<MIR>): Val<MIR> {
  const idx = getIntValue(I)
  if (types.isAtom(T) && idx !== undefined) {
    if (idx > 1)
      return abort(pr, `Invalid index ${idx} for ${types.repr(T)}`)
    if (types.isValue(types.part(T, idx)))
      return pr.push(stmt(expr('tuple')))
    if (isEqual(T, types.float64()))
      return pr.push(stmt(xcall(new WIntrinsic('i64.reinterpret_f64'), x), { type: types.bits(64) }))
    if (isEqual(T, types.float32()))
      return pr.push(stmt(xcall(new WIntrinsic('i32.reinterpret_f32'), x), { type: types.bits(32) }))
    return x
  } else if (T.kind === 'vpack') {
    return vpack_indexer(pr, T, I, x, i)
  } else if (T.kind === 'pack' && idx !== undefined) {
    if (!(0 <= idx && idx <= types.nparts(T)))
      return abort(pr, `Invalid index ${idx} for ${types.repr(T)}`)
    const P = types.part(T, idx)
    const range = sublayout(T, idx)
    const _part = (k: number): Val<MIR> => pr.push(stmt(expr('ref', x, i64(k)), { type: [layout(T)[k - 1]] }))
    return pr.push(stmt(expr('tuple', ...range.map(_part)), { type: P }))
  } else {
    throw new Error('unimplemented')
  }
}

// packcat
// =======

function packir(Ts: Type): MIR {
  if (!(Ts.kind === 'pack')) throw new Error('nope')
  const T = types.packcat(...types.parts(Ts))
  const U = types.unroll(T)
  if (!(U.kind === 'vpack') || !(types.tagOf(T).kind === 'tag')) throw new Error('nope')
  const E = some(types.partial_eltype(U))
  const code = MIR(new FuncInfo(types.tag('common.core.packcat')))
  const xs = code.argument(Ts)
  const ps: Val<MIR>[] = []
  for (let i = 1; i <= types.nparts(Ts); i++)
    ps.push(indexer(code, Ts, types.int64(i), xs, types.int64(i)))
  const ls: Val<MIR>[] = []
  for (let i = 1; i <= types.nparts(Ts); i++) {
    let l = nparts(code, types.part(Ts, i), ps[i - 1])
    const lv = getIntValue(asType(code.type(l)))
    if (lv !== undefined) l = i64(lv)
    ls.push(l)
  }
  let size = ls.shift()!
  for (const l of ls)
    size = call(code, types.tag('common.+'), [size, l], types.int64())
  size = call(code, types.tag('common.Int32'), [size], types.int32())
  if (T.kind === 'vpack' && sizeof(E) === 0) {
    code.return(code.push(stmt(xtuple(size), { type: T })))
    return code
  }
  const bytes = call(code, types.tag('common.*'), [size, i32(sizeof(E))], types.int32())
  const ptr = call(code, types.tag('common.malloc!'), [bytes], types.int32())
  let pos: Val<MIR> = ptr
  for (let i = 1; i <= types.nparts(Ts); i++) {
    let P = types.part(Ts, i)
    if (P.kind === 'recursive') {
      P = types.unroll(P)
      ps[i - 1] = unbox(code, P, ps[i - 1])
    }
    if (P.kind === 'pack') {
      for (let j = 1; j <= types.nparts(P); j++) {
        let x = indexer(code, P, types.int64(j), ps[i - 1], types.int64(j))
        x = cast(code, types.part(P, j), E, x)
        store(code, E, pos, x)
        pos = call(code, types.tag('common.+'), [pos, i32(sizeof(E))], types.int32())
      }
    } else if (P.kind === 'vpack') {
      // TODO memcpy when possible
      if (sizeof(some(types.partial_eltype(P))) === 0) throw new Error('nope')
      let len = code.push(stmt(expr('ref', ps[i - 1], i64(1)), { type: types.int32() }))
      let src = code.push(stmt(expr('ref', ps[i - 1], i64(2)), { type: types.int32() }))

      const before = code.block()
      const header = code.newBlock()
      const body = code.newBlock()
      const after = code.newBlock()

      before.branch(header, [pos, src, len])
      pos = header.argument(types.int32())
      src = header.argument(types.int32())
      len = header.argument(types.int32())
      const done = call(header, types.tag('common.=='), [len, i32(0)], types.int32())
      header.branch(after, [], { when: done })
      header.branch(body)

      let x = load(body, some(types.partial_eltype(P)), src)
      x = cast(body, some(types.partial_eltype(P)), E, x)
      store(body, E, pos, x)
      const pos2 = call(body, types.tag('common.+'), [pos, i32(sizeof(E))], types.int32())
      const src2 = call(body, types.tag('common.+'), [src, i32(sizeof(E))], types.int32())
      const len2 = call(body, types.tag('common.-'), [len, i32(1)], types.int32())
      body.branch(header, [pos2, src2, len2])

      code.push(stmt(expr('release', ps[i - 1])))
    } else
      throw new Error('unsupported')
  }
  let result = code.push(stmt(xtuple(size, ptr), { type: U }))
  if (T.kind === 'recursive') {
    result = box(code, U, result)
    result = code.push(stmt(xtuple(result), { type: T })) // TODO remove
  }
  code.return(result)
  return code
}

// Expansion pass

function lowerdata(code: MIR): MIR {
  const pr = new Pipe(code)
  for (const [v, st] of pr) {
    const ex = st.expr
    if (ex.head === 'pack') {
      // remove constants, which have zero width
      const args = ex.body.filter(x => typeof x === 'number')
      pr.set(v, xtuple(...args))
    } else if (ex.head === 'call') {
      const callee = ex.body[0]
      if (callee instanceof WIntrinsic) {
        if (wasmPartials.has(callee.name) && types.isValue(asType(st.type)))
          pr.set(v, expr('tuple'))
      } else {
        const F = pr.type(callee)
        if (F instanceof Method && inlinePrimitive.has(F))
          // TODO deletion/replacement here rather than within each primitive
          inlinePrimitive.get(F)!(pr, code, v)
      }
    } else if (ex.head === 'global' && st.type === ir.unreachable) {
      pr.delete(v)
      abort(pr, `${asBinding(ex.body[0]).name} is not defined`)
    }
  }
  return pr.finish()
}

// Casts

function store(pr: Fragment<MIR>, T: Type, ptr: Val<MIR>, x: Val<MIR>): void {
  if (!(isEqual(pr.type(ptr), types.Ptr()) || isEqual(pr.type(ptr), types.int32()))) throw new Error('store: expected RPtr or Int32')
  const regs = layout(T)
  for (let i = 0; i < regs.length; i++) {
    const part = pr.push(stmt(expr('ref', x, i64(i + 1))))
    pr.push(stmt(xcall(new WIntrinsic(`${regs[i]}.store`), ptr, part), { type: types.nil }))
    // TODO could use constant offset here
    if (i + 1 < regs.length)
      ptr = pr.push(stmt(xcall(new WIntrinsic('i32.add'), ptr, i32(wsizeof(regs[i]))), { type: types.int32() }))
  }
}

function load(pr: Fragment<MIR>, T: Type, ptr: Val<MIR>, { count = true }: { count?: boolean } = {}): Val<MIR> {
  if (!(isEqual(pr.type(ptr), types.Ptr()) || isEqual(pr.type(ptr), types.int32()))) throw new Error('load: expected RPtr or Int32')
  const regs = layout(T)
  const parts: Val<MIR>[] = []
  for (let i = 0; i < regs.length; i++) {
    const bits = pr.push(stmt(expr('ref', ptr, i64(1)), { type: types.bits(32) }))
    const val = pr.push(stmt(xcall(new WIntrinsic(`${regs[i]}.load`), bits), { type: [regs[i]] }))
    parts.push(val)
    // TODO same as above
    if (i + 1 < regs.length)
      ptr = pr.push(stmt(xcall(new WIntrinsic('i32.add'), ptr, i32(wsizeof(regs[i]))), { type: types.int32() }))
  }
  const result = pr.push(stmt(expr('tuple', ...parts), { type: T })) as Val<MIR>
  if (count && isreftype(T)) pr.push(stmt(expr('retain', result)))
  return result
}

function box(pr: Fragment<MIR>, T: Type, x: Val<MIR>): number {
  const ptr = call(pr, types.tag('common.malloc!'), [i32(sizeof(T))], types.int32())
  store(pr, T, ptr, x)
  return ptr
}

function unbox(pr: Fragment<MIR>, T: Type, x: Val<MIR>, { count = true }: { count?: boolean } = {}): Val<MIR> {
  const ptr = pr.push(stmt(expr('ref', x, i64(1)), { type: types.int32() }))
  const result = load(pr, T, ptr, { count })
  if (count) pr.push(stmt(expr('release', x)))
  return result
}

function blockargtype(bl: Block<MIR>, i: number): Type {
  return asType(bl.type(bl.args[i - 1]))
}

function cast(pr: Fragment<MIR>, from: Anno<Type>, to: Anno<Type>, x: Val<MIR>): Val<MIR> {
  if (to === unreachable || from === unreachable || isEqual(from, to)) return x
  if (['bits', 'float32', 'float64'].includes(from.kind) && isEqual(to, types.abstract(from)))
    return some(constValue(from))
  if (from.kind === 'union')
    throw new Error('casting union not implemented')
  if (from.kind === 'pack' && to.kind === 'pack') {
    if (types.nparts(from) !== types.nparts(to)) throw new Error('pack cast mismatch')
    const parts: Val<MIR>[] = []
    for (let i = 0; i <= types.nparts(from); i++) {
      let p = indexer(pr, from, types.int64(i), x, types.int64(i))
      p = cast(pr, types.part(from, i), types.part(to, i), p)
      parts.push(p)
    }
    return pr.push(stmt(expr('tuple', ...parts), { type: to }))
  }
  if (from.kind === 'pack' && to.kind === 'vpack') {
    if (!(types.tagOf(to).kind === 'tag')) throw new Error('nope')
    const E = some(types.partial_eltype(to))
    const n = types.nparts(from)
    if (sizeof(E) === 0)
      return pr.push(stmt(xtuple(i32(n)), { type: to }))
    let ptr = call(pr, types.tag('common.malloc!'), [i32(sizeof(E) * n)], types.int32())
    let pos: Val<MIR> = ptr
    for (let i = 1; i <= n; i++) {
      let el = indexer(pr, from, types.int64(i), x, types.int64(i))
      el = cast(pr, types.part(from, i), E, el)
      store(pr, E, pos, el)
      if (i < n) pos = call(pr, types.tag('common.+'), [pos, i32(sizeof(E))], types.int32())
    }
    return pr.push(stmt(xtuple(i32(n), ptr), { type: to }))
  }
  if (to.kind === 'union') {
    const i = to.options.findIndex(opt => types.issubset(from, opt)) + 1
    if (i <= 0) throw new Error('union injection: no matching case')
    let y = cast(pr, from, to.options[i - 1], x)
    const parts: Val<MIR>[] = [i32(i)]
    for (let j = 1; j <= to.options.length; j++) {
      const regs = layout(to.options[j - 1]).length
      if (j === i && typeof y === 'number') parts.push(y)
      else for (let k = 1; k <= regs; k++)
        parts.push(Const.from(layout(to.options[j - 1])[k - 1], 0))
    }
    return pr.push(stmt(expr('tuple', ...parts), { type: to }))
  }
  if (from.kind === 'pack' && to.kind === 'recursive') {
    const U = types.unroll(to)
    let y = cast(pr, from, U, x)
    const boxed = box(pr, U, y)
    return pr.push(stmt(xtuple(boxed), { type: to }))
  }
  throw new Error(`unsupported cast: ${types.repr(from)} -> ${types.repr(to)}`)
}

function casts(inf: Inferred, code: MIR, ret: Anno<Type>): MIR {
  const pr = new Pipe(code)
  for (const [v, st] of pr) {
    const ex = st.expr
    // Cast arguments to wasm primitives
    // TODO insert widen calls instead
    if (ex.head === 'call' && (ex.body[0] instanceof WIntrinsic || ex.body[0] instanceof WImport)) {
      const args = ex.body.slice(1).map(a => constValue(asType(pr.type(a))) ?? a)
      pr.set(v, xcall(ex.body[0], ...args))
    } else if (ex.head === 'call') {
      const S = ex.body.map(a => pr.type(a))
      const partial = S[0] instanceof Method && S[0].func !== undefined
      if (!partial && S.every(t => t !== ir.unreachable)) {
        const sig: Sig = [asFunc(S[0]), ...S.slice(1).map(x => asType(x))]
        if (!(inf.get(sig) instanceof Redirect)) continue
        const [_, ...T] = resolveSig(inf.inf, sig)
        pr.delete(v)
        const args = ex.body.slice(1).map((a, i) => cast(pr, asType(S[i + 1]), T[i], a))
        const v2 = pr.push(stmt(xcall(ex.body[0], ...args), { type: st.type }))
        pr.replace(v, v2)
      }
    } else if (st.expr instanceof Branch) {
      const br = st.expr
      if (br.isreturn()) {
        const val = br.args[0]
        const S = asType(pr.type(val))
        if (!isEqual(S, ret)) {
          const casted = cast(pr, S, ret, val)
          pr.set(v, Branch.return(casted))
        } else if (typeof val !== 'number') {
          const tmp = pr.push(stmt(expr('tuple'), { type: S })) as Val<MIR>
          pr.set(v, Branch.return(tmp))
        }
      } else {
        const args: Val<MIR>[] = []
        for (let i = 1; i <= br.args.length; i++) {
          let a = br.args[i - 1]
          const S = asType(pr.type(a))
          const T = blockargtype(code.block(br.target), i)
          if (typeof a !== 'number') a = pr.push(stmt(expr('tuple'), { type: S }))
          if (!isEqual(S, T)) a = cast(pr, S, T, a)
          args.push(a)
        }
        pr.set(v, new Branch(br.target, args, br.when))
      }
    }
  }
  return pr.finish()
}

function expand(inf: Inferred, code: MIR, ret: Anno<Type>): MIR {
  code = trim_unreachable(code)
  code = ir.fuseblocks(code)
  code = lowerdata(code)
  code = casts(inf, code, ret)
  return code
}

function Expanded(inf: Inferred): Cache<Sig, Redirect | MIR> {
  return new Cache<Sig, Redirect | MIR>((sig: Sig) => {
    const [F, ...Ts] = sig
    if (F instanceof Method && outlinePrimitive.has(F))
      return outlinePrimitive.get(F)!(...Ts)
    const res = inf.get(sig)
    if (res instanceof Redirect) return res
    return expand(inf, ...res)
  })
}
