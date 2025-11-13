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
import { Type } from '../frontend/types'
import { ValueType, sizeof as wsizeof } from '../wasm/wasm'
import * as wasm from '../wasm/wasm'
import { MIR, Method, Value, xstring, Global, Invoke, Wasm } from '../frontend/modules'
import { options } from '../utils/options'
import { Def } from '../dwarf'
import { Inferred, Redirect, Sig, sig as resolveSig } from './abstract'
import { wasmPartials } from '../backend/wasm'
import isEqual from 'lodash/isEqual'
import { Pipe, Block, Fragment, expr, Branch, Val, Anno, unreachable, asType } from '../utils/ir'
import { some } from '../utils/map'
import { xcall, xtuple } from '../frontend/lower'
import { xwasm } from '../frontend/modules'
import { isreftype } from './refcount'
import { partial_part, getIntValue, nparts, constValue, partial_set, copy_method } from './primitives'
import { inlinePrimitive, InvokeSt, outlinePrimitive } from './prim_map'
import { Cache } from '../utils/cache'

export { abort, call, layout, wlayout, sizeof, store, load, box, unbox, union_downcast, union_cases, cast, copyir, partir, packir, set_pack, indexer, setir, Expanded }

const i32 = Value.i32
const i64 = Value.i64

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
          const cond = asType(pr.type(br.when))
          if (!types.issubset(cond, types.bool())) throw new Error('non-bool condition')
          if (isEqual(cond, types.bool(false))) {
            pr.delete(v)
          } else if (isEqual(cond, types.bool(true))) {
            flag = true
            pr.set(v, new Branch(br.target, br.args))
          }
        }
      } else if (st.type === ir.unreachable) {
        pr.push(pr.stmt(Branch.unreachable()))
        flag = true
      }
    }
  }
  return pr.finish()
}

function union_downcast(pr: Fragment<MIR>, U: Type & { kind: 'union' }, i: number, x: Val<MIR>): Val<MIR> {
  const offset = 1 + U.options.slice(0, i - 1).reduce((n, t) => n + layout(t).length, 0)
  const regs = layout(U.options[i - 1]).length
  const parts: Val<MIR>[] = []
  for (let j = 1; j <= regs; j++) parts.push(pr.push(pr.stmt(expr('ref', x, i64(offset + j)))))
  return pr.push(pr.stmt(xtuple(...parts), { type: U.options[i - 1] }))
}

// `f` is reponsible for freeing its argument value, but not for freeing `x`
// (since they are the same object)
function union_cases(code: MIR, T: Type & { kind: 'union' }, x: Val<MIR>, f: (S: Type, val: Val<MIR>) => Val<MIR>): MIR {
  const j = code.push(code.stmt(expr('ref', x, i64(1)), { type: types.bits(32) }))
  for (let caseIdx = 1; caseIdx <= T.options.length; caseIdx++) {
    const cond = code.push(code.stmt(xwasm('i32.eq', j, i32(caseIdx)), { type: types.bool() }))
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

function abort(code: Fragment<MIR>, s: string): Val<MIR> {
  const ref = options().gc ? types.Ref : types.bits(32)
  const id = code.push(code.stmt(xstring(s), { type: ref }))
  return code.push(code.stmt(xwasm(['support', 'abort'], id)))
}

// We're a bit fast and loose with types here, because `T` and `[T]` have the
// same representation (for now).
function call(code: Fragment<MIR>, f: Val<MIR> | Method, args: Val<MIR>[], type: Anno<Type>): Val<MIR> {
  const Ts = args.map(a => asType(code.type(a)))
  const arglist = code.push(code.stmt(xtuple(...args), { type: types.list(...Ts) }))
  return code.push(code.stmt(xcall(f, arglist), { type }))
}

// Pack primitives

function layout(T: Type): Type[] {
  if (types.isValue(T)) return []
  switch (T.kind) {
    case 'float32': return [types.float32()]
    case 'float64': return [types.float64()]
    case 'ref': return [types.Ref]
    case 'bits': {
      if (T.size <= 32) return [types.bits(32)]
      if (T.size <= 64) return [types.bits(64)]
      throw new Error(`Unsupported bit size ${T.size}`)
    }
    case 'pack': return T.parts.flatMap(layout)
    case 'vpack': return [ // size, pointer
      ...layout(types.tagOf(T)), types.bits(32),
      ...(layout(some(types.partial_eltype(T))).length === 0 ? [] : [types.bits(32)])
    ]
    case 'recursive': return [types.bits(32)]
    case 'union': return [types.bits(32), ...T.options.flatMap(layout)]
    case 'tag': return []
    case 'any':
    case 'recurrence': throw new Error('unimplemented')
    default: { const _: never = T; throw new Error('unreachable') }
  }
}

function wtype(T: Type): ValueType {
  if (T.kind === 'float32') return wasm.f32
  if (T.kind === 'float64') return wasm.f64
  if (T.kind === 'ref') return wasm.externref
  if (T.kind === 'bits') {
    if (T.size <= 32) return wasm.i32
    if (T.size <= 64) return wasm.i64
    throw new Error(`Unsupported bit size ${T.size}`)
  }
  throw new Error(`wtype: unexpected type ${T.kind}`)
}

function wlayout(T: Type): ValueType[] {
  return layout(T).map(t => wtype(t))
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
  return wlayout(T).reduce((n, t) => n + wsizeof(t), 0)
}

// part
// ====

function partir_union(x: Type & { kind: 'union' }, i: Type): MIR {
  const code = MIR(Def('common.core.part'))
  const retT = partial_part(x, i)
  const vx = code.argument(x)
  const vi = code.argument(i)
  union_cases(code, x, vx, (T, val) => {
    // TODO possibly insert `part_method` calls and redo lowering
    let ret = indexer(code, T, i, val, vi)
    if (partial_part(T, i) === unreachable) return ret // TODO insert unreachable?
    if (isreftype(partial_part(T, i))) code.push(code.stmt(expr('retain', ret)))
    if (isreftype(T)) code.push(code.stmt(expr('release', val)))
    ret = cast(code, partial_part(T, i), retT, ret)
    return ret
  })
  return code
}

// Create a `part` method to dynamically index tuples allocated as registers.
// TODO: should make sure this comes out as a switch / branch table.
function partir(x: Type, i: Type): MIR {
  if (x.kind === 'union') return partir_union(x, i)
  if (!types.tag('common.Int').isEqual(types.tagOf(i))) throw new Error('partir: expected Int index')
  const T = partial_part(x, i)
  const code = MIR(Def('common.core.part'))
  const vx = code.argument(x)
  const vi = code.argument(i)
  const xlayout = layout(x)
  const part = (k: number): Val<MIR> =>
    code.push(code.stmt(expr('ref', vx, i64(k)), { type: xlayout[k - 1] }))
  for (let idx = 1; idx <= types.nparts(x); idx++) {
    const cond = call(code, types.tag('common.=='), [i64(idx), vi], types.bool())
    const before = code.block()
    const body = code.newBlock()
    // TODO: recurse to `indexer` here, let casting happen later
    const range = sublayout(x, idx)
    const caseT = partial_part(x, types.int64(idx))
    if (caseT === unreachable) throw new Error('partir: unreachable case')
    let y = code.push(code.stmt(xtuple(...range.map(part)), { type: caseT }))
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
  if (idx === 0 || layout(T).length === 0) return T
  if (idx !== undefined)
    i = i32((idx - 1) * sizeof(T))
  else {
    i = call(pr, types.tag('common.Int32'), [i], types.int32())
    i = call(pr, types.tag('common.-'), [i, i32(1)], types.int32())
    i = call(pr, types.tag('common.*'), [i, i32(sizeof(T))], types.int32())
  }
  // TODO bounds check
  let p = pr.push(pr.stmt(expr('ref', x, i64(2)), { type: types.int32() }))
  p = call(pr, types.tag('common.+'), [p, i], types.int32())
  return load(pr, T, p, { count: false })
}

function indexer(pr: Fragment<MIR>, T: Type, I: Type, x: Val<MIR>, i: Val<MIR>): Val<MIR> {
  const idx = getIntValue(I)
  if (types.isAtom(T) && idx !== undefined) {
    if (idx > 1)
      return abort(pr, `Invalid index ${idx} for ${types.repr(T)}`)
    if (types.isValue(types.part(T, idx)))
      return types.part(T, idx)
    if (isEqual(T, types.float64()))
      return pr.push(pr.stmt(xwasm('i64.reinterpret_f64', x), { type: types.bits(64) }))
    if (isEqual(T, types.float32()))
      return pr.push(pr.stmt(xwasm('i32.reinterpret_f32', x), { type: types.bits(32) }))
    return x
  } else if (T.kind === 'vpack') {
    return vpack_indexer(pr, T, I, x, i)
  } else if (T.kind === 'pack' && idx !== undefined) {
    if (!(0 <= idx && idx <= types.nparts(T)))
      return abort(pr, `Invalid index ${idx} for ${types.repr(T)}`)
    const P = types.part(T, idx)
    const range = sublayout(T, idx)
    const _part = (k: number): Val<MIR> => pr.push(pr.stmt(expr('ref', x, i64(k)), { type: layout(T)[k - 1] }))
    return pr.push(pr.stmt(xtuple(...range.map(_part)), { type: P }))
  } else {
    throw new Error('unimplemented')
  }
}

// packcat
// =======

// TODO memcpy when possible
function copyir(S: Type, D: Type): MIR {
  let code = MIR(Def('common.core.copy'))
  let src = code.argument(types.int32())
  let dst = code.argument(types.int32())
  let len = code.argument(types.int32())
  call(code, types.tag('common.debugger'), [], types.nil)

  let before = code.block()
  let header = code.newBlock()
  let body = code.newBlock()
  let after = code.newBlock()

  before.branch(header, [src, dst, len])
  src = header.argument(types.int32())
  dst = header.argument(types.int32())
  len = header.argument(types.int32())
  const done = call(header, types.tag('common.=='), [len, i32(0)], types.int32())
  header.branch(after, [], { when: done })
  header.branch(body)

  let x = load(body, S, src)
  x = cast(body, S, D, x)
  store(body, D, dst, x)
  const src2 = call(body, types.tag('common.+'), [src, i32(sizeof(S))], types.int32())
  const dst2 = call(body, types.tag('common.+'), [dst, i32(sizeof(D))], types.int32())
  const len2 = call(body, types.tag('common.-'), [len, i32(1)], types.int32())
  body.branch(header, [src2, dst2, len2])
  code.return(dst)
  return code
}

function copy(code: MIR, S: Type, D: Type, src: Val<MIR>, dst: Val<MIR>, len: Val<MIR>) {
  return call(code, copy_method.param(S, D), [src, dst, len], types.int32())
}

function packir(Ts: Type): MIR {
  if (!(Ts.kind === 'pack')) throw new Error('nope')
  const T = types.packcat(...types.parts(Ts))
  const U = types.unroll(T)
  if (!(U.kind === 'vpack') || !(types.tagOf(T).kind === 'tag')) throw new Error('nope')
  const E = some(types.partial_eltype(U))
  const code = MIR(Def('common.core.packcat'))
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
    code.return(code.push(code.stmt(xtuple(size), { type: T })))
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
      let len = code.push(code.stmt(expr('ref', ps[i - 1], i64(1)), { type: types.int32() }))
      let src = code.push(code.stmt(expr('ref', ps[i - 1], i64(2)), { type: types.int32() }))
      pos = copy(code, types.partial_eltype(P), E, src, pos, len)
      code.push(code.stmt(expr('release', ps[i - 1])))
    } else throw new Error('unsupported')
  }
  let result = code.push(code.stmt(xtuple(size, ptr), { type: U }))
  if (T.kind === 'recursive') {
    result = box(code, U, result)
    result = code.push(code.stmt(xtuple(result), { type: T })) // TODO remove
  }
  code.return(result)
  return code
}

// set
// ===

function set_pack(code: Fragment<MIR>, xs: Val<MIR>, i: Val<MIR>, x: Val<MIR>): Val<MIR> {
  let T = asType(partial_set(asType(code.type(xs)), asType(code.type(i)), asType(code.type(x))))
  let idx = some(getIntValue(asType(code.type(i))))
  const parts: Val<MIR>[] = []
  for (let i = 0; i <= types.nparts(T); i++)
    parts.push(i === idx ? x : indexer(code, T, types.int64(i), xs, types.int64(i)))
  return code.push(code.stmt(xtuple(...parts), { type: T }))
}

function set_vpack(T: Type & { kind: 'vpack' }, I: Type, X: Type): MIR {
  let code = MIR(Def('common.core.set'))
  let xs = code.argument(T) as Val<MIR>
  let i = code.argument(I) as Val<MIR>
  let x = code.argument(X) as Val<MIR>
  let E = some(types.partial_eltype(asType(partial_set(T, I, X))))
  if (layout(E).length == 0) {
    code.return(xs)
    return code
  }
  let size = code.push(code.stmt(expr('ref', xs, i64(1)), { type: types.int32() }))
  let src = code.push(code.stmt(expr('ref', xs, i64(2)), { type: types.int32() }))
  let bytes = call(code, types.tag('common.*'), [size, i32(sizeof(E))], types.int32())
  let ptr = call(code, types.tag('common.malloc!'), [bytes], types.int32())
  let result = code.push(code.stmt(xtuple(size, ptr), { type: T }))

  i = getIntValue(I) ? i32(getIntValue(I)!) : call(code, types.tag('common.Int32'), [i], types.int32())
  let len = call(code, types.tag('common.-'), [i, i32(1)], types.int32())
  let pos = copy(code, types.partial_eltype(T), E, src, ptr, len)
  x = cast(code, X, E, x)
  store(code, E, pos, x)
  pos = call(code, types.tag('common.+'), [pos, i32(sizeof(E))], types.int32())
  src = call(code, types.tag('common.+'), [src, call(code, types.tag('common.*'), [i, i32(sizeof(E))], types.int32())], types.int32())
  len = call(code, types.tag('common.-'), [size, i], types.int32())
  pos = copy(code, types.partial_eltype(T), E, src, pos, len)

  code.push(code.stmt(expr('release', xs)))
  code.return(result)
  return code
}

function setir(xs: Type, I: Type, X: Type): MIR {
  if (xs.kind === 'vpack') return set_vpack(xs, I, X)
  throw new Error('set: unsupported type')
}

// Expansion pass

function lowerdata(code: MIR): MIR {
  const pr = new Pipe(code)
  for (const [v, st] of pr) {
    const ex = st.expr
    if (ex.head === 'pack') {
      const args = ex.body.filter(x => !types.isValue(asType(code.type(x))))
      args.length ? pr.set(v, xtuple(...args)) : pr.replace(v, asType(st.type))
    } else if (ex instanceof Wasm && !ex.isImport()) {
      const instr = ex.callee as wasm.Instruction
      const op = instr.kind === 'op' ? instr.name : ''
      if (wasmPartials.has(op) && types.isValue(asType(st.type)))
        pr.replace(v, asType(st.type))
    } else if (ex instanceof Invoke) {
      if (inlinePrimitive.has(ex.method.id)) {
        pr.delete(v)
        pr.replace(v, inlinePrimitive.get(ex.method.id)!(pr, st as InvokeSt))
      }
    } else if (ex instanceof Global && st.type === ir.unreachable) {
      pr.delete(v)
      abort(pr, `${ex.binding.name} is not defined`)
    }
  }
  return pr.finish()
}

// Casts

function store(pr: Fragment<MIR>, T: Type, ptr: Val<MIR>, x: Val<MIR>): void {
  if (!(isEqual(pr.type(ptr), types.Ptr()) || isEqual(pr.type(ptr), types.int32()))) throw new Error('store: expected RPtr or Int32')
  const regs = wlayout(T)
  for (let i = 0; i < regs.length; i++) {
    const part = pr.push(pr.stmt(expr('ref', x, i64(i + 1))))
    pr.push(pr.stmt(xwasm(`${regs[i]}.store`, ptr, part), { type: types.nil }))
    // TODO could use constant offset here
    if (i + 1 < regs.length)
      ptr = pr.push(pr.stmt(xwasm('i32.add', ptr, i32(wsizeof(regs[i]))), { type: types.int32() }))
  }
}

function load(pr: Fragment<MIR>, T: Type, ptr: Val<MIR>, { count = true }: { count?: boolean } = {}): Val<MIR> {
  if (!(isEqual(pr.type(ptr), types.Ptr()) || isEqual(pr.type(ptr), types.int32()))) throw new Error('load: expected RPtr or Int32')
  const regs = layout(T)
  const parts: Val<MIR>[] = []
  for (let i = 0; i < regs.length; i++) {
    const bits = pr.push(pr.stmt(expr('ref', ptr, i64(1)), { type: types.bits(32) }))
    const val = pr.push(pr.stmt(xwasm(`${wtype(regs[i])}.load`, bits), { type: regs[i] }))
    parts.push(val)
    // TODO same as above
    if (i + 1 < regs.length)
      ptr = pr.push(pr.stmt(xwasm('i32.add', ptr, i32(sizeof(regs[i]))), { type: types.int32() }))
  }
  const result = pr.push(pr.stmt(xtuple(...parts), { type: T }))
  if (count && isreftype(T)) pr.push(pr.stmt(expr('retain', result)))
  return result
}

function box(pr: Fragment<MIR>, T: Type, x: Val<MIR>): Val<MIR> {
  const ptr = call(pr, types.tag('common.malloc!'), [i32(sizeof(T))], types.int32())
  store(pr, T, ptr, x)
  return ptr
}

function unbox(pr: Fragment<MIR>, T: Type, x: Val<MIR>, { count = true }: { count?: boolean } = {}): Val<MIR> {
  const ptr = pr.push(pr.stmt(expr('ref', x, i64(1)), { type: types.int32() }))
  const result = load(pr, T, ptr, { count })
  if (count) pr.push(pr.stmt(expr('release', x)))
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
    return pr.push(pr.stmt(xtuple(...parts), { type: to }))
  }
  if (from.kind === 'pack' && to.kind === 'vpack') {
    if (!(types.tagOf(to).kind === 'tag')) throw new Error('nope')
    const E = some(types.partial_eltype(to))
    const n = types.nparts(from)
    if (sizeof(E) === 0)
      return pr.push(pr.stmt(xtuple(i32(n)), { type: to }))
    let ptr = call(pr, types.tag('common.malloc!'), [i32(sizeof(E) * n)], types.int32())
    let pos: Val<MIR> = ptr
    for (let i = 1; i <= n; i++) {
      let el = indexer(pr, from, types.int64(i), x, types.int64(i))
      el = cast(pr, types.part(from, i), E, el)
      store(pr, E, pos, el)
      if (i < n) pos = call(pr, types.tag('common.+'), [pos, i32(sizeof(E))], types.int32())
    }
    return pr.push(pr.stmt(xtuple(i32(n), ptr), { type: to }))
  }
  if (to.kind === 'union') {
    const i = to.options.findIndex(opt => types.issubset(from, opt)) + 1
    if (i <= 0) throw new Error('union injection: no matching case')
    let y = cast(pr, from, to.options[i - 1], x)
    const parts: Val<MIR>[] = [i32(i)]
    for (let j = 1; j <= to.options.length; j++) {
      const regs = wlayout(to.options[j - 1]).length
      if (j === i && typeof y === 'number') parts.push(y)
      else for (let k = 1; k <= regs; k++)
        parts.push(Value.from(wasm.asNumType(wlayout(to.options[j - 1])[k - 1]), 0))
    }
    return pr.push(pr.stmt(xtuple(...parts), { type: to }))
  }
  if (from.kind === 'pack' && to.kind === 'recursive') {
    const U = types.unroll(to)
    let y = cast(pr, from, U, x)
    const boxed = box(pr, U, y)
    return pr.push(pr.stmt(xtuple(boxed), { type: to }))
  }
  throw new Error(`unsupported cast: ${types.repr(from)} -> ${types.repr(to)}`)
}

function casts(inf: Inferred, code: MIR, ret: Anno<Type>): MIR {
  const pr = new Pipe(code)
  for (const [v, st] of pr) {
    const ex = st.expr
    // Cast arguments to wasm primitives
    // TODO insert widen calls instead
    if (ex instanceof Wasm) {
      const args = ex.body.map(a => constValue(asType(pr.type(a))) ?? a)
      pr.set(v, xwasm(ex.callee, ...args))
    } else if (ex instanceof Invoke) {
      const S = ex.body.map(a => pr.type(a))
      if (!ex.method.func && S.every(t => t !== ir.unreachable)) {
        const sig: Sig = [ex.method, ...S.map(x => asType(x))]
        if (!(inf.get(sig) instanceof Redirect)) continue
        const [_, ...T] = resolveSig(inf.inf, sig)
        pr.delete(v)
        const args = ex.body.map((a, i) => cast(pr, asType(S[i]), T[i], a))
        const v2 = pr.push(pr.stmt(xcall(ex.method, ...args), { type: st.type }))
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
          pr.set(v, Branch.return(S))
        }
      } else {
        const args: Val<MIR>[] = []
        for (let i = 1; i <= br.args.length; i++) {
          let a = br.args[i - 1]
          const S = asType(pr.type(a))
          const T = blockargtype(code.block(br.target), i)
          if (typeof a !== 'number') a = S
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
    if (F instanceof Method && outlinePrimitive.has(F.id))
      return outlinePrimitive.get(F.id)!(...F.params, ...Ts)
    const res = inf.get(sig)
    if (res instanceof Redirect) return res
    return expand(inf, ...res)
  })
}
