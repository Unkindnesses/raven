// Reference Counting
//
// The unoptimised approach is:
// * Insert a `retain` whenever we pass a live variable as an argument.
// * Insert a `release` whenever a variable is dropped.
//
// Passing a variable as an argument means code like:
//
// ```
// x = ...
// y = f(x)
// ```
//
// Here `x` only needs to be retained if it is used again later on. If not,
// `f(x)` will free it. (Branch arguments are treated the same way.)
//
// Return values aren't dropped, so we don't need to `release` them before
// returning. The caller will take care of them. If `f(x) = x`, the code above
// doesn't actually need any counting instructions to be emitted. (If `f(x)`
// drops `x`, eg `f(x) = nothing`, we do need to release it before returning.)
//
// A variable can be dropped immediately (if it is never used) or after a branch
// (because the variable is not used in the current path). An edge case is when a
// block has multiple predecessors, one of which has multiple successors, such
// that the block only sometimes releases a preceding variable. For now this case
// is an error.

import * as ir from '../utils/ir'
import * as types from '../frontend/types'
import { Type, tag, tagOf, asType } from '../frontend/types'
import { MIR, Const, IRValue, Method, asConst, Invoke } from '../frontend/modules'
import { Def } from '../dwarf'
import { Redirect, type Sig } from './abstract'
import { Cache } from '../utils/cache'
import { primitive } from './primitives'
import isEqual from 'lodash/isEqual'
import { HashMap, asNumber, some, setdiff, filter as filter } from '../utils/map'
import { layout, call, indexer, load, sizeof, union_cases, unbox } from './expand'
import { unreachable } from '../utils/ir'
import { xcall, xtuple } from '../frontend/lower'
import { Accessor } from '../utils/fixpoint'

export { isrefobj, isreftype, CountMode, retain_method, release_method, refcounts }

function isrefobj(x: Type): boolean {
  return x.kind === 'pack' && tag('common.Ref').isEqual(tagOf(x))
}

function isreftype(x: Type): boolean {
  if (x.kind === 'pack') return isrefobj(x) || types.parts(x).some(isreftype)
  if (x.kind === 'union') return x.options.some(isreftype)
  if (x.kind === 'vpack') return layout(some(types.partial_eltype(x))).length !== 0
  if (x.kind === 'recursive') return true
  return false
}

function isglobal(code: MIR, v: number): boolean {
  return code.has(v) && code.get(v).expr.head === 'global'
}

// Define refcount methods for different types
// ===========================================

type CountMode = 'retain' | 'release'

// Used as a key for generated methods
const retain_method = primitive('common.core.retain', 'args', (_: Type) => unreachable)
const release_method = primitive('common.core.release', 'args', (_: Type) => unreachable)

function count(code: ir.Fragment<MIR>, T: Type, x: ir.Val<MIR>, mode: CountMode): void {
  if (T.kind === 'pack' && !isrefobj(T)) return count_inline(code, T, x, mode)
  code.push(code.stmt(xcall(mode === 'retain' ? retain_method : release_method, x), { type: types.nil }))
}

function retain(code: ir.Fragment<MIR>, T: Type, x: ir.Val<MIR>): void {
  count(code, T, x, 'retain')
}

function release(code: ir.Fragment<MIR>, T: Type, x: ir.Val<MIR>): void {
  count(code, T, x, 'release')
}

function countptr(code: ir.Fragment<MIR>, ptr: ir.Val<MIR>, mode: CountMode): void {
  const f = mode === 'retain' ? tag('common.retain!') : tag('common.release!')
  const t = asType(code.type(ptr))
  if (!tag('common.Ptr').isEqual(tagOf(t))) throw new Error('countptr: expected Ptr')
  call(code, f, [ptr], types.nil)
}

function count_inline(code: ir.Fragment<MIR>, T: Type, x: ir.Val<MIR>, mode: CountMode): void {
  if (T.kind === 'pack') return pack_count_inline(code, T, x, mode)
  if (T.kind === 'vpack') return vpack_count_inline(code, T, x, mode)
  if (T.kind === 'union') return union_count_inline(code, T, x, mode)
  if (T.kind === 'recursive') return recursive_count_inline(code, T, x, mode)
  throw new Error('unimplemented')
}

function pack_count_inline(code: ir.Fragment<MIR>, T: Type, x: ir.Val<MIR>, mode: CountMode): void {
  if (isrefobj(T)) {
    const ptr = indexer(code, T, types.int64(1), x, types.int64(1))
    if (mode === 'release') {
      const cleanup = call(code, types.tag('common.i32load'), [ptr], types.int32())
      call(code, types.tag('common.release!'), [ptr, cleanup], types.nil)
    } else
      call(code, types.tag('common.retain!'), [ptr], types.nil)
  } else {
    for (let i = 0; i <= types.nparts(T); i++) {
      const P = types.part(T, i)
      if (!isreftype(P)) continue
      const p = indexer(code, T, types.int64(i), x, types.int64(i))
      count(code, P, p, mode)
    }
  }
}

function vpack_count_inline(code: ir.Fragment<MIR>, T: Type, x: ir.Val<MIR>, mode: CountMode): void {
  if (!(code instanceof ir.IR)) throw new Error('nope')
  if (!isreftype(T)) return
  let len = code.push(code.stmt(ir.expr('ref', x, Const.i64(1)), { type: types.int32() }))
  const ptr = code.push(code.stmt(ir.expr('ref', x, Const.i64(2)), { type: types.Ptr() }))
  let pos: ir.Val<MIR> = ptr
  const elT = some(types.partial_eltype(T))
  if (mode === 'release' && isreftype(elT)) {
    const test = code.block()
    const header = code.newBlock()
    const body = code.newBlock()
    const after = code.newBlock()

    const unique = call(test, tag('common.blockUnique'), [ptr], types.int32())
    test.branch(header, [len, ptr], { when: unique })
    test.branch(after)

    len = header.argument(types.int32())
    pos = header.argument(types.Ptr())
    const done = call(header, tag('common.=='), [len, Const.i32(0)], types.int32())
    header.branch(after, [], { when: done })
    header.branch(body)

    const el = load(body, elT, pos, { count: false })
    release(body, elT, el)
    const len2 = call(body, tag('common.-'), [len, Const.i32(1)], types.int32())
    const pos2 = call(body, tag('common.+'), [pos, Const.i32(sizeof(elT))], types.Ptr())
    body.branch(header, [len2, pos2])
  }
  countptr(code, ptr, mode)
}

function union_count_inline(code: ir.Fragment<MIR>, T: Type & { kind: 'union' }, x: ir.Val<MIR>, mode: CountMode): void {
  if (!(code instanceof ir.IR)) throw new Error('nope')
  union_cases(code, T, x, (S, val) => {
    if (isreftype(S)) count(code, S, val, mode)
    return types.nil
  })
}

function recursive_count_inline(code: ir.Fragment<MIR>, T: Type, x: ir.Val<MIR>, mode: CountMode): void {
  if (!(code instanceof ir.IR)) throw new Error('nope')
  const ptr = code.push(code.stmt(ir.expr('ref', x, Const.i64(1)), { type: types.Ptr() }))
  if (mode === 'release') {
    const before = code.block()
    const body = code.newBlock()
    const after = code.newBlock()
    const unique = call(before, tag('common.blockUnique'), [ptr], types.int32())
    before.branch(body, [], { when: unique })
    before.branch(after)
    const U = types.unroll(T)
    const inner = unbox(body, U, x, { count: false })
    release(body, U, inner)
    body.branch(after)
  }
  countptr(code, ptr, mode)
}

function count_ir(T: Type, mode: CountMode): MIR {
  const code = MIR(Def(`common.core.${mode}`))
  const x = code.argument(T)
  count_inline(code, T, x, mode)
  code.return(types.nil)
  return code
}

// Insert counting instructions
// ============================

function refcountsIR(code: MIR): MIR {
  const lv = ir.liveness(code)
  const pr = new ir.Pipe(code)
  for (const bl of pr.blocks()) {
    const rel = (x: number) => { const T = asType(code.type(x)); if (isreftype(T)) release(pr, T, x) }
    // unused block arguments
    for (const x of code.block(bl.id).args)
      if (!some(lv.blocks.get(bl.id)).has(x)) rel(x)
    // conditionally dropped variables
    const dropped = ir.predecessors(code.block(bl.id)).map(c =>
      filter(setdiff(ir.liveness_after(c, lv.blocks), lv.blocks.get(bl.id)!), x => isreftype(asType(code.type(x)))))
    if (dropped.length) {
      if (!dropped.every(xs => isEqual(xs, dropped[0]))) throw new Error('nope') // condition mentioned above
      for (const x of dropped[0]) rel(x)
    }
    for (const [v, st] of bl) {
      if (st.expr.head === 'release') {
        pr.delete(v)
        const x = asNumber(st.expr.body[0])
        const T = asType(code.type(x))
        if (isreftype(T) && !lv.stmts.get(v)!.has(x)) release(pr, T, x)
      } else if (st.expr.head === 'retain') {
        pr.delete(v)
        const x = asNumber(st.expr.body[0])
        const T = asType(code.type(x))
        if (isreftype(T) && lv.stmts.get(v)!.has(x)) retain(pr, T, x)
      } else if (['call', 'tuple', 'branch'].includes(st.expr.head)) {
        const live = st.expr.head === 'branch' ? ir.liveness_after(code.blockOf(v), lv.blocks) : lv.stmts.get(v)!
        pr.delete(v)
        // reused argument
        for (const x of new Set(st.expr.body.filter(a => typeof a === 'number'))) {
          if (code.type(x) === unreachable || Array.isArray(code.type(x))) continue // skip WASM native types (TODO remove them)
          const T = asType(code.type(x))
          if (!isreftype(T)) continue
          const occ = st.expr.body.filter(a => a === x).length
          const ret = (isglobal(code, x) ? 1 : 0) + occ - (live.has(x) ? 0 : 1)
          for (let i = 0; i < ret; i++) retain(pr, T, x)
        }
        const v2 = pr.push(st)
        pr.replace(v, v2)
        // dropped variable
        if (st.type !== ir.unreachable && !Array.isArray(st.type)) {
          const T = asType(st.type)
          if (isreftype(T) && !lv.stmts.get(v)!.has(v)) release(pr, T, v2)
        }
      }
    }
  }
  return pr.finish()
}

type RegAlias = [number, number]
type AliasItem = RegAlias | IRValue

function aliases(code: MIR): Map<number, AliasItem[]> {
  const out = new Map<number, AliasItem[]>()
  const lyt = (x: any) => Array.isArray(x) ? x : layout(asType(x))
  const alias = (x: ir.Val<MIR>) => {
    if (typeof x !== 'number') return [x]
    if (!out.has(x)) out.set(x, Array.from({ length: lyt(code.type(x)).length }, (_, i) => [x, i + 1]))
    return out.get(x)!
  }
  for (const [v, st] of code) {
    if (st.expr.head === 'tuple')
      out.set(v, st.expr.body.flatMap(alias))
    else if (st.expr.head === 'ref' && typeof st.expr.body[0] === 'number') {
      out.set(v, [alias(st.expr.body[0])[Number(asConst(st.expr.body[1]).value) - 1]])
    }
  }
  return out
}

function ismethod(m: unknown, name: types.Tag): boolean {
  return m instanceof Method && name.path === m.name.path
}

// ...and take them away again
function elide_counts(code: MIR): MIR {
  const vs = aliases(code)
  for (const bl of code.blocks()) {
    const retains = new HashMap<AliasItem[], number[]>()
    for (const [v, st] of bl) {
      if (!(st.expr instanceof Invoke)) continue
      const callee = st.expr.method
      const arg = st.expr.body[0]
      if (ismethod(callee, tag('common.core.retain'))) {
        // TODO aliases for args
        if (typeof arg !== 'number' || !vs.has(arg)) continue
        const key = vs.get(arg)!
        const rs = retains.get(key) ?? []
        retains.set(key, rs)
        rs.push(v)
      } else if (ismethod(callee, tag('common.core.release'))) {
        if (typeof arg !== 'number' || !vs.has(arg)) continue
        const key = vs.get(arg)!
        const rs = retains.get(key) ?? []
        retains.set(key, rs)
        if (rs.length > 0) {
          code.delete(some(rs.pop()))
          code.delete(v)
        } else {
          // TODO figure out which `retain`s can be affected by a `release`
          retains.clear()
        }
      }
    }
  }
  return code
}

function refcounts(c: Accessor<Sig, Redirect | MIR>): Cache<Sig, Redirect | MIR> {
  return new Cache<Sig, Redirect | MIR>(sig => {
    const [F, ...Ts] = sig
    if (isEqual(F, retain_method) || isEqual(F, release_method))
      return count_ir(Ts[0], isEqual(F, retain_method) ? 'retain' : 'release')
    const ir = c.get(sig)
    if (ir instanceof Redirect) return ir
    return elide_counts(refcountsIR(ir))
  })
}
