import * as types from '../frontend/types'
import { Type, tagOf, tag, asType, bits } from '../frontend/types'
import { unreachable, Anno, Pipe, stmt, expr, asAnno, Val, Fragment } from '../utils/ir'
import { HashSet, only } from '../utils/map'
import isEqual from 'lodash/isEqual'
import { Method, MIR, Module, WIntrinsic, FuncInfo, Const, xstring } from '../frontend/modules'
import { xtuple, xcall } from '../frontend/lower'
import { lowerpattern } from '../frontend/patterns'
import * as parse from '../frontend/parse'
import { inlinePrimitive, outlinePrimitive } from './prim_map'
import { abort, call, layout, sizeof, unbox, union_downcast, union_cases, cast, partir, packir, indexer } from './expand'
import { isreftype } from './refcount'
import { options } from '../utils/options'
import { maybe_union } from './abstract'

export { core, symbolValues, string, inlinePrimitive, outlinePrimitive, invoke_method, pack_method, packcat_method, part_method, isnil_method, notnil_method, partial_isnil, partial_part, getIntValue, nparts, primitive, constValue }

const i64 = Const.i64

const bitopFuncs = new Map<string, (x: bigint, y: bigint) => bigint>([
  ['shl', (x, y) => x << y],
  ['shr_u', (x, y) => x >> y],
  ['shr_s', (x, y) => x >> y],
  ['and', (x, y) => x & y],
  ['or', (x, y) => x | y],
  ['xor', (x, y) => x ^ y],
  ['add', (x, y) => x + y],
  ['sub', (x, y) => x - y],
  ['mul', (x, y) => x * y],
  ['div_u', (x, y) => x / y],
  ['div_s', (x, y) => x / y],
  ['rem_u', (x, y) => x % y],
  ['rem_s', (x, y) => x % y]
])

const bitcmpFuncs = new Map<string, (x: bigint, y: bigint) => boolean>([
  ['eq', (x, y) => x === y],
  ['ne', (x, y) => x !== y],
  ['gt_u', (x, y) => x > y],
  ['lt_u', (x, y) => x < y],
  ['ge_u', (x, y) => x >= y],
  ['le_u', (x, y) => x <= y],
  ['gt_s', (x, y) => x > y],
  ['lt_s', (x, y) => x < y],
  ['ge_s', (x, y) => x >= y],
  ['le_s', (x, y) => x <= y]
])

const bitops = new Map<string, (x: types.Bits, y: types.Bits) => types.Bits>()
for (const [op, f] of bitopFuncs) {
  const convert = op.endsWith('_s') ? BigInt.asIntN : BigInt.asUintN
  bitops.set(op, (x: types.Bits, y: types.Bits) => {
    if (x.size !== y.size) throw new Error('Bit width mismatch')
    return new types.Bits(x.size, f(convert(x.size, x.value), convert(y.size, y.value)))
  })
}

const bitcmps = new Map<string, (x: types.Bits, y: types.Bits) => types.Bits>()
for (const [op, f] of bitcmpFuncs) {
  const convert = op.endsWith('_s') ? BigInt.asIntN : BigInt.asUintN
  bitcmps.set(op, (x: types.Bits, y: types.Bits) => {
    if (x.size !== y.size) throw new Error('Bit width mismatch')
    return new types.Bits(1, f(convert(x.size, x.value), convert(y.size, y.value)) ? 1n : 0n)
  })
}

// Core primitives – pack, packcat, part and nparts – are dealt with in
// IR expansion, but we define type inference here, and implement some
// simple built-in functions.
// TODO: bit ops should be handled by the backend.

function isInt(x: Type, size?: number): x is { kind: 'pack'; parts: [Type, Type & { kind: 'bits' }] } {
  return isEqual(tagOf(x), tag('common.Int')) &&
    x.kind === 'pack' && x.parts[1].kind === 'bits' && (size === undefined || x.parts[1].size === size)
}

function getIntValue(x: Type): number | undefined {
  if (isInt(x) && types.isValue(x)) return Number(x.parts[1].value)
}

function partial_eltype(x: Type): Anno<Type> {
  return types.partial_eltype(x) ?? unreachable
}

function partial_part(data: Type, i: Type): Anno<Type> {
  if (data.kind === 'union') return data.options.map(d => partial_part(d, i)).reduce((a, b) => maybe_union(a, b), unreachable)
  if (data.kind === 'recursive') return partial_part(types.unroll(data), i)
  const idx = getIntValue(i)
  // TODO: HACK: we assume index != 0 when indexing dynamically.
  // Should instead have a seperate `index` function that enforces this.
  if (idx === undefined) return partial_eltype(data)
  if (types.isAtom(data) || data.kind === 'pack')
    return (0 <= idx && idx <= types.nparts(data)) ? types.part(data, idx) : unreachable
  if (data.kind === 'vpack')
    return idx === 0 ? tagOf(data) : partial_eltype(data)
  throw new Error('unimplemented')
}

function partial_nparts(x: Type): Type {
  if (x.kind === 'union') return x.options.map(partial_nparts).reduce(types.union)
  if (x.kind === 'recursive') return partial_nparts(types.unroll(x))
  if (x.kind === 'vpack') return types.pack(tag('common.Int'), bits(64))
  return types.pack(tag('common.Int'), bits(64, types.nparts(x)))
}

function partial_widen(x: Type): Type {
  if (types.isAtom(x)) return types.abstract(x)
  if (x.kind === 'pack') {
    const tg = types.tagOf(x)
    if (isEqual(tg, tag('common.Int')) || isEqual(tg, tag('common.Bool')))
      return types.pack(tg, types.abstract(types.part(x, 1)))
  }
  throw new Error('unimplemented')
}

function symbolValues(x: Type): HashSet<Type> {
  if (x.kind === 'recursive') return symbolValues(types.unroll(x))
  if (x.kind === 'tag') return new HashSet<Type>([x])
  if (x.kind === 'union') return x.options.map(symbolValues).reduce((a, b) => new HashSet([...a, ...b]), new HashSet())
  return new HashSet()
}

// Fast, approximate equality check; basically a stand-in for pointer equality.
// TODO extend to handle VPack
function partial_shortcutEquals(a: Type, b: Type): Type {
  if (types.isValue(a) && types.isValue(b)) return Type(isEqual(a, b))
  const intersection = new Set([...symbolValues(a)].filter(x => symbolValues(b).has(x)))
  if (intersection.size > 0) return types.pack(tag('common.Bool'), bits(1))
  return Type(false)
}

function partial_bitsize(x: Type): Type {
  if (x.kind === 'bits') return Type(BigInt(x.size))
  throw new Error('Expected bits type')
}

function partial_bitcast(target: Type, source: Type): Type {
  if (!(target.kind === 'bits' && source.kind === 'bits')) throw new Error('not a bits type')
  return bits(target.size, source.value)
}

function partial_bitcast_s(target: Type, source: Type): Type {
  if (!(target.kind === 'bits' && source.kind === 'bits')) throw new Error('not a bits type')
  if (types.isValue(source)) return bits(target.size, new types.Bits(source.size, source.value!).signed())
  else return bits(target.size)
}

function partial_bitop(x: Type, y: Type): Type {
  if (!(x.kind === 'bits' && y.kind === 'bits' && x.size === y.size)) throw new Error('bitop requires same-size bits')
  return bits(x.size)
}

function partial_bitcmp(x: Type, y: Type): Type {
  if (!(x.kind === 'bits' && y.kind === 'bits' && x.size === y.size)) throw new Error('bitcmp requires same-size bits')
  return bits(1)
}

function partial_biteqz(x: Type): Type {
  if (x.kind !== 'bits') throw new Error('not a bits type')
  if (types.isValue(x)) return bits(1, x.value === 0n ? 1n : 0n)
  return bits(1)
}

// Needed by dispatchers, since a user-defined method would need runtime matching
// to deal with unions.
function partial_isnil(x: Type): Type {
  if (isEqual(x, types.nil)) return Type(true)
  if (types.issubset(types.nil, x)) return types.pack(tag('common.Bool'), bits(1))
  return Type(false)
}

function partial_notnil(x: Type): Anno<Type> {
  if (!types.issubset(types.nil, x)) return x
  if (x.kind === 'pack') return isEqual(x, types.nil) ? unreachable : x
  if (x.kind === 'union') return types.onion(...x.options.filter(opt => !isEqual(opt, types.nil)))
  if (x.kind === 'recursive') return types.recursive(partial_notnil(types.unroll(x)) as Type)
  throw new Error('unreachable')
}

function partial_tagcast(x: Type, t: Type): Anno<Type> {
  if (!(t instanceof types.Tag)) throw new Error('t must be a tag')
  x = types.unroll(x)
  if (x.kind !== 'union') return isEqual(tagOf(x), t) ? x : unreachable
  const ps = x.options.filter(opt => isEqual(tagOf(opt), t))
  return ps.length === 0 ? unreachable : only(ps)
}

function partial_tagstring(x: Type): Type {
  return types.String()
}

function rvtype(x: Type): Type {
  if (!types.isValue(x)) throw new Error('Expected value')
  if (x.kind === 'tag') throw new Error('unimplemented')
  if (types.isAtom(x)) return types.abstract(x)
  if (isEqual(tagOf(x), tag('common.List')))
    return types.pack(tagOf(x), ...types.parts(x).map(rvtype))
  if (isEqual(tagOf(x), tag('common.Pack')))
    return types.pack(...types.parts(x).map(rvtype))
  if (isEqual(tagOf(x), tag('common.Literal')))
    return types.part(x, 1)
  throw new Error(`Unrecognised type ${types.repr(x)}`)
}

function partial_function(f: Type, I: Type, O: Type): Type {
  return types.pack(tag('common.Int'), bits(32))
}

function partial_invoke(f: Type, I: Type, O: Type, ...xs: Type[]): Type {
  return rvtype(O)
}

function partial_jsalloc(): Type {
  return Type(options().jsalloc)
}

function primitive(name: string, pattern: string, func: (...args: Type[]) => Anno<Type>): Method {
  return new Method(tag('common.core'), tag(name), lowerpattern(parse.expr(pattern)), func)
}

const pack_method = primitive('common.core.pack', 'args', (args: Type) => types.pack(...types.parts(args)))
const part_method = primitive('common.core.part', '[data, i]', partial_part)
const nparts_method = primitive('common.core.nparts', '[x]', partial_nparts)
const packcat_method = primitive('common.core.packcat', 'args', (args: Type) => { const parts = types.parts(args); return parts.length === 0 ? unreachable : parts.reduce((x, y) => types.packcat(x, y)) })
const widen_method = primitive('common.core.widen', '[x]', partial_widen)
const shortcutEquals_method = primitive('common.core.shortcutEquals', '[a, b]', partial_shortcutEquals)

const bitsize_method = primitive('common.core.bitsize', '[x]', partial_bitsize)
const bitcast_method = primitive('common.core.bitcast', '[x, y]', partial_bitcast)
const bitcast_s_method = primitive('common.core.bitcast_s', '[x, y]', partial_bitcast_s)

const bitop_methods = new Map<string, Method>()
for (const [op, f] of bitops) {
  bitop_methods.set(op, primitive(`common.core.bit${op}`, '[x, y]', (x: Type, y: Type): Anno<Type> => {
    if (types.isValue(x) && types.isValue(y) && x.kind === 'bits' && y.kind === 'bits')
      return types.Type(f(new types.Bits(x.size, x.value!), new types.Bits(y.size, y.value!)))
    return partial_bitop(x, y)
  }))
}

const bitcmp_methods = new Map<string, Method>()
for (const [op, f] of bitcmps) {
  bitcmp_methods.set(op, primitive(`common.core.bit${op}`, '[x, y]', (x: Type, y: Type): Anno<Type> => {
    if (types.isValue(x) && types.isValue(y) && x.kind === 'bits' && y.kind === 'bits')
      return types.Type(f(new types.Bits(x.size, x.value!), new types.Bits(y.size, y.value!)))
    return partial_bitcmp(x, y)
  }))
}

const biteqz_method = primitive('common.core.biteqz', '[x]', partial_biteqz)

const isnil_method = primitive('common.core.nil?', '[x]', partial_isnil)
const notnil_method = primitive('common.core.notnil', '[x]', partial_notnil)
const tagcast_method = primitive('common.core.tagcast', '[x, t]', partial_tagcast)
const tagstring_method = primitive('common.core.tagstring', '[x]', partial_tagstring)

const function_method = primitive('common.core.function', '[f, I, O]', partial_function)
const invoke_method = primitive('common.core.invoke', '[f, I, O, xs...]', partial_invoke)

const jsalloc_method = primitive('common.core.jsalloc', '[]', partial_jsalloc)

function primitives(): Method[] {
  return [
    pack_method,
    part_method,
    nparts_method,
    packcat_method,
    widen_method,
    shortcutEquals_method,
    bitsize_method,
    bitcast_method,
    bitcast_s_method,
    ...bitop_methods.values(),
    ...bitcmp_methods.values(),
    biteqz_method,
    isnil_method,
    notnil_method,
    tagcast_method,
    tagstring_method,
    function_method,
    invoke_method,
    jsalloc_method,
  ]
}

// Primitive implementations
// Invoked from expansion. `inline` primitives replace the call with a
// definition. `outline` ones return an expanded IR fragment, to be called as a
// normal function.

inlinePrimitive.set(pack_method, (pr, ir, v) => {
  const T = asType(ir.get(v).type)
  if (isEqual(T, types.float64())) {
    const arg = ir.get(v).expr.body[1]
    const x = pr.insert(v, stmt(expr('ref', arg, i64(1)), { type: bits(64) }))
    pr.set(v, xcall(new WIntrinsic('f64.reinterpret_i64'), x))
  } else if (isEqual(T, types.float32())) {
    const arg = ir.get(v).expr.body[1]
    const x = pr.insert(v, stmt(expr('ref', arg, i64(1)), { type: bits(32) }))
    pr.set(v, xcall(new WIntrinsic('f32.reinterpret_i32'), x))
  } else {
    // Arguments are turned into a tuple when calling any function, so this
    // is just a cast.
    const x = ir.get(v).expr.body[1]
    if (!isEqual(layout(T), layout(asType(pr.type(x))))) throw new Error('pack: layout mismatch')
    pr.replace(v, x)
  }
})

inlinePrimitive.set(part_method, (pr, ir, v) => {
  let [x, i] = ir.get(v).expr.body.slice(1)
  let [T, I] = [x, i].map(x => asType(pr.type(x)))
  pr.delete(v)
  if (T.kind === 'recursive') {
    T = types.unroll(T)
    x = unbox(pr, T, x)
  }
  if ((T.kind === 'pack' && !types.isValue(I)) || T.kind === 'union') {
    const y = pr.push(stmt(xcall(part_method, x, i), { type: partial_part(T, I) }))
    pr.replace(v, y)
  } else {
    const y = indexer(pr, T, I, x, i) as Val<MIR>
    pr.replace(v, y)
    if (partial_part(T, I) === unreachable) return
    if (isreftype(asType(partial_part(T, I)))) pr.push(stmt(expr('retain', y)))
    if (isreftype(T)) pr.push(stmt(expr('release', x)))
  }
})

outlinePrimitive.set(part_method, partir)

inlinePrimitive.set(packcat_method, (pr, ir, v) => {
  const x = ir.get(v).expr.body[1]
  const S = asType(pr.type(x))
  const T = asType(ir.get(v).type)
  if (types.isValue(T)) {
    pr.set(v, xtuple())
  } else if ((S.kind === 'pack' && types.isAtom(T)) || T.kind === 'pack') {
    if (!isEqual(layout(S), layout(T))) throw new Error('packcat: layout mismatch')
    pr.set(v, xtuple(x))
  }
})

outlinePrimitive.set(packcat_method, packir)

function nparts(code: Fragment<MIR>, T: Type, x: Val<MIR>): Val<MIR> {
  if (T.kind === 'recursive') {
    T = types.unroll(T)
    x = unbox(code, T, x)
  }
  if (T.kind === 'vpack') {
    const sz = code.push(stmt(expr('ref', x, i64(1)), { type: types.int32() }))
    code.push(stmt(expr('release', x)))
    return call(code, types.tag('common.Int64'), [sz], types.int64())
  } else {
    code.push(stmt(expr('release', x)))
    return code.push(stmt(xtuple(), { type: types.int64(types.nparts(T)) }))
  }
}

inlinePrimitive.set(nparts_method, (pr, ir, v) => {
  let x = ir.get(v).expr.body[1]
  let T = asType(pr.type(x))
  pr.delete(v)
  if (T.kind === 'recursive') {
    T = types.unroll(T)
    x = unbox(pr, T, x)
  }
  if (T.kind === 'union') {
    const y = pr.push(stmt(xcall(nparts_method, x), { type: types.int64() }))
    pr.replace(v, y)
  } else {
    pr.replace(v, nparts(pr, T, x))
    if (isreftype(T)) pr.push(stmt(expr('release', x)))
  }
})

outlinePrimitive.set(nparts_method, (x: Type): MIR => {
  if (x.kind !== 'union') throw new Error('expected union type')
  const code = MIR(new FuncInfo(tag('common.core.nparts')))
  const retT = partial_nparts(x)
  const vx = code.argument(x)
  union_cases(code, x, vx, (T, val) => {
    // TODO possibly insert `nparts_method` calls and redo lowering
    let ret = nparts(code, T, val)
    ret = cast(code, partial_nparts(T), retT, ret)
    if (isreftype(T)) code.push(stmt(expr('release', val)))
    return ret
  })
  return code
})

function constValue(T: Type): Const | undefined {
  if (T.kind === 'bits' && T.value !== undefined)
    return Const.from(only(layout(types.abstract(T))), BigInt.asIntN(T.size, T.value))
  if (T.kind === 'float32' && T.value !== undefined)
    return Const.f32(T.value)
  if (T.kind === 'float64' && T.value !== undefined)
    return Const.f64(T.value)
}

inlinePrimitive.set(widen_method, (pr, ir, v) => {
  const x = ir.get(v).expr.body[1]
  const T = asType(pr.type(x))
  if (types.isAtom(T) && types.isValue(T))
    pr.replace(v, constValue(T) ?? T)
  else if (isEqual(tagOf(T), tag('common.Int')) || isEqual(tagOf(T), tag('common.Bool')))
    pr.replace(v, constValue(types.part(T, 1)) ?? types.part(T, 1))
  else
    pr.replace(v, x)
})

inlinePrimitive.set(bitsize_method, (pr, ir, v) => { pr.set(v, xtuple()) })

type BitsType = Type & { kind: 'bits' }

// TODO use Const rather than BitsType in the output?
function mask(T: BitsType, x: Val<MIR>) {
  const m = bits(sizeof(T) * 8, (1n << BigInt(T.size)) - 1n)
  return stmt(xcall(new WIntrinsic(`${only(layout(T))}.and`), x, m), { type: layout(T) })
}

function extend(pr: Pipe<MIR>, v: number, T: BitsType, x: Val<MIR>): number {
  const n = sizeof(T) * 8
  const shift = bits(n, BigInt(n - T.size))
  x = pr.insert(v, stmt(xcall(new WIntrinsic(`${only(layout(T))}.shl`), x, shift), { type: bits(n) }))
  x = pr.insert(v, stmt(xcall(new WIntrinsic(`${only(layout(T))}.shr_s`), x, shift), { type: bits(n) }))
  return x
}

inlinePrimitive.set(bitcast_method, (pr, ir, v) => {
  if (types.isValue(asType(ir.get(v).type))) {
    pr.set(v, xtuple())
    return
  }
  let x = ir.get(v).expr.body[2]
  const F = asType(pr.type(x), 'bits')
  const T = asType(pr.type(v), 'bits')
  const lT = only(layout(T))
  const lF = only(layout(F))
  if (lT === 'i32' && lF === 'i64')
    x = pr.insert(v, stmt(xcall(new WIntrinsic('i32.wrap_i64'), x), { type: bits(32) }))
  else if (lT === 'i64' && lF === 'i32')
    x = pr.insert(v, stmt(xcall(new WIntrinsic('i64.extend_i32_u'), x), { type: bits(64) }))
  if (T.size < F.size && T.size < sizeof(T) * 8)
    x = pr.insert(v, mask(T, x))
  pr.replace(v, x)
})

inlinePrimitive.set(bitcast_s_method, (pr, ir, v) => {
  if (types.isValue(asType(ir.get(v).type))) {
    pr.set(v, xtuple())
    return
  }
  let x = ir.get(v).expr.body[2]
  const F = asType(pr.type(x), 'bits')
  const T = asType(pr.type(v), 'bits')
  if (T.size <= F.size) return inlinePrimitive.get(bitcast_method)!(pr, ir, v)
  if (F.size < sizeof(F) * 8) x = extend(pr, v, F, x)
  if (isEqual([sizeof(T), sizeof(F)], [8, 4]))
    x = pr.insert(v, stmt(xcall(new WIntrinsic('i64.extend_i32_s'), x), { type: bits(64) }))
  if (T.size < sizeof(T) * 8) x = pr.insert(v, mask(T, x))
  pr.replace(v, x)
})

for (const [op, method] of bitop_methods)
  inlinePrimitive.set(method, (pr, ir, v) => {
    const T = asType(ir.get(v).type, 'bits')
    if (types.isValue(T)) {
      pr.set(v, xtuple())
      return
    }
    let x = ir.get(v).expr.body[1]
    let y = ir.get(v).expr.body[2]
    const sz = sizeof(T) * 8
    if (op.endsWith('_s') && T.size < sz) {
      x = extend(pr, v, T, x)
      y = extend(pr, v, T, y)
    }
    let r = pr.insert(v, stmt(xcall(new WIntrinsic(`${only(layout(T))}.${op}`), x, y), { type: T }))
    if (T.size < sz) r = pr.insert(v, mask(T, r))
    pr.replace(v, r)
  })

for (const [op, method] of bitcmp_methods)
  inlinePrimitive.set(method, (pr, ir, v) => {
    if (types.isValue(asType(ir.get(v).type))) {
      pr.set(v, xtuple())
      return
    }
    let x = ir.get(v).expr.body[1]
    let y = ir.get(v).expr.body[2]
    const T = asType(types.union(asType(ir.type(x)), asType(ir.type(y))), 'bits')
    const sz = sizeof(T) * 8
    if (op.endsWith('_s') && T.size < sz) {
      x = extend(pr, v, T, x)
      y = extend(pr, v, T, y)
    }
    pr.set(v, xcall(new WIntrinsic(`${only(layout(T))}.${op}`), x, y))
  })

inlinePrimitive.set(biteqz_method, (pr, ir, v) => {
  const x = ir.get(v).expr.body[1]
  const T = asType(pr.type(x))
  if (types.isValue(asType(ir.get(v).type))) pr.set(v, xtuple())
  else if (sizeof(T) * 8 === 64) pr.set(v, xcall(new WIntrinsic('i64.eqz'), x))
  else if (sizeof(T) * 8 === 32) pr.set(v, xcall(new WIntrinsic('i32.eqz'), x))
})

function symOverlap(x: Type, y: Type): number[] {
  if (x.kind === 'tag' && y.kind === 'union') return y.options.map((opt, i) => isEqual(x, opt) ? i + 1 : -1).filter(i => i >= 0)
  if (x.kind === 'union' && y.kind === 'tag') return symOverlap(y, x)
  throw new Error('unimplemented')
}

inlinePrimitive.set(shortcutEquals_method, (pr, ir, v) => {
  if (types.isValue(asType(ir.get(v).type))) {
    pr.set(v, xtuple())
    return
  }
  // symbol case
  let a = ir.get(v).expr.body[1]
  let b = ir.get(v).expr.body[2]
  let A = asType(pr.type(a))
  let B = asType(pr.type(b))
  if (B.kind === 'union') [a, b, A, B] = [b, a, B, A]
  const ov = symOverlap(A, B)
  const i = pr.insert(v, stmt(expr('ref', a, i64(1)), { type: bits(32) }))
  pr.set(v, xcall(new WIntrinsic('i32.eq'), i, bits(32, only(ov))))
})

inlinePrimitive.set(isnil_method, (pr, ir, v) => {
  const x = ir.get(v).expr.body[1]
  const T = asType(pr.type(x))
  if (types.isValue(asType(ir.get(v).type))) {
    pr.set(v, xtuple())
  } else {
    if (T.kind !== 'union') throw new Error('unimplemented')
    const i = T.options.findIndex(opt => isEqual(opt, types.nil)) + 1
    const j = pr.insert(v, stmt(expr('ref', x, i64(1)), { type: bits(32) }))
    pr.set(v, xcall(new WIntrinsic('i32.eq'), j, bits(32, i)))
  }
  if (isreftype(T)) pr.insert(v, stmt(expr('release', x)))
})

inlinePrimitive.set(notnil_method, (pr, ir, v) => {
  const x = ir.get(v).expr.body[1]
  const T = asType(pr.type(x))
  const V = asAnno(asType, ir.get(v).type)
  if (isEqual(T, V)) {
    pr.replace(v, x)
  } else if (V === unreachable) {
    // TODO make sure `not` in dispatcher infers
    pr.delete(v)
    pr.replace(v, abort(pr.to, 'notnil(nil)'))
  } else {
    if (!(T.kind === 'union' && V.kind !== 'union')) throw new Error('unimplemented')
    const i = T.options.findIndex(opt => !isEqual(opt, types.nil)) + 1
    pr.delete(v)
    const down = union_downcast(pr, T, i, x)
    pr.replace(v, down)
  }
})

inlinePrimitive.set(tagcast_method, (pr, ir, v) => {
  let x = ir.get(v).expr.body[1]
  let T = asType(pr.type(x))
  const V = asAnno(asType, ir.get(v).type)
  let tg = asType(pr.type(ir.get(v).expr.body[2]), 'tag')
  if (isEqual(T, V))
    pr.replace(v, x)
  else if (V === unreachable) {
    pr.delete(v)
    pr.replace(v, abort(pr.to, 'tagcast'))
  } else {
    pr.delete(v)
    if (T.kind === 'recursive') {
      T = types.unroll(T)
      x = unbox(pr, T, x)
    }
    const i = asType(T, 'union').options.findIndex(opt => isEqual(tagOf(opt), tg)) + 1
    pr.replace(v, union_downcast(pr, T, i, x))
  }
})

function string(pr: MIR | Pipe<MIR>, s: string): number {
  const id = pr.push(stmt(xstring(s), { type: types.list(types.int32()) }))
  return pr.push(stmt(xcall(tag('common.JSObject'), id), { type: types.String() }))
}

inlinePrimitive.set(tagstring_method, (pr, ir, v) => {
  const T = asType(pr.type(ir.get(v).expr.body[1]))
  if (T.kind === 'tag') {
    pr.delete(v)
    const s = string(pr, types.asTag(T).path)
    pr.replace(v, s)
  }
})

outlinePrimitive.set(tagstring_method, (T: Type): MIR => {
  if (T.kind !== 'union') throw new Error('expected union type')
  const code = MIR(new FuncInfo(tag('common.core.tagstring')))
  const x = code.argument(T)
  union_cases(code, T, x, S => string(code, types.asTag(S).path))
  return code
})

// UB if inferred output type is not `O`
// TODO wrap with a type check / conversion
inlinePrimitive.set(function_method, (pr, ir, v) => {
  const [f, I, O] = ir.get(v).expr.body.slice(1, 4).map(x => asType(pr.type(x)))
  if (![f, I, O].every(types.isValue)) throw new Error('nope')
  pr.set(v, expr('func', f, rvtype(I), rvtype(O)))
})

inlinePrimitive.set(invoke_method, (pr, ir, v) => {
  const [f, I0, O0, args0] = ir.get(v).expr.body.slice(1, 5)
  const [I, O] = [I0, O0].map(x => rvtype(asType(pr.type(x))))
  // TODO conversion
  if (!types.issubset(asType(pr.type(args0)), I)) throw new Error('invoke: argument type mismatch')
  pr.delete(v)
  const args = cast(pr, asType(pr.type(args0)), I, args0)
  const v2 = pr.push({ ...ir.get(v), expr: expr('call_indirect', f, args) })
  pr.replace(v, v2)
})

inlinePrimitive.set(jsalloc_method, (pr, ir, v) => { pr.set(v, xtuple()) })

// Core module

function core() {
  const mod = new Module(tag('common.core'))
  for (const meth of primitives()) mod.methods.method(meth)
  return mod
}
