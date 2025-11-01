import * as types from '../frontend/types'
import { Type, tagOf, tag, asType, bits } from '../frontend/types'
import { unreachable, Anno, expr, asAnno, Val, Fragment } from '../utils/ir'
import { HashSet, only, some } from '../utils/map'
import isEqual from 'lodash/isEqual'
import { Method, MIR, Module, WIntrinsic, Value, xstring } from '../frontend/modules'
import { Def } from '../dwarf'
import { xtuple, xcall } from '../frontend/lower'
import { xwasm } from '../frontend/modules'
import { lowerpattern } from '../frontend/patterns'
import * as parse from '../frontend/parse'
import { inlinePrimitive, outlinePrimitive } from './prim_map'
import { abort, call, layout, sizeof, unbox, union_downcast, union_cases, cast, partir, packir, set_pack, indexer, setir, copyir } from './expand'
import { isreftype } from './refcount'
import { options } from '../utils/options'
import { maybe_union } from './abstract'
import { asNumType } from '../wasm/wasm'

export { core, symbolValues, string, inlinePrimitive, outlinePrimitive, invoke_method, pack_method, packcat_method, part_method, isnil_method, notnil_method, copy_method, partial_isnil, partial_part, partial_set, getIntValue, nparts, primitive, constValue }

const i64 = Value.i64

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
  return tag('common.Int').isEqual(tagOf(x)) &&
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

function partial_set(xs: Type, i: Type, x: Type): Anno<Type> {
  if (xs.kind === 'pack') {
    const idx = some(getIntValue(i))
    const part = partial_part(xs, i)
    if (part === unreachable) return unreachable
    if (idx < 0 || idx >= xs.parts.length) return unreachable
    const parts = xs.parts.slice()
    parts[idx] = x
    return types.pack(...parts)
  } else if (xs.kind === 'vpack') {
    const part = partial_part(xs, i)
    if (part === unreachable) return unreachable
    const idx = getIntValue(i)
    if (idx !== undefined && idx <= 0) return unreachable
    return types.vpack(xs.tag, types.union(xs.parts, x))
  }
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
    if (tag('common.Int').isEqual(tg) || tag('common.Bool').isEqual(tg))
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
  if (tag('common.List').isEqual(tagOf(x)))
    return types.pack(tagOf(x), ...types.parts(x).map(rvtype))
  if (tag('common.Pack').isEqual(tagOf(x)))
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
const set_method = primitive('common.core.set', '[xs, i, x]', partial_set)
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
    set_method,
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

inlinePrimitive.set(pack_method.id, (code, st) => {
  const T = asType(st.type)
  if (isEqual(T, types.float64())) {
    const arg = st.expr.body[0]
    const ref = code.push(code.stmt(expr('ref', arg, i64(1)), { type: bits(64) }))
    return code.push({ ...st, expr: xwasm(new WIntrinsic('f64.reinterpret_i64'), ref) })
  } else if (isEqual(T, types.float32())) {
    const arg = st.expr.body[0]
    const ref = code.push(code.stmt(expr('ref', arg, i64(1)), { type: bits(32) }))
    return code.push({ ...st, expr: xwasm(new WIntrinsic('f32.reinterpret_i32'), ref) })
  } else {
    // Arguments are turned into a tuple when calling any function, so this
    // is just a cast.
    const x = st.expr.body[0]
    if (!isEqual(layout(T), layout(asType(code.type(x))))) throw new Error('pack: layout mismatch')
    return x
  }
})

inlinePrimitive.set(part_method.id, (code, st) => {
  let [x, i] = st.expr.body
  let [T, I] = [x, i].map(x => asType(code.type(x)))
  if (T.kind === 'recursive') {
    T = types.unroll(T)
    x = unbox(code, T, x)
  }
  if ((T.kind === 'pack' && !types.isValue(I)) || T.kind === 'union')
    return code.push({ ...st, expr: xcall(part_method, x, i) })
  const y = indexer(code, T, I, x, i)
  if (partial_part(T, I) !== unreachable) {
    if (isreftype(asType(partial_part(T, I)))) code.push(code.stmt(expr('retain', y)))
    if (isreftype(T)) code.push(code.stmt(expr('release', x)))
  }
  return y
})

outlinePrimitive.set(part_method.id, partir)

const copy_method = primitive('common.core.copy', '[src, dst, len]', (...xs) => { throw new Error('unimplemented') })
outlinePrimitive.set(copy_method.id, copyir)

inlinePrimitive.set(packcat_method.id, (code, st) => {
  const x = st.expr.body[0]
  const S = asType(code.type(x))
  const T = asType(st.type)
  if (types.isValue(T)) return T
  if ((S.kind === 'pack' && types.isAtom(T)) || T.kind === 'pack') {
    if (!isEqual(layout(S), layout(T))) throw new Error('packcat: layout mismatch')
    return code.push({ ...st, expr: xtuple(x) })
  }
  return code.push(st)
})

outlinePrimitive.set(packcat_method.id, packir)

inlinePrimitive.set(set_method.id, (code, st) => {
  if (asType(st.type).kind === 'pack') {
    const [xs, i, x] = st.expr.body
    return set_pack(code, xs, i, x)
  }
  return code.push(st)
})

outlinePrimitive.set(set_method.id, setir)

function nparts(code: Fragment<MIR>, T: Type, x: Val<MIR>): Val<MIR> {
  if (T.kind === 'recursive') {
    T = types.unroll(T)
    x = unbox(code, T, x)
  }
  if (T.kind === 'vpack') {
    const sz = code.push(code.stmt(expr('ref', x, i64(1)), { type: types.int32() }))
    code.push(code.stmt(expr('release', x)))
    return call(code, types.tag('common.Int64'), [sz], types.int64())
  } else {
    code.push(code.stmt(expr('release', x)))
    return types.int64(types.nparts(T))
  }
}

inlinePrimitive.set(nparts_method.id, (code, st) => {
  let x = st.expr.body[0]
  let T = asType(code.type(x))
  if (T.kind === 'recursive') {
    T = types.unroll(T)
    x = unbox(code, T, x)
  }
  if (T.kind === 'union')
    return code.push({ ...st, expr: xcall(nparts_method, x) })
  const y = nparts(code, T, x)
  if (isreftype(T)) code.push(code.stmt(expr('release', x)))
  return y
})

outlinePrimitive.set(nparts_method.id, (x: Type): MIR => {
  if (x.kind !== 'union') throw new Error('expected union type')
  const code = MIR(Def('common.core.nparts'))
  const retT = partial_nparts(x)
  const vx = code.argument(x)
  union_cases(code, x, vx, (T, val) => {
    // TODO possibly insert `nparts_method` calls and redo lowering
    let ret = nparts(code, T, val)
    ret = cast(code, partial_nparts(T), retT, ret)
    if (isreftype(T)) code.push(code.stmt(expr('release', val)))
    return ret
  })
  return code
})

function constValue(T: Type): Value | undefined {
  if (T.kind === 'bits' && T.value !== undefined)
    return Value.from(asNumType(only(layout(types.abstract(T)))), BigInt.asIntN(T.size, T.value))
  if (T.kind === 'float32' && T.value !== undefined)
    return Value.f32(T.value)
  if (T.kind === 'float64' && T.value !== undefined)
    return Value.f64(T.value)
}

inlinePrimitive.set(widen_method.id, (code, st) => {
  const x = st.expr.body[0]
  const T = asType(code.type(x))
  if (types.isAtom(T) && types.isValue(T)) return constValue(T) ?? T
  if (tag('common.Int').isEqual(tagOf(T)) || tag('common.Bool').isEqual(tagOf(T)))
    return constValue(types.part(T, 1)) ?? types.part(T, 1)
  return x
})

inlinePrimitive.set(bitsize_method.id, (code, st) => asType(st.type))

type BitsType = Type & { kind: 'bits' }

// TODO use Const rather than BitsType in the output?
function mask(code: Fragment<MIR>, T: BitsType, x: Val<MIR>): Val<MIR> {
  const m = bits(sizeof(T) * 8, (1n << BigInt(T.size)) - 1n)
  x = code.push(code.stmt(xwasm(new WIntrinsic(`${only(layout(T))}.and`), x, m), { type: layout(T) }))
  return x
}

function extend(code: Fragment<MIR>, T: BitsType, x: Val<MIR>): Val<MIR> {
  const n = sizeof(T) * 8
  const shift = bits(n, BigInt(n - T.size))
  x = code.push(code.stmt(xwasm(new WIntrinsic(`${only(layout(T))}.shl`), x, shift), { type: bits(n) }))
  x = code.push(code.stmt(xwasm(new WIntrinsic(`${only(layout(T))}.shr_s`), x, shift), { type: bits(n) }))
  return x
}

inlinePrimitive.set(bitcast_method.id, (code, st) => {
  if (types.isValue(asType(st.type))) return asType(st.type)
  let x = st.expr.body[1]
  const F = asType(code.type(x), 'bits')
  const T = asType(st.type, 'bits')
  const lT = only(layout(T))
  const lF = only(layout(F))
  if (lT === 'i32' && lF === 'i64')
    x = code.push(code.stmt(xwasm(new WIntrinsic('i32.wrap_i64'), x), { type: bits(32) }))
  else if (lT === 'i64' && lF === 'i32')
    x = code.push(code.stmt(xwasm(new WIntrinsic('i64.extend_i32_u'), x), { type: bits(64) }))
  if (T.size < F.size && T.size < sizeof(T) * 8)
    x = mask(code, T, x)
  return x
})

inlinePrimitive.set(bitcast_s_method.id, (code, st) => {
  if (types.isValue(asType(st.type))) return asType(st.type)
  let x = st.expr.body[1]
  const F = asType(code.type(x), 'bits')
  const T = asType(st.type, 'bits')
  if (T.size <= F.size) return inlinePrimitive.get(bitcast_method.id)!(code, st)
  if (F.size < sizeof(F) * 8) x = extend(code, F, x)
  if (isEqual([sizeof(T), sizeof(F)], [8, 4])) x = code.push(code.stmt(xwasm(new WIntrinsic('i64.extend_i32_s'), x), { type: bits(64) }))
  if (T.size < sizeof(T) * 8) x = mask(code, T, x)
  return x
})

for (const [op, method] of bitop_methods)
  inlinePrimitive.set(method.id, (code, st) => {
    const T = asType(st.type, 'bits')
    if (types.isValue(T)) return T
    let x = st.expr.body[0]
    let y = st.expr.body[1]
    const sz = sizeof(T) * 8
    if (op.endsWith('_s') && T.size < sz) {
      x = extend(code, T, x)
      y = extend(code, T, y)
    }
    let result: Val<MIR> = code.push({ ...st, expr: xwasm(new WIntrinsic(`${only(layout(T))}.${op}`), x, y) })
    if (T.size < sz) result = mask(code, T, result)
    return result
  })

for (const [op, method] of bitcmp_methods)
  inlinePrimitive.set(method.id, (code, st) => {
    if (types.isValue(asType(st.type))) return asType(st.type)
    let x = st.expr.body[0]
    let y = st.expr.body[1]
    const T = asType(types.union(asType(code.type(x)), asType(code.type(y))), 'bits')
    const sz = sizeof(T) * 8
    if (op.endsWith('_s') && T.size < sz) {
      x = extend(code, T, x)
      y = extend(code, T, y)
    }
    return code.push({ ...st, expr: xwasm(new WIntrinsic(`${only(layout(T))}.${op}`), x, y) })
  })

inlinePrimitive.set(biteqz_method.id, (code, st) => {
  const x = st.expr.body[0]
  const T = asType(code.type(x))
  if (types.isValue(asType(st.type))) return asType(st.type)
  if (sizeof(T) * 8 === 64) return code.push({ ...st, expr: xwasm(new WIntrinsic('i64.eqz'), x) })
  if (sizeof(T) * 8 === 32) return code.push({ ...st, expr: xwasm(new WIntrinsic('i32.eqz'), x) })
  throw new Error('unimplemented')
})

function symOverlap(x: Type, y: Type): number[] {
  if (x.kind === 'tag' && y.kind === 'union') return y.options.map((opt, i) => isEqual(x, opt) ? i + 1 : -1).filter(i => i >= 0)
  if (x.kind === 'union' && y.kind === 'tag') return symOverlap(y, x)
  throw new Error('unimplemented')
}

inlinePrimitive.set(shortcutEquals_method.id, (code, st) => {
  if (types.isValue(asType(st.type))) return asType(st.type)
  let a = st.expr.body[0]
  let b = st.expr.body[1]
  let A = asType(code.type(a))
  let B = asType(code.type(b))
  if (B.kind === 'union') [a, b, A, B] = [b, a, B, A]
  const ov = symOverlap(A, B)
  const i = code.push(code.stmt(expr('ref', a, i64(1)), { type: bits(32) }))
  return code.push({ ...st, expr: xwasm(new WIntrinsic('i32.eq'), i, bits(32, only(ov))) })
})

inlinePrimitive.set(isnil_method.id, (code, st) => {
  const x = st.expr.body[0]
  const T = asType(code.type(x))
  if (types.isValue(asType(st.type))) return asType(st.type)
  if (T.kind !== 'union') throw new Error('unimplemented')
  const i = T.options.findIndex(opt => isEqual(opt, types.nil)) + 1
  const j = code.push(code.stmt(expr('ref', x, i64(1)), { type: bits(32) }))
  const result = code.push({ ...st, expr: xwasm(new WIntrinsic('i32.eq'), j, bits(32, i)) })
  if (isreftype(T)) code.push(code.stmt(expr('release', x)))
  return result
})

inlinePrimitive.set(notnil_method.id, (code, st) => {
  const x = st.expr.body[0]
  const T = asType(code.type(x))
  const V = asAnno(asType, st.type)
  if (isEqual(T, V)) return x
  if (V === unreachable)
    // TODO make sure `not` in dispatcher infers
    return abort(code, 'notnil(nil)')
  if (!(T.kind === 'union' && V.kind !== 'union')) throw new Error('unimplemented')
  const i = T.options.findIndex(opt => !isEqual(opt, types.nil)) + 1
  return union_downcast(code, T, i, x)
})

inlinePrimitive.set(tagcast_method.id, (code, st) => {
  let x = st.expr.body[0]
  let T = asType(code.type(x))
  const V = asAnno(asType, st.type)
  const tg = asType(code.type(st.expr.body[1]), 'tag')
  if (isEqual(T, V)) return x
  if (V === unreachable) return abort(code, 'tagcast')
  if (T.kind === 'recursive') {
    T = types.unroll(T)
    x = unbox(code, T, x)
  }
  const i = asType(T, 'union').options.findIndex(opt => isEqual(tagOf(opt), tg)) + 1
  return union_downcast(code, T, i, x)
})

function string(pr: Fragment<MIR>, s: string) {
  if (options().gc) {
    return pr.push(pr.stmt(xstring(s), { type: types.String() }))
  } else {
    const id = pr.push(pr.stmt(xstring(s), { type: types.list(types.int32()) }))
    return pr.push(pr.stmt(xcall(tag('common.JSObject'), id), { type: types.String() }))
  }
}

inlinePrimitive.set(tagstring_method.id, (code, st) => {
  const T = asType(code.type(st.expr.body[0]))
  if (T.kind === 'tag') return string(code, types.asTag(T).path)
  return code.push(st)
})

outlinePrimitive.set(tagstring_method.id, (T: Type): MIR => {
  if (T.kind !== 'union') throw new Error('expected union type')
  const code = MIR(Def('common.core.tagstring'))
  const x = code.argument(T)
  union_cases(code, T, x, S => string(code, types.asTag(S).path))
  return code
})

// UB if inferred output type is not `O`
// TODO wrap with a type check / conversion
inlinePrimitive.set(function_method.id, (code, st) => {
  const [f, I, O] = st.expr.body.slice(0, 3).map(x => asType(code.type(x)))
  if (![f, I, O].every(types.isValue)) throw new Error('nope')
  return code.push({ ...st, expr: expr('func', f, rvtype(I), rvtype(O)) })
})

inlinePrimitive.set(invoke_method.id, (code, st) => {
  const [f, I0, O0, args0] = st.expr.body.slice(0, 4)
  const [I, O] = [I0, O0].map(x => rvtype(asType(code.type(x))))
  // TODO conversion
  if (!types.issubset(asType(code.type(args0)), I)) throw new Error('invoke: argument type mismatch')
  const args = cast(code, asType(code.type(args0)), I, args0)
  return code.push({ ...st, expr: expr('call_indirect', f, args) })
})

inlinePrimitive.set(jsalloc_method.id, (code, st) => asType(st.type))

// Core module

function core() {
  const mod = new Module(tag('common.core'))
  for (const meth of primitives()) mod.methods.method(meth)
  return mod
}
