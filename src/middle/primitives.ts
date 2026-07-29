import * as types from '../frontend/types.js'
import { Type, tagOf, tag, bits } from '../frontend/types.js'
import { unreachable, Anno, expr, Val, Fragment, asType } from '../utils/ir.js'
import { HashMap, HashSet, only, some } from '../utils/map.js'
import { isEqual } from '../utils/isEqual.js'
import { Method, MIR, Module, Value, xfunc, xstring } from '../frontend/modules.js'
import { Def } from '../dwarf/index.js'
import { xtuple, xcall } from '../frontend/lower.js'
import { xwasm } from '../frontend/modules.js'
import { inlinePrimitives, InvokeSt, outlinePrimitives, inlinePrimitive, outlinePrimitive, primitive, sources } from './prim_map.js'
import { releaseFunction_method } from './refcount.js'
import { abort, call, layout, wlayout, sizeof, unbox, union_downcast, union_cases, cast, partir, packir, set_pack, indexer, setir, copyir, i32, store, load } from './expand.js'
import { isreftype } from './refcount.js'
import { maybe_union, traitType } from './abstract.js'
import { GetGlobal, SetGlobal } from '../wasm/wasm.js'
import { xref } from '../wasm/ir.js'

export { core, symbolValues, string, inlinePrimitives, outlinePrimitives, inlinePrimitive, outlinePrimitive, invoke_method, invokeFunction_method, pack_method, packcat_method, part_method, isnil_method, notnil_method, tagcast_method, copy_method, load_method, store_method, partial_isnil, partial_part, partial_set, getIntValue, nparts, primitive, constValue }

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
  return x.kind === 'pack' && tag('common.integer.Int').isEqual(tagOf(x)) &&
    x.parts[1].kind === 'bits' && (size === undefined || x.parts[1].size === size)
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
  if (data.kind === 'any') return types.Any
  const idx = getIntValue(i)
  // TODO: HACK: we assume index != 0 when indexing dynamically.
  // Should instead have a seperate `index` function that enforces this.
  if (idx === undefined) return partial_eltype(data)
  if (types.isAtom(data) || data.kind === 'pack' || data.kind === 'closure')
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
  if (x.kind === 'vpack') return types.pack(tag('common.integer.Int'), bits(64))
  if (x.kind === 'any') return types.pack(tag('common.integer.Int'), bits(64))
  return types.pack(tag('common.integer.Int'), bits(64, types.nparts(x)))
}

function partial_widen(x: Type): Type {
  if (types.isAtom(x)) return types.abstract(x)
  if (x.kind === 'pack') {
    const tg = types.tagOf(x)
    if (tag('common.integer.Int').isEqual(tg) || tag('common.integer.Bool').isEqual(tg))
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
  if (a.kind === 'any' || b.kind === 'any') return types.bool()
  const intersection = new Set([...symbolValues(a)].filter(x => symbolValues(b).has(x)))
  if (intersection.size > 0) return types.pack(tag('common.integer.Bool'), bits(1))
  return Type(false)
}

function partial_bitsize(x: Type): Type {
  if (x.kind === 'bits') return Type(BigInt(x.size))
  return types.int64()
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
  if (x.kind === 'any') return types.pack(tag('common.integer.Bool'), bits(1))
  if (types.issubset(types.nil, x)) return types.pack(tag('common.integer.Bool'), bits(1))
  return Type(false)
}

function partial_notnil(x: Type): Anno<Type> {
  if (!types.issubset(types.nil, x)) return x
  if (x.kind === 'pack') return isEqual(x, types.nil) ? unreachable : x
  if (x.kind === 'union') return types.onion(...x.options.filter(opt => !isEqual(opt, types.nil)))
  if (x.kind === 'recursive') return types.recursive(partial_notnil(types.unroll(x)) as Type)
  throw new Error('unreachable')
}

const corePrimitive = new HashMap<Type, Type>([
  [tag('common.core.Float32'), types.float32()],
  [tag('common.core.Float64'), types.float64()],
  [tag('common.core.Ref'), types.Ref],
  [tag('common.core.Func'), types.Func]
])

function partial_tagcast(x: Type, t: Type): Anno<Type> {
  const n = getIntValue(t)
  if (n !== undefined) return types.bits(n)
  if (!(t instanceof types.Tag)) throw new Error('t must be a tag')
  x = types.unroll(x)
  if (x.kind === 'any') return corePrimitive.get(t) ?? types.vpack(t, types.Any)
  if (x.kind !== 'union') return t.isEqual(tagOf(x)) ? x : unreachable
  const ps = x.options.filter(opt => t.isEqual(tagOf(opt)))
  return ps.length === 0 ? unreachable : types.onion(...ps)
}

function partial_tagstring(x: Type): Type {
  return types.String()
}

function rvtype(x: Type): Type {
  if (!types.isValue(x)) throw new Error('Expected value')
  if (tag('common.list.List').isEqual(tagOf(x)))
    return types.pack(tagOf(x), ...types.parts(x).map(rvtype))
  const T = traitType(x)
  if (T === unreachable || types.occursin(types.Any, T)) throw new Error(`Invalid type ${types.repr(x)}`)
  return T
}

function partial_function(f: Type, I: Type, O: Type): Type {
  return types.Func
}

function partial_invoke(f: Type, I: Type, O: Type, ...xs: Type[]): Type {
  return rvtype(O)
}

const pack_method = primitive('common.core.pack', '[args...]', (args: Type) => types.pack(...types.parts(args)))
const part_method = primitive('common.core.part', '[data, i]', partial_part)
const nparts_method = primitive('common.core.nparts', '[x]', partial_nparts)
const packcat_method = primitive('common.core.packcat', '[args...]', (args: Type) => { const parts = types.parts(args); return parts.length === 0 ? unreachable : parts.reduce((x, y) => types.packcat(x, y)) })
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
const invokeFunction_method = primitive('common.core.invokeFunction', '[f, xs]')

const alloc_method = primitive('common.core.alloc', '[T, n]', (T: Type, n: Type) => types.Ptr())
const load_method = primitive('common.core.load', '[T, ptr, i]', (T: Type, ptr: Type, i: Type) => rvtype(T))
const store_method = primitive('common.core.store', '[T, ptr, i, x]', (T: Type, ptr: Type, i: Type, x: Type) => types.nil)
const length_method = primitive('common.core.length', '[ptr]', (ptr: Type) => types.int64())

const allocs_method = primitive('common.core.allocs', '[n]', (n: Type) => isInt(n, 32) ? types.int32() : unreachable)
const frees_method = primitive('common.core.frees', '[n]', (n: Type) => isInt(n, 32) ? types.int32() : unreachable)

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
    alloc_method,
    load_method,
    store_method,
    length_method,
    allocs_method,
    frees_method,
  ]
}

// Primitive implementations
// Invoked from expansion. `inline` primitives replace the call with a
// definition. `outline` ones return an expanded IR fragment, to be called as a
// normal function.

inlinePrimitives.set(pack_method.id, (code, st) => {
  const T = asType(st.type)
  if (isEqual(T, types.float64())) {
    const arg = st.expr.body[0]
    const ref = code.push(code.stmt(xref(arg, 1), { type: bits(64) }))
    return code.push({ ...st, expr: xwasm('f64.reinterpret_i64', ref) })
  } else if (isEqual(T, types.float32())) {
    const arg = st.expr.body[0]
    const ref = code.push(code.stmt(xref(arg, 1), { type: bits(32) }))
    return code.push({ ...st, expr: xwasm('f32.reinterpret_i32', ref) })
  } else {
    // Arguments are turned into a tuple when calling any function, so this
    // is just a cast.
    const x = st.expr.body[0]
    const S = asType(code.type(x))
    if (isEqual(T, S)) return x
    if (!isEqual(layout(T), layout(S))) throw new Error('pack: layout mismatch')
    if (types.isValue(T)) return T
    return code.push({ ...st, expr: xtuple(x) })
  }
})

inlinePrimitives.set(part_method.id, (code, st) => {
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

outlinePrimitives.set(part_method.id, partir)

const copy_method = primitive('common.core.copy', '[src, dst, len]', (...xs) => { throw new Error('unimplemented') })
outlinePrimitives.set(copy_method.id, copyir)

inlinePrimitives.set(packcat_method.id, (code, st) => {
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

outlinePrimitives.set(packcat_method.id, packir)

inlinePrimitives.set(set_method.id, (code, st) => {
  if (asType(st.type).kind === 'pack') {
    const [xs, i, x] = st.expr.body
    return set_pack(code, xs, i, x)
  }
  return code.push(st)
})

outlinePrimitives.set(set_method.id, setir)

function nparts(code: Fragment<MIR>, T: Type, x: Val<MIR>): Val<MIR> {
  if (T.kind === 'recursive') {
    T = types.unroll(T)
    x = unbox(code, T, x)
  }
  if (T.kind === 'vpack') {
    const sz = code.push(code.stmt(xref(x, 1), { type: types.int32() }))
    code.push(code.stmt(expr('release', x)))
    return call(code, types.tag('common.Int64'), [sz], types.int64())
  } else {
    code.push(code.stmt(expr('release', x)))
    return types.int64(types.nparts(T))
  }
}

inlinePrimitives.set(nparts_method.id, (code, st) => {
  let x = st.expr.body[0]
  let T = asType(code.type(x))
  if (T.kind === 'recursive') {
    T = types.unroll(T)
    x = unbox(code, T, x)
  }
  if (T.kind === 'union')
    return code.push({ ...st, expr: xcall(nparts_method, x) })
  const y = nparts(code, T, x)
  return y
})

outlinePrimitives.set(nparts_method.id, (x: Type): MIR => {
  if (x.kind !== 'union') throw new Error('expected union type')
  const code = MIR(Def('common.core.nparts'))
  const retT = partial_nparts(x)
  const vx = code.argument(x)
  union_cases(code, x, vx, (T, val) => {
    // TODO possibly insert `nparts_method` calls and redo lowering
    let ret = nparts(code, T, val)
    ret = cast(code, partial_nparts(T), retT, ret)
    return ret
  })
  return code
})

function constValue(T: Type): Value | undefined {
  if (['bits', 'float32', 'float64'].includes(T.kind) && (T as any).value !== undefined)
    return Value.from(T)
}

inlinePrimitives.set(widen_method.id, (code, st) => {
  const x = st.expr.body[0]
  const T = asType(code.type(x))
  if (types.isAtom(T) && types.isValue(T)) return constValue(T) ?? T
  if ((tag('common.integer.Int').isEqual(tagOf(T)) || tag('common.integer.Bool').isEqual(tagOf(T))) && types.isValue(T))
    return code.push(code.stmt(xtuple(some(constValue(types.part(T, 1)))), { type: asType(st.type) }))
  return x
})

inlinePrimitives.set(bitsize_method.id, (code, st) => asType(st.type))

type BitsType = Type & { kind: 'bits' }

// TODO use Const rather than BitsType in the output?
function mask(code: Fragment<MIR>, T: BitsType, x: Val<MIR>): Val<MIR> {
  const m = bits(sizeof(T) * 8, (1n << BigInt(T.size)) - 1n)
  x = code.push(code.stmt(xwasm(`${only(wlayout(T))}.and`, x, m), { type: only(layout(T)) }))
  return x
}

function extend(code: Fragment<MIR>, T: BitsType, x: Val<MIR>): Val<MIR> {
  const n = sizeof(T) * 8
  const shift = bits(n, BigInt(n - T.size))
  x = code.push(code.stmt(xwasm(`${only(wlayout(T))}.shl`, x, shift), { type: bits(n) }))
  x = code.push(code.stmt(xwasm(`${only(wlayout(T))}.shr_s`, x, shift), { type: bits(n) }))
  return x
}

inlinePrimitives.set(bitcast_method.id, (code, st) => {
  if (types.isValue(asType(st.type))) return asType(st.type)
  let x = st.expr.body[1]
  const F = asType(code.type(x))
  const T = asType(st.type)
  if (F.kind !== 'bits' || T.kind !== 'bits') throw new Error('bitcast: expected bits')
  const lT = only(wlayout(T))
  const lF = only(wlayout(F))
  if (lT === 'i32' && lF === 'i64')
    x = code.push(code.stmt(xwasm('i32.wrap_i64', x), { type: bits(32) }))
  else if (lT === 'i64' && lF === 'i32')
    x = code.push(code.stmt(xwasm('i64.extend_i32_u', x), { type: bits(64) }))
  if (T.size < F.size && T.size < sizeof(T) * 8)
    x = mask(code, T, x)
  return x
})

inlinePrimitives.set(bitcast_s_method.id, (code, st) => {
  if (types.isValue(asType(st.type))) return asType(st.type)
  let x = st.expr.body[1]
  const F = asType(code.type(x))
  const T = asType(st.type)
  if (F.kind !== 'bits' || T.kind !== 'bits') throw new Error('bitcast_s: expected bits')
  if (T.size <= F.size) return some(inlinePrimitive(bitcast_method))(code, st)
  if (F.size < sizeof(F) * 8) x = extend(code, F, x)
  if (isEqual([sizeof(T), sizeof(F)], [8, 4])) x = code.push(code.stmt(xwasm('i64.extend_i32_s', x), { type: bits(64) }))
  if (T.size < sizeof(T) * 8) x = mask(code, T, x)
  return x
})

for (const [op, method] of bitop_methods)
  inlinePrimitives.set(method.id, (code, st) => {
    const T = asType(st.type)
    if (types.isValue(T)) return T
    if (T.kind !== 'bits') throw new Error('bitop: expected bits')
    let x = st.expr.body[0]
    let y = st.expr.body[1]
    const sz = sizeof(T) * 8
    if (op.endsWith('_s') && T.size < sz) {
      x = extend(code, T, x)
      y = extend(code, T, y)
    }
    let result: Val<MIR> = code.push({ ...st, expr: xwasm(`${only(wlayout(T))}.${op}`, x, y) })
    if (T.size < sz) result = mask(code, T, result)
    return result
  })

for (const [op, method] of bitcmp_methods)
  inlinePrimitives.set(method.id, (code, st) => {
    if (types.isValue(asType(st.type))) return asType(st.type)
    let x = st.expr.body[0]
    let y = st.expr.body[1]
    const T = types.union(asType(code.type(x)), asType(code.type(y)))
    if (T.kind !== 'bits') throw new Error('bitcmp: expected bits')
    const sz = sizeof(T) * 8
    if (op.endsWith('_s') && T.size < sz) {
      x = extend(code, T, x)
      y = extend(code, T, y)
    }
    return code.push({ ...st, expr: xwasm(`${only(wlayout(T))}.${op}`, x, y) })
  })

inlinePrimitives.set(biteqz_method.id, (code, st) => {
  const x = st.expr.body[0]
  const T = asType(code.type(x))
  if (types.isValue(asType(st.type))) return asType(st.type)
  if (sizeof(T) * 8 === 64) return code.push({ ...st, expr: xwasm('i64.eqz', x) })
  if (sizeof(T) * 8 === 32) return code.push({ ...st, expr: xwasm('i32.eqz', x) })
  throw new Error('unimplemented')
})

function symOverlap(x: Type, y: Type): number[] {
  if (x.kind === 'tag' && y.kind === 'union') return y.options.map((opt, i) => isEqual(x, opt) ? i + 1 : -1).filter(i => i >= 0)
  if (x.kind === 'union' && y.kind === 'tag') return symOverlap(y, x)
  throw new Error('unimplemented')
}

inlinePrimitives.set(shortcutEquals_method.id, (code, st) => {
  if (types.isValue(asType(st.type))) return asType(st.type)
  let a = st.expr.body[0]
  let b = st.expr.body[1]
  let A = asType(code.type(a))
  let B = asType(code.type(b))
  if (B.kind === 'union') [a, b, A, B] = [b, a, B, A]
  const ov = symOverlap(A, B)
  const i = code.push(code.stmt(xref(a, 1), { type: bits(32) }))
  return code.push({ ...st, expr: xwasm('i32.eq', i, bits(32, only(ov))) })
})

inlinePrimitives.set(isnil_method.id, (code, st) => {
  const x = st.expr.body[0]
  const T = asType(code.type(x))
  if (types.isValue(asType(st.type))) return asType(st.type)
  if (T.kind !== 'union') throw new Error('unimplemented')
  const i = T.options.findIndex(opt => isEqual(opt, types.nil)) + 1
  const j = code.push(code.stmt(xref(x, 1), { type: bits(32) }))
  const result = code.push({ ...st, expr: xwasm('i32.eq', j, bits(32, i)) })
  if (isreftype(T)) code.push(code.stmt(expr('release', x)))
  return result
})

inlinePrimitives.set(notnil_method.id, (code, st) => {
  const x = st.expr.body[0]
  const T = asType(code.type(x))
  const V = st.type
  if (isEqual(T, V)) return x
  if (V === unreachable)
    // TODO make sure `not` in dispatcher infers
    return abort(code, 'notnil(nil)')
  if (!(T.kind === 'union' && V.kind !== 'union')) throw new Error('unimplemented')
  const i = T.options.findIndex(opt => !isEqual(opt, types.nil)) + 1
  return union_downcast(code, T, i, x)
})

inlinePrimitives.set(tagcast_method.id, (code, st) => {
  let x = st.expr.body[0]
  let T = asType(code.type(x))
  const V = st.type
  const tg = asType(code.type(st.expr.body[1]), 'tag')
  if (isEqual(T, V)) return x
  if (V === unreachable) return abort(code, 'tagcast')
  if (T.kind === 'recursive') {
    T = types.unroll(T)
    x = unbox(code, T, x)
  }
  if (!(T.kind === 'union')) throw new Error('unimplemented')
  const i = asType(T, 'union').options.findIndex(opt => types.asTag(tg).isEqual(tagOf(opt))) + 1
  return union_downcast(code, T, i, x)
})

function string(pr: Fragment<MIR>, s: string) {
  return pr.push(pr.stmt(xstring(s), { type: types.String() }))
}

inlinePrimitives.set(tagstring_method.id, (code, st) => {
  const T = asType(code.type(st.expr.body[0]))
  if (T.kind === 'tag') return string(code, types.asTag(T).path)
  return code.push(st)
})

outlinePrimitives.set(tagstring_method.id, (T: Type): MIR => {
  if (T.kind !== 'union') throw new Error('expected union type')
  const code = MIR(Def('common.core.tagstring'))
  const x = code.argument(T)
  union_cases(code, T, x, S => string(code, types.asTag(S).path))
  return code
})

// UB if inferred output type is not `O`
// TODO wrap with a type check / conversion
inlinePrimitives.set(function_method.id, (code, st) => {
  const [F, I, O] = st.expr.body.slice(0, 3).map(x => asType(code.type(x)))
  if (![I, O].every(types.isValue)) throw new Error('nope')
  const id = code.push(code.stmt(xfunc(invokeFunction_method.param(F, rvtype(O)), types.int32(), rvtype(I)), { type: types.bits(32) }))
  const ptr = call(code, types.tag('common.wasm.malloc.malloc!'), [i32(code, 8 + sizeof(F))], types.int32())
  code.push(code.stmt(xwasm('i32.store', ptr, id), { type: types.nil }))
  const release = code.push(code.stmt(xwasm('i32.add', ptr, Value.bits(32, 4)), { type: types.int32() }))
  const drop = code.push(code.stmt(xfunc(releaseFunction_method.param(F), types.int32()), { type: types.bits(32) }))
  code.push(code.stmt(xwasm('i32.store', release, drop), { type: types.nil }))
  const data = code.push(code.stmt(xwasm('i32.add', ptr, Value.bits(32, 8)), { type: types.int32() }))
  store(code, F, data, st.expr.body[0])
  return code.push(code.stmt(xtuple(ptr), { type: asType(st.type) }))
})

inlinePrimitives.set(invoke_method.id, (code, st) => {
  const [f, I0, , args0] = st.expr.body.slice(0, 4)
  const I = rvtype(asType(code.type(I0)))
  // TODO conversion
  if (!types.issubset(asType(code.type(args0)), I)) throw new Error('invoke: argument type mismatch')
  const args = cast(code, asType(code.type(args0)), I, args0)
  const id = code.push(code.stmt(xwasm('i32.load', f), { type: types.bits(32) }))
  const env = code.push(code.stmt(xwasm('i32.add', f, Value.bits(32, 8)), { type: types.int32() }))
  const result = code.push({ ...st, expr: expr('call_indirect', id, env, args) })
  code.push(code.stmt(expr('release', f)))
  return result
})

outlinePrimitives.set(invokeFunction_method.id, (F: Type, O: Type, _: Type, I: Type): MIR => {
  const code = MIR(Def('common.core.invokeFunction'))
  const ptr = code.argument(types.int32())
  const args = code.argument(I)
  code.return(code.push(code.stmt(xcall(load(code, F, ptr), args), { type: O })))
  return code
})

function ptrOffset(code: Fragment<MIR>, ptr: Val<MIR>, i: Val<MIR>, I: Type, T: Type): Val<MIR> {
  ptr = call(code, types.tag('common.+'), [ptr, i32(code, 4)], types.Ptr())
  ptr = call(code, types.tag('common.wasm.memory.addr'), [ptr], types.int32())
  const idx = getIntValue(I) === undefined
    ? call(code, types.tag('common.-'), [call(code, types.tag('common.Int32'), [i], types.int32()), i32(code, 1)], types.int32())
    : i32(code, some(getIntValue(I)) - 1)
  const size = sizeof(T)
  if (size === 0) return ptr
  const off = size === 1 ? idx : call(code, types.tag('common.*'), [idx, i32(code, size)], types.int32())
  return call(code, types.tag('common.+'), [ptr, off], types.int32())
}

inlinePrimitives.set(alloc_method.id, (code, st) => {
  const T = rvtype(asType(code.type(st.expr.body[0])))
  const n = st.expr.body[1]
  const count = getIntValue(asType(code.type(n))) === undefined
    ? call(code, types.tag('common.Int32'), [n], types.int32())
    : i32(code, some(getIntValue(asType(code.type(n)))))
  const bytes = sizeof(T) === 1 ? count : call(code, types.tag('common.*'), [count, i32(code, sizeof(T))], types.int32())
  const ptr = call(code, types.tag('common.wasm.malloc.malloc!'), [call(code, types.tag('common.+'), [bytes, i32(code, 4)], types.int32())], types.Ptr())
  store(code, types.int32(), ptr, count)
  return ptr
})

inlinePrimitives.set(load_method.id, (code, st) => {
  const [t, ptr, i] = st.expr.body
  const T = rvtype(asType(code.type(t)))
  return load(code, T, ptrOffset(code, ptr, i, asType(code.type(i)), T))
})

inlinePrimitives.set(store_method.id, (code, st) => {
  const [t, ptr, i, x] = st.expr.body
  const T = rvtype(asType(code.type(t)))
  const X = asType(code.type(x))
  store(code, T, ptrOffset(code, ptr, i, asType(code.type(i)), T), cast(code, X, T, x))
  return types.nil
})

inlinePrimitives.set(length_method.id, (code, st) => {
  const ptr = st.expr.body[0]
  const len = load(code, types.int32(), ptr)
  return call(code, types.tag('common.Int64'), [len], types.int64())
})

function counter(code: Fragment<MIR>, st: InvokeSt, global: string): Val<MIR> {
  let n = st.expr.body[0]
  let T = asType(code.type(n))
  if (types.isValue(T)) n = some(constValue(types.part(T, 1)))
  const current = code.push(code.stmt(xwasm(GetGlobal(global)), { type: types.int32() }))
  const next = code.push(code.stmt(xwasm('i32.add', current, n), { type: types.int32() }))
  code.push(code.stmt(xwasm(SetGlobal(global), next), { type: types.nil }))
  return next
}

inlinePrimitives.set(allocs_method.id, (code, st) => counter(code, st, 'allocs'))
inlinePrimitives.set(frees_method.id, (code, st) => counter(code, st, 'frees'))

// Core module

function core() {
  const mod = new Module(tag('common.core'))
  for (const meth of primitives())
    mod.methods.method(meth.key, meth.sig, some(sources.get(meth.id)))
  return mod
}
