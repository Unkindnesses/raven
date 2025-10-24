import isEqual from 'lodash/isEqual'
import { hash, HashSet } from '../utils/map'
import { Fixpoint, Accessor } from '../utils/fixpoint'
import { Symbol } from './ast'
import { options } from '../utils/options'

export {
  Tag, Bits, Type,
  tag, asTag, asType, bits, pack, packcat, vpack, onion, float32, float64, int32, int64, bool, recursive, recurrence,
  repr, issubset, isdisjoint, union, unroll, recur, finite, tagOf, part, parts,
  nil, isValue, isAtom, nparts, allparts, abstract, partial_eltype, Ref, String, Ptr, list, asBits, simplify, Any
}

// Tags

class Tag {
  readonly kind = 'tag' as const
  readonly parts: readonly string[]
  readonly path: string
  constructor(...parts: (Tag | string | Symbol)[]) {
    const flatten = (p: Tag | string | Symbol): readonly string[] =>
      p instanceof Tag ? p.parts : p.toString().split('.').filter(Boolean)
    this.parts = parts.flatMap(flatten)
    this.path = this.parts.join('.')
  }
  get [hash](): string { return this.path }
  toString(): string { return 'tag\"' + this.path + '\"' }
}

function tag(s: string) { return new Tag(s) }

function asTag(x: unknown): Tag {
  if (!(x instanceof Tag)) throw new Error(`Expected Tag`)
  return x
}

// Bits

class Bits {
  readonly size: number
  readonly value: bigint
  constructor(size: number, value: bigint | number | boolean = 0) {
    this.size = size
    const v = typeof value === 'boolean' ? BigInt(value ? 1 : 0) : BigInt(value)
    this.value = BigInt.asUintN(size, v)
  }
  bitstring(): string {
    return this.value.toString(2).padStart(this.size, '0')
  }
  toString() { return 'bits\"' + this.bitstring() + '\"' }
  signed(): bigint { return BigInt.asIntN(this.size, this.value) }
}

// Types

type Type =
  | Tag
  | { kind: 'bits'; size: number; value?: bigint }
  | { kind: 'float32'; value?: number }
  | { kind: 'float64'; value?: number }
  | { kind: 'ref' }
  | { kind: 'pack'; parts: Type[] }
  | { kind: 'vpack'; tag: Type; parts: Type }
  | { kind: 'union'; options: Type[] }
  | { kind: 'recurrence' }
  | { kind: 'recursive'; inner: Type }
  | { kind: 'any' }

function asType(x: unknown): Type
function asType<K extends Type['kind']>(x: unknown, kind: K): Type & { kind: K }
function asType(x: unknown, kind?: Type['kind']): Type {
  if (typeof x === 'object' && x !== null && 'kind' in x) {
    const validKinds: Type['kind'][] = ['tag', 'bits', 'float32', 'float64', 'ref', 'pack', 'vpack', 'union', 'recurrence', 'recursive', 'any']
    const actual = (x as any).kind
    if (!validKinds.includes(actual)) throw new Error(`Expected Type, got invalid kind ${'' + actual}`)
    if (kind !== undefined && actual !== kind) throw new Error(`Expected ${kind} type, got ${actual}`)
    return x as Type
  }
  throw new Error(`Expected Type, got ${typeof x}`)
}

function repr(x: Type): string {
  switch (x.kind) {
    case 'tag':
      return x.toString()
    case 'bits': {
      const { size, value } = x
      if (value === undefined) return `bits ${size}`
      return `bits"${new Bits(size, value).bitstring()}"`
    }
    case 'float32':
      return x.value === undefined ? 'Float32' : `Float32(${x.value})`
    case 'float64':
      return x.value === undefined ? 'Float64' : `Float64(${x.value})`
    case 'ref':
      return 'ref'
    case 'pack': {
      if (x.parts.length === 2 &&
        x.parts[0].kind === 'tag' && x.parts[0].path === 'common.Int' &&
        x.parts[1].kind === 'bits') {
        const bits = x.parts[1]
        if (bits.value === undefined) return `int ${bits.size}`
        const value = new Bits(bits.size, bits.value).signed()
        if (bits.size === 64) return value.toString()
        return `int ${bits.size} ${value}`
      }
      if (isEqual(tagOf(x), tag('common.List')))
        return `[${parts(x).map(repr).join(', ')}]`
      return `pack(${x.parts.map(repr).join(', ')})`
    }
    case 'vpack':
      return `vpack(${repr(x.tag)}, ${repr(x.parts)} ...)`
    case 'union':
      return x.options.map(repr).join(' | ')
    case 'recurrence':
      return 'T'
    case 'recursive':
      return `(T = ${repr(x.inner)})`
    case 'any':
      return 'Any'
    default: let _: never = x
  }
  throw new Error('unreachable')
}

type TypeLike = Type | Tag | Bits | bigint | number | boolean

function bits(size: number, value?: bigint | number | boolean): Type {
  return { kind: 'bits', size, value: value === undefined ? undefined : new Bits(size, value).value }
}

function float32(value?: number): Type {
  return { kind: 'float32', value }
}

function float64(value?: number): Type {
  return { kind: 'float64', value }
}

function floatToBits(x: Type & { kind: 'float32' | 'float64' }): Type {
  const size = x.kind === 'float32' ? 32 : 64
  if (x.value === undefined) return bits(size)
  if (size === 32) {
    const view = new DataView(new ArrayBuffer(4))
    view.setFloat32(0, x.value, true)
    return bits(32, BigInt(view.getUint32(0, true)))
  } else {
    const view = new DataView(new ArrayBuffer(8))
    view.setFloat64(0, x.value, true)
    const lo = BigInt(view.getUint32(0, true))
    const hi = BigInt(view.getUint32(4, true))
    return bits(64, lo | (hi << 32n))
  }
}

function bitsToFloat(x: Type & { kind: 'bits' }): Type {
  if (x.size == 32) {
    if (x.value === undefined) return float32()
    const view = new DataView(new ArrayBuffer(4))
    view.setUint32(0, Number(x.value & 0xffffffffn), true)
    return float32(view.getFloat32(0, true))
  } else if (x.size == 64) {
    if (x.value === undefined) return float64()
    const view = new DataView(new ArrayBuffer(8))
    const value = x.value
    view.setUint32(0, Number(value & 0xffffffffn), true)
    view.setUint32(4, Number((value >> 32n) & 0xffffffffn), true)
    return float64(view.getFloat64(0, true))
  } else throw new Error(`Cannot convert bits ${x.size} to float`)
}

function Type(x: TypeLike): Type {
  if (x instanceof Tag) return x
  if (x instanceof Bits) return { kind: 'bits', size: x.size, value: x.value }
  if (typeof x === 'bigint') return int64(x)
  if (typeof x === 'number') return float64(x)
  if (typeof x === 'boolean') return bool(x)
  return x
}

function pack(...xs: TypeLike[]): Type {
  const parts = xs.map(Type)
  if (parts.length === 2) {
    const [t, data] = parts
    if (isEqual(t, tag('common.core.Float32')) && data.kind === 'bits' && data.size === 32)
      return bitsToFloat(data)
    if (isEqual(t, tag('common.core.Float64')) && data.kind === 'bits' && data.size === 64)
      return bitsToFloat(data)
  }
  return { kind: 'pack', parts }
}

function vpack(t: TypeLike, x: TypeLike): Type {
  return { kind: 'vpack', tag: Type(t), parts: Type(x) }
}

const recurrence: Type = { kind: 'recurrence' }

const Ref: Type = { kind: 'ref' }

const Any: Type = { kind: 'any' }

function recursive(x: Type): Type {
  return { kind: 'recursive', inner: x }
}

function onion(...xs: TypeLike[]): Type {
  if (!xs.length) throw new Error('onion() called with no arguments')
  if (xs.length === 1) return Type(xs[0])
  const options = xs.map(Type).sort((a, b) => sortkey(a).localeCompare(sortkey(b)))
  return { kind: 'union', options }
}

const nil = pack(tag('common.Nil'))

function bool(value?: boolean): Type {
  return pack(tag('common.Bool'), bits(1, value))
}

function int32(value?: bigint | number): Type {
  return pack(tag('common.Int'), bits(32, value))
}

function int64(value?: bigint | number): Type {
  return pack(tag('common.Int'), bits(64, value))
}

function Ptr(): Type {
  return pack(tag('common.Ptr'), int32())
}

function JSObject(): Type {
  if (options().gc)
    return pack(tag('common.JSObject'), Ref)
  return options().jsalloc ?
    pack(tag('common.JSObject'), pack(tag('common.Ref'), Ptr())) :
    pack(tag('common.JSObject'), int32())
}

function String(): Type {
  return pack(tag('common.String'), JSObject())
}

function list(...args: TypeLike[]): Type {
  return pack(tag('common.List'), ...args)
}

function asBits(x: Type): Bits {
  if (x.kind !== 'bits' || x.value === undefined) throw new Error('Expected bits value')
  return new Bits(x.size, x.value)
}

function isAtom(x: Type) {
  return x.kind === 'tag' ||
    x.kind === 'bits' ||
    x.kind === 'float32' ||
    x.kind === 'float64' ||
    x.kind === 'ref'
}

function isValue(x: Type): boolean {
  switch (x.kind) {
    case 'tag': return true
    case 'bits': return x.value !== undefined
    case 'float32':
    case 'float64': return x.value !== undefined
    case 'pack': return x.parts.every(isValue)
    default: return false
  }
}

function abstract(x: Type): Type {
  if (x.kind === 'bits') return bits(x.size)
  if (x.kind === 'float32') return float32()
  if (x.kind === 'float64') return float64()
  if (x.kind === 'ref') return Ref
  throw new Error(`not an atom type: ${repr(x)}`)
}

function tagOf(x: Type): Type {
  if (x.kind === 'tag') return Type(tag('common.core.Tag'))
  if (x.kind === 'bits') return Type(tag('common.core.Bits'))
  if (x.kind === 'float32') return Type(tag('common.core.Float32'))
  if (x.kind === 'float64') return Type(tag('common.core.Float64'))
  if (x.kind === 'ref') return Type(tag('common.core.Ref'))
  if (x.kind === 'pack') return x.parts[0]
  if (x.kind === 'vpack') return x.tag
  if (x.kind === 'recursive') return tagOf(x.inner)
  if (x.kind === 'any') return x
  throw new Error(`No fixed tag for ${repr(x)} type`)
}

function astag(x: Type): Tag {
  if (x.kind !== 'tag') throw new Error(`not a tag type: ${repr(x)}`)
  return x
}

function allparts(x: Type): Type[] {
  if (x.kind === 'tag' || x.kind === 'bits') return [tagOf(x), x]
  if (x.kind === 'float32' || x.kind === 'float64') return [tagOf(x), floatToBits(x)]
  if (x.kind === 'ref') return [tagOf(x), x]
  if (x.kind === 'pack') return x.parts
  throw new Error(`No fixed parts for ${repr(x)} type`)
}

function parts(x: Type): Type[] { return allparts(x).slice(1) }

function part(x: Type, i: number): Type {
  return allparts(x)[i]
}

function nparts(x: Type): number {
  return allparts(x).length - 1
}

function packcat(...x: Type[]): Type
function packcat(x: Type, y?: Type, ...z: Type[]): Type {
  if (y === undefined) return x
  const [ux, uy] = [unroll(x), unroll(y)]
  const result = (ux.kind === 'vpack' || uy.kind === 'vpack') ?
    vpack(tagOf(ux), maybe_union(partial_eltype(ux), partial_eltype(uy))!) :
    pack(tagOf(ux), ...parts(ux), ...parts(uy))
  return packcat(recur(result), ...z)
}

function sortkey(x: Type): string {
  if (x.kind === 'tag') return [astag(tagOf(x)).path, x.path].join(':')
  if (x.kind === 'bits') return [astag(tagOf(x)).path, x.size.toString()].join(':')
  if (x.kind === 'union') return sortkey(x.options[0])
  if (x.kind === 'recursive') return sortkey(x.inner)
  if (x.kind === 'any') return 'any:'
  let key = tagOf(x)
  return key.kind === 'tag' ? key.path : ""
}

function typekey(x: Type, r = new Set<string>()): Set<string> {
  if (x.kind === 'union') {
    x.options.forEach(t => typekey(t, r))
  } else if (x.kind === 'recursive') {
    typekey(x.inner, r)
  } else {
    r.add(sortkey(x))
  }
  return r
}

function setDisjoint<T>(setA: Set<T>, setB: Set<T>): boolean {
  const [smaller, larger] = setA.size < setB.size ? [setA, setB] : [setB, setA]
  for (const part of smaller) {
    if (larger.has(part)) {
      return false
    }
  }
  return true
}

function overlapping(x: Type, y: Type): boolean {
  return !setDisjoint(typekey(x), typekey(y))
}

// Subset

function postwalk<T extends Type | null>(f: (x: Type) => T, x: Type): T {
  const inner = (x: Type): Type | null => postwalk(f, x)
  let y: Type | null
  if (isAtom(x) || x.kind === 'recurrence' || x.kind === 'recursive' || x.kind === 'any') {
    y = x
  } else if (x.kind === 'pack') {
    const parts = x.parts.map(inner)
    if (parts.includes(null)) return null as T
    y = pack(...parts as Type[])
  } else if (x.kind === 'vpack') {
    const tag = inner(x.tag)
    if (tag === null) return null as T
    const parts = inner(x.parts)
    y = parts === null ? pack(tag) : vpack(tag, parts)
  } else if (x.kind === 'union') {
    const ts = x.options.flatMap(t => {
      const s = inner(t)
      if (s === null) return []
      return s.kind === 'union' ? s.options : [s]
    })
    y = onion(...ts)
  } else {
    let _: never = x
    throw new Error('unreachable')
  }
  return f(y)
}

function _unroll(x: Type, s: Type | null = x): Type {
  if (x.kind === 'recursive') {
    return postwalk((x: Type) => x.kind === 'recurrence' ? s : x, x.inner) as Type
  } else if (x.kind === 'union') {
    const ts = x.options.flatMap(t => {
      const s = _unroll(t)
      return s.kind === 'union' ? s.options : [s]
    })
    return onion(...ts)
  } else {
    return x
  }
}

function _issubset(self: Accessor<[Type, Type], boolean>, x: Type, y: Type): boolean {
  const iss = (a: Type, b: Type) => self.get([a, b])
  if (x.kind === 'recursive' || y.kind === 'recursive')
    return iss(_unroll(x), _unroll(y))
  if (x.kind === 'union') return x.options.every(t => iss(t, y))
  if (y.kind === 'union') return y.options.some(t => iss(x, t))
  if (isAtom(x) && isAtom(y))
    return isEqual(x, y) ||
      x.kind === y.kind && !isValue(y) &&
      (x.kind !== 'bits' || x.size == (y as any).size)
  if (x.kind === 'pack' && y.kind === 'pack')
    return nparts(x) === nparts(y) &&
      x.parts.every((t, i) => iss(t, y.parts[i]))
  if (x.kind === 'pack' && y.kind === 'vpack')
    return iss(tagOf(x), tagOf(y)) &&
      x.parts.slice(1).every(t => iss(t, y.parts))
  if (x.kind === 'vpack' && y.kind === 'vpack')
    return iss(tagOf(x), tagOf(y)) && iss(x.parts, y.parts)
  if (isEqual(x, Any) && isEqual(y, Any))
    return true
  return false
}

function subsetter() {
  const fp = new Fixpoint<[Type, Type], boolean>(
    (self, [a, b]) => _issubset(self, a, b),
    () => true
  )
  return (a: Type, b: Type) => fp.get([a, b])
}

function issubset(x: Type, y: Type): boolean {
  return subsetter()(x as Type, y as Type)
}

// Disjoint

function disjuncts(x: Type): Type[] { return x.kind === 'union' ? x.options : [x] }

function _isdisjoint(self: Accessor<[Type, Type], boolean>, x: Type, y: Type): boolean {
  const isd = (a: Type, b: Type) => self.get([a, b])
  if (x.kind === 'recursive' || y.kind === 'recursive')
    return isd(_unroll(x), _unroll(y))
  if (x.kind === 'union' || y.kind === 'union')
    return disjuncts(x).every(x => disjuncts(y).every(y => isd(x, y)))
  if (isAtom(x) && isAtom(y))
    return x.kind !== y.kind ||
      isValue(x) && isValue(y) && !isEqual(x, y) ||
      x.kind === 'bits' && y.kind === 'bits' && x.size !== y.size
  if (x.kind === 'pack' && y.kind === 'pack')
    return nparts(x) !== nparts(y) || x.parts.some((t, i) => isd(t, y.parts[i]))
  if (x.kind === 'pack' && y.kind === 'vpack')
    return isd(tagOf(x), tagOf(y)) || x.parts.slice(1).some(t => isd(t, y.parts))
  if (x.kind === 'vpack' && y.kind === 'pack')
    return isd(y, x)
  if (x.kind === 'vpack' && y.kind === 'vpack')
    return isd(tagOf(x), tagOf(y))
  if (isEqual(x, Any) && isEqual(y, Any))
    return false
  return true
}

function disjointer() {
  const fp = new Fixpoint<[Type, Type], boolean>(
    (self, [x, y]) => _isdisjoint(self, x, y),
    () => false
  )
  return (x: Type, y: Type) => fp.get([x, y])
}

function isdisjoint(x: Type, y: Type): boolean {
  return disjointer()(x, y)
}

// Distinct

function _isdistinct(
  self: Accessor<[Type, Type], boolean>, x: Type, y: Type,
  isdisjoint: (a: Type, b: Type) => boolean
): boolean {
  const isdist = (a: Type, b: Type) => self.get([a, b])
  if (x.kind === 'recursive' || y.kind === 'recursive')
    return isdist(_unroll(x), _unroll(y))
  if (x.kind === 'union' || y.kind === 'union') {
    const xs = disjuncts(x), ys = disjuncts(y)
    if (!xs.every(a => ys.every(b => isdist(a, b)))) return false
    let overlaps = 0
    for (const a of xs)
      for (const b of ys)
        if (!isdisjoint(a, b) && ++overlaps >= 2) return false
    return true
  }
  if (x.kind === 'pack' && y.kind === 'pack')
    return nparts(x) < 1 || isdisjoint(x, y) ||
      x.parts.every((t, i) => isdist(t, y.parts[i]))
  if (x.kind === 'pack' && y.kind === 'vpack')
    return nparts(x) < 1 || isdisjoint(x, y) ||
      (isdist(tagOf(x), tagOf(y)) &&
        x.parts.every(t => isdist(t, y.parts)))
  if (x.kind === 'vpack' && y.kind === 'pack')
    return isdist(y, x)
  if (x.kind === 'vpack' && y.kind === 'vpack')
    return isdisjoint(tagOf(x), tagOf(y)) || isdisjoint(x.parts, y.parts)
  return true
}

function distincter() {
  const isd = disjointer()
  const fp = new Fixpoint<[Type, Type], boolean>(
    (self, [a, b]) => _isdistinct(self, a, b, isd),
    () => false
  )
  return (a: Type, b: Type) => fp.get([a, b])
}

function isdistinct(x: Type, y: Type): boolean {
  return distincter()(x as Type, y as Type)
}

// Union

function finite(T: Type, depth = 1): Type {
  if (T.kind === 'union') {
    return onion(...T.options.flatMap(x => disjuncts(finite(x, depth))))
  } else if (T.kind === 'recursive') {
    let term = _unroll(T, null)
    if (isdistinct(T, term)) term = _unroll(T, term)
    for (let i = 0; i < depth; i++) term = _unroll(T, term as Type)
    return term
  } else {
    return T
  }
}

function partial_eltype(x: Type & { kind: 'vpack' }, u?: (x: Type, y: Type) => Type): Type
function partial_eltype(x: Type, u?: (x: Type, y: Type) => Type): Type | null

function partial_eltype(x: Type, u: (x: Type, y: Type) => Type = union): Type | null {
  if (x.kind === 'pack') return x.parts.length > 1 ? x.parts.slice(1).reduce(u) : null
  if (x.kind === 'vpack') return x.parts
  throw new Error(`No eltype for ${x.kind} type`)
}

function splitby<T>(f: (x: T) => boolean, xs: T[]): [T[], T[]] {
  const ts: T[] = []
  const fs: T[] = []
  for (const x of xs) (f(x) ? ts : fs).push(x)
  return [ts, fs]
}

function _union(x: Type, y: Type): Type {
  [x, y] = [x, y].map(x => finite(x))
  if (isEqual(x, Any) && isEqual(y, Any)) return x
  if (x.kind === 'union' || y.kind === 'union') {
    let ys: Type[] = []
    for (let term of [...disjuncts(y), ...disjuncts(x)]) {
      while (true) {
        let xs;
        [xs, ys] = splitby(y => overlapping(term, y), ys)
        if (!xs.length) break
        let next = xs.reduce((term, y) => _union(term, y), term)
        term = next
      }
      ys.push(...disjuncts(term))
    }
    return onion(...ys)
  }
  if (sortkey(x) !== sortkey(y)) return onion(x, y)
  if (isAtom(x) && isAtom(y)) {
    return isEqual(x, y) ? x : abstract(x)
  }
  if (x.kind === 'vpack' || y.kind === 'vpack' || nparts(x) !== nparts(y)) {
    const t = _union(tagOf(x), tagOf(y))
    const parts = maybe_union(partial_eltype(x, _union), partial_eltype(y, _union))!
    return vpack(t, parts)
  }
  if (x.kind === 'pack' && y.kind === 'pack' && nparts(x) === nparts(y)) {
    let parts = x.parts.map((part, i) => _union(part, y.parts[i]))
    return pack(...parts as Type[])
  }
  throw new Error(`unreachable`)
}

function maybe_union<T = null>(x: Type, y: Type | T, sentinel?: T): Type
function maybe_union<T = null>(x: Type | T, y: Type, sentinel?: T): Type
function maybe_union<T = null>(x: Type | T, y: Type | T, sentinel?: T): Type | T
function maybe_union<T = null>(x: Type | T, y: Type | T, sentinel: T = null as T): Type | T {
  if (x === sentinel) return y
  if (y === sentinel) return x
  return _union(x as Type, y as Type)
}

// Reroll

function reconstruct(x: Type): [Type[], (xs: Type[]) => Type] {
  const parts =
    x.kind === 'pack' ? x.parts :
      x.kind === 'vpack' ? [x.tag, x.parts] :
        x.kind === 'union' ? x.options :
          []
  const re = (xs: Type[]): Type =>
    x.kind === 'pack' ? pack(...xs) :
      x.kind === 'vpack' ? vpack(xs[0], xs[1]) :
        x.kind === 'union' ? onion(...xs) :
          x
  return [parts, re]
}

function occursin(x: Type, y: Type): boolean {
  return isEqual(x, y) ||
    (y.kind !== 'recursive' && y.kind !== 'recurrence') &&
    reconstruct(y)[0].some(y => occursin(x, y))
}

function isRecursive(x: Type): boolean {
  return x.kind === 'recursive' || x.kind === 'union' && x.options.some(isRecursive)
}

function rfinite(x: Type): Type {
  return postwalk((x: Type) => x.kind === 'recursive' ? finite(x) : x, x)
}

function isrecur(x: Type, T: Type): boolean {
  return !isdistinct(x, T) && issubset(rfinite(x), T)
}

function reroll_inner(T: Type, x: Type, seen: HashSet<Type>, self = _reroll): [Type, Set<string>] {
  if (x.kind === 'recursive') {
    if (seen.has(x)) return [x, new Set()]
    const [y, ks] = reroll_outer(T, _unroll(x), new HashSet([...seen, x]))
    if (!ks.size) return [x, new Set()]
    if (occursin(x, y)) return [x, new Set()]
    return [y, ks]
  } else {
    const [xs, re] = reconstruct(x)
    const ys = xs.map(t => self(T, t, seen))
    const ch = x.kind === 'union' ? ys.flatMap(([y]) => disjuncts(y)) : ys.map(([y]) => y)
    return [re(ch), new Set(ys.flatMap(([, k]) => Array.from(k)))]
  }
}

function reroll_outer(T: Type, x: Type, seen: HashSet<Type>): [Type, Set<string>] {
  return x.kind === 'union' ?
    reroll_inner(T, x, seen, reroll_inner) :
    reroll_inner(T, x, seen)
}

function _reroll(T: Type, x: Type, seen: HashSet<Type>): [Type, Set<string>] {
  return isrecur(x, T) ?
    [recurrence, typekey(x)] :
    reroll_outer(T, x, seen)
}

function reroll(T: Type): Type {
  if (isRecursive(T)) return T
  const xs = disjuncts(T)
  let ys = xs.map(x => {
    const [y, keys] = reroll_inner(T, x, new HashSet())
    return [y, new Set([...keys, ...typekey(x)])] as const
  })
  let results: [Type, Set<string>][] = []
  // Group by typekey in common
  for (const [x, ks] of ys) {
    const [as, bs] = splitby(([_, k]) => !setDisjoint(ks, k), results)
    const combinedType = onion(x, ...as.flatMap(([a]) => disjuncts(a)))
    const keys = as.reduce((acc, [_, k]) => new Set([...acc, ...k]), new Set([...ks]))
    results = [...bs, [combinedType, keys]]
  }
  return onion(...results.map(([x]) => occursin(recurrence, x) ? recursive(x) : x))
}

function unroll_inner(S: Type, T: Type): Type {
  if (T.kind === 'recursive') {
    return T
  } else if (T.kind === 'recurrence') {
    return S
  } else if (T.kind === 'union') {
    return onion(...T.options.map(d => unroll_inner(S, d)))
  } else {
    const [xs, re] = reconstruct(T)
    return re(xs.map(t => reroll(unroll_inner(S, t))))
  }
}

function unroll(T: Type): Type {
  if (T.kind === 'recursive') {
    return unroll_inner(T, T.inner)
  } else if (T.kind === 'union') {
    return onion(...T.options.flatMap(S => disjuncts(unroll(S))))
  } else {
    return T
  }
}

// Lift

function lift_inner(T: Type, x: Type, seen: HashSet<Type>, self = _lift): [Type | null, boolean, boolean] {
  if (x.kind === 'recursive') {
    if (seen.has(x)) return [null, false, true]
    const [inner, s, r] = lift_outer(T, _unroll(x), new HashSet([...seen, x]))
    return s && r ? [x, true, false] : [inner, s, false]
  } else {
    const [xs] = reconstruct(x)
    const ys = xs.map(t => self(T, t, seen))
    return [
      ys.reduce((acc, [y]) => maybe_union(acc, y), null as Type | null),
      ys.some(([, s]) => s),
      ys.some(([, , r]) => r)
    ]
  }
}

function lift_outer(T: Type, x: Type, seen: HashSet<Type>): [Type | null, boolean, boolean] {
  return x.kind === 'union'
    ? lift_inner(T, x, seen, lift_inner)
    : lift_inner(T, x, seen)
}

function _lift(T: Type, x: Type, seen: HashSet<Type>): [Type | null, boolean, boolean] {
  if (!isdistinct(x, T)) {
    return [issubset(x, T) ? null : x, true, false]
  }
  return lift_outer(T, x, seen)
}

function lift(T: Type): Type | null { return lift_outer(T, T, new HashSet())[0] }

// Recursive

function recur_inner(T: Type, self: (t: Type) => Type): Type {
  if (T.kind === 'union') {
    return onion(...T.options.map(x => recur_inner(x, self)))
  } else {
    const [xs, re] = reconstruct(T)
    return re(xs.map(x => self(x)))
  }
}

function _recur(T: Type, self = (x: Type) => x): Type {
  if (T.kind === 'vpack' || T.kind === 'union' || T.kind === 'recursive') {
    const R = reroll(recur_inner(maybe_union(_unroll(T), lift(T)), self))
    return issubset(R, T) ? R : self(R)
  } else {
    return recur_inner(_unroll(T), self)
  }
}

function recurser() {
  const check = (old: Type, nue: Type) => {
    if (!issubset(old, nue)) throw new Error("recursion subset")
  }
  const fp = new Fixpoint<Type, Type>(
    (self, T) => _recur(T, (T) => self.get(T)),
    x => x, check
  )
  return (T: Type) => fp.get(T)
}

function recur(T: Type): Type {
  return recurser()(T)
}

// Strip ⊤ types
function simplify(T: Type): Type {
  if (T.kind === 'union') {
    const xs = T.options.map(simplify)
    if (xs.some(x => isEqual(x, Any))) return Any
    return onion(...xs)
  }
  if (T.kind === 'recursive') {
    const r = simplify(T.inner)
    if (isEqual(r, Any)) return Any
    return occursin(recurrence, r) ? recursive(r) : r
  }
  const [children, f] = reconstruct(T)
  return f(children.map(simplify))
}

function union(x: Type, y: Type): Type {
  return recur(_union(x, y))
}
