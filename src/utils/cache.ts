import { isEqual } from '../utils/isEqual.js'
import { WorkQueue, Accessor } from "./fixpoint.js"
import { HashMap } from "./map.js"
import { now } from "../utils/bench.js"

export {
  nft, trackdeps, track, withtime,
  Caching, Ref, Map, Cache, EagerCache, DualCache, CycleCache,
  fingerprint, reuse, reset, pipe, time, size
}

let nft_id = 0n
function nft() { return ++nft_id }

// Timer

let timestack: bigint[] = []

function withtime<T>(f: () => T): [T, bigint]
function withtime<T>(f: () => Promise<T>): Promise<[T, bigint]>
function withtime<T>(f: () => T | Promise<T>): [T, bigint] | Promise<[T, bigint]> {
  const start = now()
  timestack.push(0n)
  const finish = (result: T): [T, bigint] => {
    const offset = timestack.pop()!
    const span = now() - start
    if (timestack.length > 0) timestack[timestack.length - 1] += span
    return [result, span - offset]
  }
  try {
    const result = f()
    if (result instanceof Promise) {
      return result.then(finish, err => {
        timestack.pop()
        throw err
      })
    }
    return finish(result)
  } catch (err) {
    timestack.pop()
    throw err
  }
}

// Track dependencies through the call stack

let cachedeps: Set<bigint>[] = []

function trackdeps<T>(f: () => T): [T, Set<bigint>] {
  const stack = cachedeps
  const deps = new Set<bigint>()
  stack.push(deps)
  try {
    return [f(), deps]
  } finally {
    stack.pop()
  }
}

function track(t: bigint) {
  const stack = cachedeps
  if (stack.length) stack[stack.length - 1].add(t)
}

// Cache interface

interface Caching {
  subcaches?: Iterable<Caching>
  fingerprint?(): Set<bigint>
  reuse?(ch: this): this
  reset?(deps: Set<bigint>): void
  time?: bigint
  size?: number
}

// `reuse` should really take a fingerprint. The intent is to copy only valid
// results from `b` into `a` (which may already contain results). But for now
// we assume `a` is empty and `b` is complete, which lets us copy caches.
function reuse<T extends Caching>(a: T, b: T): T {
  if (a.reuse) return a.reuse(b)
  const xs = Array.from(a.subcaches ?? [])
  const ys = Array.from(b.subcaches ?? [])
  if (xs.length !== ys.length) throw new Error('Cache reuse shape mismatch')
  for (let i = 0; i < xs.length; i++) reuse(xs[i], ys[i])
  return a
}

function fingerprint(ch: Caching): Set<bigint> {
  if (ch.fingerprint) return ch.fingerprint()
  let result = new Set<bigint>()
  for (const sub of ch.subcaches ?? [])
    for (const id of fingerprint(sub))
      result.add(id)
  return result
}

function reset(ch: Caching, deps: Set<bigint> | Caching[] = new Set()) {
  if (Array.isArray(deps)) deps = fingerprint({ subcaches: deps })
  if (ch.reset) return ch.reset(deps)
  for (const sub of ch.subcaches ?? []) reset(sub, deps)
}

function time(ch: Caching): bigint {
  let t = ch.time ?? 0n
  for (const sub of ch.subcaches ?? []) t += time(sub)
  return t
}

function size(ch: Caching): number {
  let n = ch.size ?? 0
  for (const sub of ch.subcaches ?? []) n += size(sub)
  return n
}

class Pipe implements Caching {
  constructor(readonly subcaches: Caching[]) { }
  reset(deps: Set<bigint>) {
    deps = new Set(deps)
    for (const ch of this.subcaches) {
      reset(ch, deps)
      for (const id of fingerprint(ch)) deps.add(id)
    }
  }
}

function pipe(...subcaches: Caching[]) { return new Pipe(subcaches) }

// Simple Ref-like type containing a single value. Tracked by `Cache`s.

class Ref<T> implements Caching {
  private id: bigint = nft()
  constructor(private value: T) { }
  fingerprint() { return new Set([this.id]) }
  get size() { return 1 }
  get() { track(this.id); return this.value }
  set(x: T) { this.id = nft(); this.value = x; return this }
  clone(): Ref<T> { const ref = new Ref(this.value); ref.id = this.id; return ref }
}

// Just a map, with tracking enabled for `Cache`s.

class Map<K, V> implements Caching {
  private print: Set<bigint> = new Set()
  private data: HashMap<K, [bigint, V]> = new HashMap()
  private haskey: HashMap<K, bigint> = new HashMap()

  fingerprint() { return this.print }
  get size() { return this.data.size + this.haskey.size }

  keys() { return this.data.keys() }
  iscached(k: K) { return this.data.has(k) }
  id(k: K) { return this.data.get(k)![0] }

  get(k: K): V | undefined {
    if (!this.has(k)) return
    const [id, value] = this.data.get(k)!
    track(id)
    return value
  }

  has(k: K): boolean {
    let id = this.haskey.has(k) ? this.haskey.get(k)! : nft()
    this.haskey.set(k, id)
    this.print.add(id)
    track(id)
    return this.data.has(k)
  }

  set(k: K, v: V, { id = nft() }: { id?: bigint } = {}) {
    if (this.data.has(k)) {
      this.print.delete(this.data.get(k)![0])
      this.data.delete(k)
    } else if (this.haskey.has(k)) {
      this.print.delete(this.haskey.get(k)!)
      this.haskey.delete(k)
    }
    this.data.set(k, [id, v])
    this.print.add(id)
    return this
  }

  delete(k: K): boolean {
    if (this.data.has(k)) {
      this.print.delete(this.data.get(k)![0])
      this.data.delete(k)
      if (this.haskey.has(k)) {
        this.print.delete(this.haskey.get(k)!)
        this.haskey.delete(k)
      }
      return true
    } else return false
  }

  clear() {
    for (const k of this.keys()) {
      this.delete(k)
    }
  }

  clone(): Map<K, V> {
    const out = new Map<K, V>()
    out.print = new Set(this.print)
    out.data = new HashMap(this.data)
    out.haskey = new HashMap(this.haskey)
    return out
  }
}

interface CacheValue<T> {
  value: T
  id: bigint
  deps: Set<bigint>
}

class Cache<K, V> implements Caching {
  private print: Set<bigint> = new Set()
  private data: HashMap<K, CacheValue<V>> = new HashMap()
  time = 0n
  constructor(private init: (k: K) => V) { }

  fingerprint() { return this.print }
  get size() { return this.data.size }
  keys() { return this.data.keys() }

  iscached(k: K) { return this.data.has(k) }
  cached(k: K): V { return this.data.get(k)!.value }
  id(k: K): bigint { return this.data.get(k)!.id }
  deps(k: K): Set<bigint> { return this.data.get(k)!.deps }
  value(k: K): [V, Set<bigint>] { return trackdeps(() => this.init(k)) }

  delete(k: K): boolean {
    const fr = this.data.get(k)
    if (!fr) return false
    this.print.delete(fr.id)
    this.data.delete(k)
    return true
  }

  protected _set(k: K, v: V, { id, deps }: { id?: bigint, deps?: Set<bigint> } = {}): Cache<K, V> {
    id ??= nft()
    deps ??= new Set()
    this.print.add(id)
    this.data.set(k, { value: v, id, deps })
    return this
  }

  set(k: K, v: V, options: { id?: bigint, deps?: Set<bigint> } = {}): Cache<K, V> {
    if (this.iscached(k)) throw new Error(`Cannot overwrite cache value: ${k}`)
    return this._set(k, v, options)
  }

  _get(k: K): V {
    if (!this.iscached(k)) {
      const [v, deps] = this.value(k)
      this._set(k, v, { deps })
    }
    track(this.id(k))
    return this.cached(k)
  }

  get(k: K): V {
    const [v, time] = withtime(() => this._get(k))
    this.time += time
    return v
  }

  *invalid(deps: Set<bigint> = new Set()): Generator<K> {
    for (const k of this.keys()) {
      if (!Array.from(this.deps(k)).every(id => deps.has(id))) yield k
    }
  }

  reuse(ch: this) {
    this.print = new Set(ch.print)
    this.data = new HashMap(ch.data)
    return this
  }

  reset(deps: Set<bigint>) {
    for (const k of this.invalid(deps)) this.delete(k)
  }
}

// Eagerly updated version

class EagerCache<K, V> extends Cache<K, V> {
  reset(deps: Set<bigint>) {
    for (const k of Array.from(this.invalid(deps))) {
      const [, time] = withtime(() => {
        const [v, deps] = this.value(k)
        if (!isEqual(v, this.cached(k))) {
          this.delete(k)
          this._set(k, v, { deps })
        }
      })
      this.time += time
    }
  }
}

// Value -> key mapping

class DualCache<K, V> extends Cache<K, V> {
  dual: HashMap<V, K> = new HashMap()
  set(k: K, v: V): Cache<K, V> { throw new Error('Cannot set DualCache value') }
  _get(k: K): V {
    let v = super._get(k)
    this.dual.set(v, k)
    return v
  }
  reuse(ch: this) {
    super.reuse(ch)
    this.dual = new HashMap(ch.dual)
    return this
  }
  reset(deps: Set<bigint>) {
    for (const k of this.invalid(deps)) {
      const v = this.cached(k)
      this.dual.delete(v)
      this.delete(k)
    }
  }
  hasvalue(v: V): boolean { return this.dual.has(v) }
  getkey(v: V): K {
    if (!this.hasvalue(v)) throw new Error(`Value not found: ${v}`)
    return this.dual.get(v)!
  }
}

// Cycle cache

interface CycleCacheValue<T> {
  value: T
  id: bigint
  deps: Set<bigint>
  edges: Set<bigint>
}

class CycleCache<K, V> implements Caching {
  private print = new Set<bigint>()
  private keys = new globalThis.Map<bigint, K>()
  private data = new HashMap<K, CycleCacheValue<V>>()
  private queue = new WorkQueue<bigint>()
  time = 0n
  constructor(private init: (k: K) => V, private iter: (ch: Accessor<K, V>, k: K) => V) { }

  fingerprint() { return this.print }
  get size() { return this.data.size }
  key(k: K | bigint): K { return typeof k === 'bigint' ? this.keys.get(k)! : k }
  iscached(k: K) { return this.data.has(k) }
  cached(k: K): V { return this.data.get(k)!.value }
  id(k: K): bigint { return this.data.get(k)!.id }
  deps(k: K | bigint): Set<bigint> { return this.data.get(this.key(k))!.deps }
  edges(k: K | bigint): Set<bigint> { return this.data.get(this.key(k))!.edges }

  delete(k: K | bigint): boolean {
    k = this.key(k)
    if (!this.iscached(k)) return false
    let v = this.data.get(k)!
    this.print.delete(v.id)
    this.keys.delete(v.id)
    this.data.delete(k)
    for (const id of v.deps)
      if (this.keys.has(id)) this.edges(id).delete(v.id)
    for (const id of v.edges)
      if (this.keys.has(id)) this.delete(id)
    return true
  }

  set(k: K, v: V, { id, deps }: { id?: bigint, deps?: Set<bigint> } = {}): CycleCache<K, V> {
    id ??= nft()
    deps ??= new Set()
    if (this.iscached(k) && !(this.id(k) === id)) throw new Error(`Cache inconsistency`)
    this.print.add(id)
    this.keys.set(id, k)
    this.data.set(k, { value: v, id, deps, edges: new Set() })
    for (const d of deps)
      if (this.keys.has(d)) this.edges(d).add(id)
    return this
  }

  private update(k: K) {
    let v = this.data.get(k)!
    const inner = { get: (k: K) => this.get(k, false) }
    const [val, deps] = trackdeps(() => this.iter(inner, k))
    this.set(k, val, { id: v.id, deps: new Set([...v.deps, ...deps]) })
    if (isEqual(val, v.value)) return
    for (const id of this.edges(k))
      if (this.keys.has(id)) this.queue.push(id)
  }

  _get(k: K, loop = true): V {
    if (!this.iscached(k)) {
      const [[val, deps], time] = withtime(() => trackdeps(() => this.init(k)))
      this.time += time
      this.set(k, val, { deps })
      this.update(k)
    }
    if (loop) while (!this.queue.empty) this.update(this.key(this.queue.pop()))
    track(this.id(k))
    return this.cached(k)
  }

  get(k: K, loop = true): V {
    const [v, time] = withtime(() => this._get(k, loop))
    this.time += time
    return v
  }

  reuse(ch: this): this {
    this.print = new Set(ch.print)
    this.keys = new globalThis.Map(ch.keys)
    this.data = new HashMap()
    for (const [k, v] of ch.data)
      this.data.set(k, { value: v.value, id: v.id, deps: new Set(v.deps), edges: new Set(v.edges) })
    this.queue = new WorkQueue<bigint>()
    return this
  }

  reset(deps: Set<bigint>) {
    for (const k of this.keys.keys()) {
      if (!Array.from(this.deps(k)).every(id => deps.has(id) || this.print.has(id)))
        this.delete(k)
    }
  }
}
