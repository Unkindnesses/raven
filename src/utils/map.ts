export {
  HashMap, HashSet, HashFn, hash, stable, some, asNumber, asBool, asBigInt, asString,
  asArray, first, only, setdiff, filter
}

function some<T>(value: T | null | undefined, message?: string): T {
  if (value === null || value === undefined)
    throw new Error(message || 'Assertion failed: value is null or undefined')
  return value
}

function first<T>(xs: Iterable<T>): T {
  for (const v of xs) return v
  throw new Error('first() expected at least one item')
}

function only<T>(xs: Iterable<T>): T {
  const it = xs[Symbol.iterator]()
  const a = it.next()
  if (a.done) throw new Error('only() expected exactly one item')
  const b = it.next()
  if (!b.done) throw new Error('only() expected at most one item')
  return a.value
}

function asNumber(value: unknown): number {
  if (typeof value === 'number') return value
  throw new Error(`Expected number, got ${typeof value}`)
}

function asBool(value: unknown): boolean {
  if (typeof value === 'boolean') return value
  throw new Error(`Expected boolean, got ${typeof value}`)
}

function asBigInt(value: unknown): bigint {
  if (typeof value === 'bigint') return value
  throw new Error(`Expected bigint, got ${typeof value}`)
}

function asString(value: unknown): string {
  if (typeof value === 'string') return value
  throw new Error(`Expected string, got ${typeof value}`)
}

type ArrayLikeOf<T> = Extract<T, readonly unknown[]>

function asArray<T>(x: T): ArrayLikeOf<T> {
  if (Array.isArray(x)) return x as ArrayLikeOf<T>
  throw new Error(`Expected array, got ${typeof x}`)
}

type HashFn<K> = (k: K) => string

const hash = Symbol('hash')

const stable = (x: any): string => {
  if (!x || typeof x !== 'object')
    return String(x)
  if (hash in x) return x[hash]
  if (Array.isArray(x))
    return '[' + x.map(stable).join(',') + ']'
  if (x instanceof Map)
    return 'Map{' +
      Array.from(x.entries())
        .map(([k, v]) => stable(k) + ':' + stable(v))
        .sort()
        .join(',') +
      '}'
  if (x instanceof Set)
    return 'Set{' +
      Array.from(x)
        .map(v => stable(v))
        .sort()
        .join(',') +
      '}'
  return '{' + Object.keys(x).sort().map(k => k + ':' + stable(x[k])).join(',') + '}'
}

const defaultHash: HashFn<any> = k => stable(k)

class HashMap<K, V> implements Iterable<[K, V]> {
  private b = new Map<string, [K, V]>()
  constructor(
    iterable: Iterable<[K, V]> = [],
    private hash: HashFn<K> = defaultHash,
  ) { for (const [k, v] of iterable) this.set(k, v) }
  get size() { return this.b.size }
  set(k: K, v: V) {
    this.b.set(this.hash(k), [k, v])
    return this
  }
  get(k: K) { return this.b.get(this.hash(k))?.[1] }
  has(k: K) { return this.b.has(this.hash(k)) }
  delete(k: K) { return this.b.delete(this.hash(k)) }
  clear() { this.b.clear() }
  *[Symbol.iterator]() { for (const p of this.b.values()) yield p }
  *keys(): IterableIterator<K> { for (const p of this.b.values()) yield p[0] }
  *values(): IterableIterator<V> { for (const p of this.b.values()) yield p[1] }
  entries(): IterableIterator<[K, V]> { return this[Symbol.iterator]() }
}

class HashSet<T> implements Iterable<T> {
  private map = new Map<string, T>()
  constructor(
    iterable: Iterable<T> = [],
    private hash: HashFn<T> = defaultHash,
  ) { for (const v of iterable) this.add(v) }
  get size() { return this.map.size }
  add(v: T): this {
    this.map.set(this.hash(v), v)
    return this
  }
  has(v: T): boolean { return this.map.has(this.hash(v)) }
  delete(v: T): boolean { return this.map.delete(this.hash(v)) }
  clear(): void { this.map.clear() }
  *values(): IterableIterator<T> { yield* this.map.values() }
  keys() { return this.values() }
  [Symbol.iterator]() { return this.values() }
  forEach(cb: (value: T, value2: T, set: HashSet<T>) => void, thisArg?: any): void {
    for (const v of this.values()) cb.call(thisArg, v, v, this)
  }
}

function setdiff<T>(a: Set<T>, b: Set<T>): Set<T> {
  const out = new Set<T>()
  for (const x of a) if (!b.has(x)) out.add(x)
  return out
}

function filter<T>(s: Set<T>, p: (x: T) => boolean): Set<T> {
  const out = new Set<T>()
  for (const x of s) if (p(x)) out.add(x)
  return out
}
