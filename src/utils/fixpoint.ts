import isEqual from 'lodash/isEqual'

export { WorkQueue, Fixpoint, Accessor }

interface Accessor<K, V> {
  get(key: K): V
}

class WorkQueue<K> {
  private items: K[] = []
  private seen = new Set<K>()

  push(k: K) {
    if (!this.seen.has(k)) {
      this.seen.add(k)
      this.items.push(k)
    }
  }

  pop(): K {
    const k = this.items.pop()
    if (k === undefined) throw new Error('pop() from empty queue')
    this.seen.delete(k)
    return k
  }

  get empty() {
    return this.items.length === 0
  }
}

type Key = string | number

class Frame<K, V> {
  key: K
  value: V
  deps = new Set<Key>()
  edges = new Set<Key>()
  constructor(k: K, v: V) {
    this.key = k
    this.value = v
  }
}

class Fixpoint<K, V> {
  private frames = new Map<Key, Frame<K, V>>()
  private queue = new WorkQueue<Key>()
  private stack: Set<Key>[] = []

  constructor(
    private update: (self: Accessor<K, V>, key: K) => V,
    private init: (key: K) => V,
    private hash: (key: K) => Key,
    private check: (oldVal: V, newVal: V) => void = () => { }
  ) { }

  private note(k: Key) {
    if (this.stack.length) this.stack[this.stack.length - 1].add(k)
  }

  private track<T>(f: () => T): [T, Set<Key>] {
    const s = new Set<Key>()
    this.stack.push(s)
    try {
      return [f(), s]
    } finally {
      this.stack.pop()
    }
  }

  private clearDeps(k: Key) {
    const fr = this.frames.get(k)!
    for (const d of fr.deps) this.frames.get(d)?.edges.delete(k)
    fr.deps.clear()
  }

  private refresh(k: Key) {
    this.clearDeps(k)
    const fr = this.frames.get(k)!
    const [val, deps] = this.track(() => this.update(new Inner(this), fr.key))
    this.check(fr.value, val)
    fr.deps = deps
    for (const d of deps) this.frames.get(d)?.edges.add(k)
    if (!isEqual(val, fr.value)) {
      fr.value = val
      fr.edges.forEach(s => this.queue.push(s))
    }
  }

  private fetch(k: K, loop: boolean): V {
    const key = this.hash(k)
    this.note(key)
    if (!this.frames.has(key)) {
      this.frames.set(key, new Frame(k, this.init(k)))
      this.refresh(key)
    }
    if (loop) {
      while (!this.queue.empty) this.refresh(this.queue.pop()!)
    }
    return this.frames.get(key)!.value
  }

  get(k: K): V {
    return this.fetch(k, true)
  }
}

class Inner<K, V> implements Accessor<K, V> {
  constructor(private fp: Fixpoint<K, V>) { }
  get(k: K): V {
    return (this.fp as any).fetch(k, false)
  }
}
