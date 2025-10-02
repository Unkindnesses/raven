import isEqual from 'lodash/isEqual'
import { HashMap, HashSet } from './map'

export { WorkQueue, Fixpoint, Accessor }

interface Accessor<K, V> {
  get(key: K): V
}

class WorkQueue<K> {
  private items: K[] = []
  private seen = new HashSet<K>()

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

class Frame<K, V> {
  value: V
  deps = new Set<K>()
  edges = new Set<K>()
  constructor(v: V) {
    this.value = v
  }
}

class Fixpoint<K, V> {
  private frames = new HashMap<K, Frame<K, V>>()
  private queue = new WorkQueue<K>()
  private stack: Set<K>[] = []

  constructor(
    private update: (self: Accessor<K, V>, key: K) => V,
    private init: (key: K) => V,
    private check: (oldVal: V, newVal: V) => void = () => { }
  ) { }

  private note(k: K) {
    if (this.stack.length) this.stack[this.stack.length - 1].add(k)
  }

  private track<T>(f: () => T): [T, Set<K>] {
    const s = new Set<K>()
    this.stack.push(s)
    try {
      return [f(), s]
    } finally {
      this.stack.pop()
    }
  }

  private clearDeps(k: K) {
    const fr = this.frames.get(k)!
    for (const d of fr.deps) this.frames.get(d)?.edges.delete(k)
    fr.deps.clear()
  }

  private refresh(k: K) {
    this.clearDeps(k)
    const fr = this.frames.get(k)!
    const [val, deps] = this.track(() => this.update(new Inner(this), k))
    this.check(fr.value, val)
    fr.deps = deps
    for (const d of deps) this.frames.get(d)?.edges.add(k)
    if (!isEqual(val, fr.value)) {
      fr.value = val
      fr.edges.forEach(s => this.queue.push(s))
    }
  }

  private fetch(k: K, loop: boolean): V {
    this.note(k)
    if (!this.frames.has(k)) {
      this.frames.set(k, new Frame(this.init(k)))
      this.refresh(k)
    }
    if (loop) {
      while (!this.queue.empty) this.refresh(this.queue.pop()!)
    }
    return this.frames.get(k)!.value
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
