import { test } from 'uvu'
import * as assert from 'assert'
import { Fixpoint } from '../src/utils/fixpoint'
import { Map, Cache, EagerCache, pipe, reset, fingerprint, CycleCache } from '../src/utils/cache'

test('Fixpoint square root', () => {
  const fp = new Fixpoint<number, number>(
    (self, x) => {
      let y = self.get(x)
      return (y + x / y) / 2
    },
    x => x, x => x
  )
  const result = fp.get(2)
  assert.ok(Math.abs(result * result - 2) < 1e-10, 'Should approximate sqrt(2)')
})

test('Basics', () => {
  let level1 = new Map<number, number>()
  level1.set(1, 5)
  assert.equal(level1.get(1), 5)
  assert.equal(level1.get(2), undefined)

  let cache_log: number[] = []

  let level2 = new Cache<number, string>((i) => {
    cache_log.push(i)
    return String(level1.get(i))
  })
  assert.equal(level2.get(1), '5')
  assert.deepEqual(cache_log, [1])

  cache_log = []
  reset(level2, [level1])
  assert.equal(level2.get(1), '5')
  assert.deepEqual(cache_log, [])

  level1.set(1, 6)
  reset(level2, [level1])
  assert.equal(level2.get(1), '6')

  let level3 = new Cache<number, string>((i) => {
    return level2.get(i) + '!'
  })
  assert.equal(level3.get(1), '6!')

  level1.set(1, 7)
  reset(pipe(level1, level2, level3))
  assert.equal(level3.get(1), '7!')

  let optional = new Cache<number, string>((i) => {
    return level1.has(i) ? `${level1.get(i)}` : 'default'
  })

  assert.equal(optional.get(1), '7')
  assert.equal(optional.get(5), 'default')

  level1.set(5, 13)
  reset(optional, [level1])
  assert.equal(optional.get(5), '13')

  level1.delete(5)
  reset(optional, [level1])
  assert.equal(optional.get(5), 'default')
})

test('Eager cache', () => {
  let data = new Map<number, number>()
  data.set(1, 2)
  let log: number[] = []

  let level1 = new EagerCache<number, number>((i) => {
    log.push(i)
    return data.get(i)! ** 2
  })

  let level2 = new Cache<number, number>((i) => {
    log.push(i)
    return level1.get(i) + 1
  })

  let p = pipe(data, level1, level2)

  assert.equal(level2.get(1), 5)
  assert.deepEqual(log, [1, 1])
  log = []

  data.set(1, 2)
  reset(p)
  assert.deepEqual(log, [1])

  assert.equal(level2.get(1), 5)
  assert.deepEqual(log, [1])
  log = []

  data.set(1, 3)
  reset(p)
  assert.deepEqual(log, [1])
  assert.equal(level2.get(1), 10)
  assert.deepEqual(log, [1, 1])
})

test('Cycle cache', () => {
  let ch = new CycleCache<number, number>(
    x => x,
    (self, x) => {
      let y = self.get(x)
      return (y + x / y) / 2
    })
  assert.ok(2 - ch.get(2) ** 2 < 1e-10)
  reset(ch)
  assert.ok(ch.iscached(2))
})

test('Recursive Fibonacci', () => {
  let log: number[] = []
  let init = new Map<number, number>()
  init.set(0, 0); init.set(1, 1)

  let fib = new CycleCache<number, number>(
    x => 0,
    (self, i) => {
      log.push(i)
      return i <= 1 ? init.get(i)! : self.get(i - 1) + self.get(i - 2)
    })

  assert.equal(fib.get(10), 55)
  assert.equal(fib.get(5), 5)
  assert.deepEqual(log, [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0])
  log = []

  reset(fib, [init])
  assert.equal(fib.get(10), 55)
  assert.deepEqual(log, [])

  init.set(0, 1)
  reset(fib, [init])
  assert.equal(fib.get(10), 89)
  assert.equal(fib.get(5), 8)
})

test.run()
