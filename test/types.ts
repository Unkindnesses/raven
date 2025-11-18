import { test } from 'uvu'
import * as assert from 'assert'
import {
  Type, tag, pack, vpack, bits, float64, float32, recursive,
  recurrence, onion, issubset, union, recur, unroll, finite, simplify, Any
} from '../src/frontend/types.js'
import * as types from '../src/frontend/types.js'

test('', () => {
  const T1 = onion(pack(tag('Empty')), pack(tag('Prepend'), pack(tag('Empty')), float64()))
  const T2 = pack(tag('Prepend'), onion(pack(tag('Empty')), pack(tag('Prepend'), pack(tag('Empty')), float64())), float64())
  const T = union(T1, T2)
  assert.deepEqual(union(T2, T1), T)
  assert.deepEqual(union(union(T1, tag('c')), T2), union(T, tag('c')))
  assert.deepEqual(T, recursive(onion(pack(tag('Empty')), pack(tag('Prepend'), recurrence, float64()))))

  assert.ok(issubset(T1, T))
  assert.ok(issubset(T2, T))
  assert.ok(!issubset(T, T1))
  assert.ok(issubset(T, T))
  assert.deepEqual(union(T, T), T)
  const Tw = pack(tag('Prepend'), T, float64())
  assert.deepEqual(recur(Tw), Tw)
  assert.ok(issubset(Tw, T))
  assert.ok(issubset(types.unroll(T), T))
  assert.ok(issubset(T, types.unroll(T)))
  assert.deepEqual(recur(types.unroll(T)), T)
})

test('', () => {
  const A = recursive(vpack(tag('a'), vpack(tag('b'), recurrence)))
  const B = recursive(vpack(tag('b'), vpack(tag('a'), recurrence)))
  assert.deepEqual(types.unroll(A), vpack(tag('a'), B))
})

test('', () => {
  const List = (T: Type) => recursive(onion(pack(tag('Empty')), pack(tag('Prepend'), recurrence, T)))
  assert.deepEqual(recur(types.finite(List(List(float64())))), List(List(float64())))
})

test('', () => {
  const T1 = recursive(onion(pack(tag('Empty1')), pack(tag('Prepend1'), recurrence, float64())))
  const T2 = recursive(onion(pack(tag('Empty2')), pack(tag('Prepend2'), recurrence, float64())))
  assert.deepEqual(union(T1, tag('b')), onion(T1, tag('b')))
  assert.deepEqual(union(T1, T2), onion(T1, T2))
})

test('', () => {
  const T1 = recursive(onion(pack(tag('Empty')), pack(tag('Prepend'), recurrence, bits(64))))
  const T2 = recursive(onion(pack(tag('Empty')), pack(tag('Prepend'), recurrence, float64())))
  const T3 = union(T1, T2)

  assert.ok(issubset(T1, T1))
  assert.ok(issubset(T3, T3))
  assert.ok(issubset(T1, T3))
  assert.ok(!issubset(T3, T1))
})

test('', () => {
  const T = onion(recursive(onion(pack(tag('a'), recurrence), tag('b'))), tag('c'))
  assert.deepEqual(union(T, T), T)
  assert.deepEqual(recur(types.unroll(T)), T)
  assert.deepEqual(union(T, pack(tag('a'), tag('b'))), T)
})

test('', () => {
  const T1 = onion(pack(tag('Empty')), pack(tag('Prepend'), pack(tag('Empty')), bits(64)))
  const T2 = pack(tag('Prepend'), onion(float64(), pack(tag('Empty')), pack(tag('Prepend'), pack(tag('Empty')), bits(64))), bits(64))
  const T = union(T1, T2)
  assert.deepEqual(T, recursive(onion(float64(), pack(tag('Empty')), pack(tag('Prepend'), recurrence, bits(64)))))
})

test('', () => {
  const A = recursive(onion(vpack(tag('a'), recurrence), vpack(tag('b'), recurrence)))
  assert.deepEqual(union(A, A), A)
})

test('', () => {
  let T = onion(pack(tag('b'), recursive(onion(float64(), bits(64), pack(tag('a'), recurrence)))), bits(64), float64())
  assert.deepEqual(recur(T), recursive(onion(float64(), bits(64), pack(tag('a'), recurrence), pack(tag('b'), recurrence))))

  T = onion(float64(), bits(64), pack(tag('d'), pack(tag('d'), recursive(onion(bits(64), float64(), pack(tag('b'), recurrence))))))
  assert.deepEqual(recur(T), recursive(onion(float64(), bits(64), pack(tag('b'), recurrence), pack(tag('d'), recurrence))))

  T = onion(float64(), bits(64), pack(tag('a'), onion(float64(), bits(64), vpack(tag('c'), bits(32))), bits(64)), vpack(tag('c'), onion(float64(), bits(64), pack(tag('c')))))
  assert.deepEqual(recur(T), recursive(onion(float64(), bits(32), bits(64), pack(tag('a'), recurrence, bits(64)), vpack(tag('c'), recurrence))))
})

test('', () => {
  const A = vpack(tag('a'), bits(64))
  const B = pack(tag('a'), vpack(tag('a'), bits(64)))
  assert.deepEqual(union(A, B), recursive(onion(bits(64), vpack(tag('a'), recurrence))))
})

test('', () => {
  const A = vpack(tag('a'), vpack(tag('a'), bits(64)))
  const B = onion(bits(64, 1), vpack(tag('a'), bits(64)))
  assert.deepEqual(union(A, B), recursive(onion(bits(64), vpack(tag('a'), recurrence))))
})

test('', () => {
  const T = onion(bits(64), pack(tag('a'), onion(float64(), bits(64)), onion(float64(), bits(64), pack(tag('a'), bits(64), bits(64)))))
  assert.deepEqual(recur(T), recursive(onion(float64(), bits(64), pack(tag('a'), recurrence, recurrence))))
})

test('', () => {
  const A = pack(tag('a'), pack(tag('a'), bits(64)))
  const B = pack(tag('a'), onion(float64(), pack(tag('a'), float64())))
  const C = float64()
  assert.deepEqual(union(union(A, B), C), union(A, union(B, C)))
})

test('', () => {
  let A = onion(bits(64, 4), vpack(tag('a'), float64()))
  let B = pack(tag('a'), bits(64, 4))
  let C = pack(tag('a'), bits(64, 5))
  assert.deepEqual(union(union(A, B), C), union(A, union(B, C)))

  A = pack(tag('c'), tag('a'), pack(tag('c')))
  B = pack(tag('c'))
  C = pack(tag('c'), pack(tag('c'), tag('c')))
  assert.deepEqual(union(union(A, B), C), union(A, union(B, C)))
})

test('', () => {
  const A = tag('b')
  const B = vpack(tag('c'), onion(bits(64), tag('b')))
  const C = recursive(vpack(tag('a'), onion(pack(tag('c'), recurrence), tag('a'))))
  assert.deepEqual(union(union(A, B), C), union(A, union(B, C)))
})

test('', () => {
  const A = vpack(tag('c'), bits(64))
  const U = recursive(onion(float64(), vpack(tag('b'), recurrence), A))
  assert.deepEqual(union(A, U), U)
})

test('', () => {
  const A = Type(1.0)
  const B = pack(tag('c'))
  const C = pack(tag('c'), vpack(tag('c'), bits(64)))
  assert.deepEqual(union(union(A, B), C), union(A, union(B, C)))
})

test('', () => {
  const A = pack(tag('c'), onion(bits(64), pack(tag('c'))), tag('b'))
  const B = bits(64, 1)
  const C = tag('c')
  assert.deepEqual(union(union(A, B), C), union(A, union(B, C)))
})

test('', () => {
  const A = pack(tag('c'), onion(bits(64), pack(tag('c'))), tag('b'))
  assert.deepEqual(union(A, bits(64, 1)), onion(A, bits(64, 1)))
})

test('', () => {
  const A = pack(tag('b'), bits(64), pack(tag('a'), onion(bits(64), pack(tag('a'), bits(64)))))
  const B = pack(tag('b'))
  assert.deepEqual(union(A, B), vpack(tag('b'), recursive(onion(bits(64), pack(tag('a'), recurrence)))))
})

test('', () => {
  const A = recursive(vpack(tag('a'), vpack(tag('b'), recurrence)))
  assert.deepEqual(recur(vpack(tag('b'), A)), recursive(vpack(tag('b'), vpack(tag('a'), recurrence))))
})

test('', () => {
  const A = vpack(tag('a'), vpack(tag('b'), bits(64)))
  const B = recursive(vpack(tag('a'), vpack(tag('b'), recurrence)))
  const C = bits(64)
  assert.deepEqual(union(union(A, B), C), recursive(onion(bits(64), vpack(tag('a'), vpack(tag('b'), recurrence)))))
})

test('', () => {
  const A = pack(tag('a'), vpack(tag('c'), tag('b')), bits(64))
  const B = bits(64)
  const C = pack(tag('a'), vpack(tag('c'), onion(bits(64), float64())), bits(64))
  assert.deepEqual(union(union(A, B), C), union(A, union(B, C)))
})

test('', () => {
  const A = pack(tag('b'), bits(64))
  const B = vpack(tag('b'), pack(tag('a'), vpack(tag('c'), float64())))
  const C = vpack(tag('c'), bits(64))
  assert.deepEqual(union(A, union(B, C)), union(union(A, B), C))
})

test('', () => {
  const A = pack(tag('b'))
  const B = pack(tag('b'), pack(tag('a'), onion(bits(64), float64())), vpack(tag('c'), float64()))
  const C = float64()
  assert.deepEqual(union(union(A, B), C), union(A, union(B, C)))
})

test('', () => {
  const A = vpack(tag('c'), bits(64, 3))
  const B = bits(64, 3)
  const C = pack(tag('c'), pack(tag('a'), vpack(tag('c'), bits(64))))
  assert.deepEqual(union(A, union(B, C)), union(union(A, B), C))
})

test('', () => {
  const A = float64()
  const B = vpack(tag('c'), float64())
  const C = vpack(tag('a'), recursive(onion(float64(), vpack(tag('b'), pack(tag('c'), recurrence)))))
  assert.deepEqual(union(A, union(B, C)), union(union(A, B), C))
})

test('', () => {
  const T = pack(tag('a'), recursive(vpack(tag('b'), recurrence)))
  assert.deepEqual(union(T, tag('b')), onion(T, tag('b')))
})

test('', () => {
  const A = pack(tag('a'), bits(64), vpack(tag('a'), bits(64)))
  const B = onion(pack(tag('a'), onion(float64(), bits(64)), bits(64)), bits(64))
  const C = bits(64)
  assert.deepEqual(union(A, union(B, C)), union(union(A, B), C))
})

test('', () => {
  const A = pack(tag('a'), bits(64))
  const B = vpack(tag('a'), pack(tag('c')))
  const C = onion(pack(tag('a'), pack(tag('a'))), vpack(tag('c'), float64()))
  assert.deepEqual(union(union(A, B), C), union(A, union(B, C)))
})

test('', () => {
  const T = onion(float64(), bits(64), pack(tag('c'), vpack(tag('c'), recursive(onion(float64(), bits(64), pack(tag('b'), recurrence))))))
  assert.deepEqual(recur(T), recursive(onion(float64(), bits(64), pack(tag('b'), recurrence), vpack(tag('c'), recurrence))))
})

test('', () => {
  const A = pack(tag('d'))
  const B = pack(tag('a'), onion(float32(), float64(), bits(32)), vpack(tag('c'), onion(bits(32), tag('c'))))
  const C = tag('c')
  assert.deepEqual(union(union(A, B), C), union(A, union(B, C)))
})

test('', () => {
  const I = recursive(onion(vpack(tag('a'), recurrence), vpack(tag('b'), recurrence)))
  const T = pack(tag('b'), I, vpack(tag('b'), bits(32)))
  assert.deepEqual(recur(T), T) // recursive(onion(bits(32), vpack(tag('a'), recurrence), vpack(tag('b'), recurrence)))
})

test('', () => {
  const T = vpack(tag('d'), vpack(tag('c'), pack(tag('b'), onion(float64(), pack(tag('d'))))))
  const R = recur(T)
  assert.deepEqual(recur(unroll(R)), R)
})

test('', () => {
  const T = onion(vpack(tag('a'), pack(tag('b'))), vpack(tag('b'), float64()), vpack(tag('d'), vpack(tag('a'), vpack(tag('b'), float64()))))
  assert.deepEqual(recur(T), recursive(onion(vpack(tag('a'), recurrence), vpack(tag('b'), float64()), vpack(tag('d'), recurrence))))
})

test('', () => {
  const T = pack(tag('List'), vpack(tag('List'), vpack(tag('List'), float64())))
  assert.deepEqual(recur(T), T)
})

// `Any` types

test('', () => {
  const List = (T: Type) => recursive(onion(pack(tag('Empty')), pack(tag('Prepend'), recurrence, T)))
  assert.deepEqual(simplify(recur(finite(List(Any)))), List(Any))
})

test('', () => {
  const A = vpack(tag('c'), vpack(tag('c'), float64(3.0)))
  const B = vpack(tag('c'), Any)
  const C = float64()
  const U = union(union(A, B), C)
  assert.deepEqual(U, union(A, union(B, C)))
  assert.deepEqual(simplify(U), onion(vpack(tag('c'), Any), float64()))
})

test('', () => {
  const A = pack(tag('b'), vpack(tag('b'), float64()))
  const B = vpack(tag('b'), float64())
  const C = vpack(tag('b'), Any)
  const U = union(union(A, B), C)
  assert.deepEqual(U, union(A, union(B, C)))
  assert.deepEqual(simplify(U), Any)
})

test.run()
