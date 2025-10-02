import { test } from 'uvu'
import { test as rv, reset as _reset } from '../src/test'

function reset() { test('reset', () => { _reset() }) }

test('Ptr', async () => {
  await rv(`
    p = Ptr(widen(5))
    a = addr(p)

    test Int64(a) == 5
  `)
})

test('simple malloc', async () => {
  await rv(`
    ptr1 = malloc!(Int32(16))
    test ptr1 == (Ptr(0) + headerSize)
    test blockSize(ptr1) == Int32(16)

    ptr2 = malloc!(Int32(16))
    test ptr2 == (Ptr(0) + headerSize + Int32(16) + headerSize)
    test blockSize(ptr2) == Int32(16)

    test allocationCount() == 2

    free!(ptr1)
    free!(ptr2)
  `)
})

test('retain/release', async () => {
  await rv(`
    ptr = malloc!(Int32(16))
    test allocationCount() == 1
    test blockCount(ptr) == 1

    retain!(ptr)
    test blockCount(ptr) == 2

    release!(ptr)
    test blockCount(ptr) == 1

    release!(ptr)
    test allocationCount() == 0

    # trim blocks resets used
  `)
})

test('retain/release', async () => {
  await rv(`
    {
      setBlockUsed!(Ptr(16), Int32(true))
      ptr = malloc!(Int32(8))
      release!(ptr)
      test allocationCount() == 0
    }
  `)
})

// Normally the last (free) block is split by an allocation, so the last block
// on the heap will be free. Sometimes the block is not split, though, so we
// have to check.
test('double malloc', async () => {
  await rv(`
    ptr1 = malloc!(pageSize - headerSize)

    ptr2 = malloc!(Int32(16))

    test ptr1 != ptr2

    free!(ptr1)
    free!(ptr2)
  `)
})

test('triple malloc', async () => {
  await rv(`
    ptr1 = malloc!(Int32(8))
    ptr2 = malloc!(Int32(8))

    test (ptr1 == Ptr(8)) && (ptr2 == Ptr(24))

    free!(ptr1)
    free!(ptr2)
    ptr3 = malloc!(Int32(16))

    test ptr3 == Ptr(8)

    free!(ptr3)
  `)
})

test('invoke', async () => {
  await rv(`
    TInt64 = Pack(Literal(Int), bits 64)
    f = Function(+, [TInt64, TInt64], TInt64)
    test invoke(f, 3, 5) == 8
  `)
})

test('ref cleanup', async () => {
  await rv(`
    {
      ref = Ref(16)
    }

    test allocationCount() == 0
  `)
})

reset()

test.run()
