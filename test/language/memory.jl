using Raven, Test
using Raven: @test_rv, @test_rv_str

test_rv"""
  p = Ptr(widen(5))
  a = addr(p)

  println(Int64(a) == 5)
  """

test_rv"""
  ptr1 = malloc!(Int32(16))
  println(ptr1 == (Ptr(0) + headerSize))
  println(blockSize(ptr1) == Int32(16))

  ptr2 = malloc!(Int32(16))
  println(ptr2 == (Ptr(0) + headerSize + Int32(16) + headerSize))
  println(blockSize(ptr2) == Int32(16))

  println(allocationCount() == 2)

  free!(ptr1)
  free!(ptr2)
  """

# retain / release
test_rv"""
  ptr = malloc!(Int32(16))
  println(allocationCount() == 1)
  println(blockCount(ptr) == 1)

  retain!(ptr)
  println(blockCount(ptr) == 2)

  release!(ptr)
  println(blockCount(ptr) == 1)

  release!(ptr)
  println(allocationCount() == 0)

  # trim blocks resets used
  """

test_rv"""
  {
    setBlockUsed!(Ptr(16), true)
    ptr = malloc!(Int32(8))
    release!(ptr)
    println(allocationCount() == 0)
  }
  """

# Normally the last (free) block is split by an allocation, so the last block
# on the heap will be free. Sometimes the block is not split, though, so we
# have to check.
test_rv"""
  ptr1 = malloc!(pageSize - headerSize)

  ptr2 = malloc!(Int32(16))

  println(ptr1 != ptr2)

  free!(ptr1)
  free!(ptr2)
  """

test_rv"""
  ptr1 = malloc!(Int32(8))
  ptr2 = malloc!(Int32(8))

  println((ptr1 == Ptr(8)) & (ptr2 == Ptr(24)))

  free!(ptr1)
  free!(ptr2)
  ptr3 = malloc!(Int32(16))

  println(ptr3 == Ptr(8))

  free!(ptr3)
  """

test_rv"""
  f = Function(+, [Int64, Int64], Int64)
  println(invoke(f, 3, 5) == 8)
  """

test_rv"""
  {
    ref = Ref(16)
  }

  println(allocationCount() == 0)
  """
