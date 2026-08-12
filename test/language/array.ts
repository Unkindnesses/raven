import { test } from 'vitest'
import { test as rv } from '../../src/cli/test.js'

test('array vectors', async () => {
  await rv(`
    v = array[1, 2, 3, 4]
    test shape(v) == [4]
    test v[1] == 1
    test v[end] == 4
    test v[2..3] == array[2, 3]
    v[2] = 20
    v[3..4] = array[30, 40]
    test v == array[1, 20, 30, 40]
  `)
})

test('array matrices', async () => {
  await rv(`
    m = array[[1, 2, 3], [4, 5, 6]]

    test shape(m) == [2, 3]
    test length(m) == 6
    test m[end, end] == 6

    test m[2, ..] == array[4, 5, 6]
    test m[.., 2] == array[2, 5]

    m[1, ..] = array[7, 8, 9]
    m[2, 2] = 50
    test m[1, ..] == array[7, 8, 9]
    test m[2, ..] == array[4, 50, 6]

    test 2 * m == m .+ m
  `)
})

test('array show', async () => {
  await rv(`
    show array[1, 2]
    show array[[1, 2], [3, 4]]
  `, { output: ['2-element Array:\n1\n2', '2×2 Array:\n1  2\n3  4'] })
})
