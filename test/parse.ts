import { test } from 'uvu'
import * as assert from 'assert'
import { parse, PrecTable, Prec, inverse, table } from '../src/frontend/parse'
import { lowerfn, lower_toplevel } from '../src/frontend/lower'
import { lowerpattern } from '../src/frontend/patterns'
import { tag, Type } from '../src/frontend/types'
import { asSymbol } from '../src/frontend/ast'
import * as ast from '../src/frontend/ast'
import { Module } from '../src/frontend/modules'

test('parse simple function definition', () => {
  const tree = parse('test', 'def foo(x) { while (true) { println(1 + 2) } }')
  assert.ok(tree, 'Parser should return an ast')
  const treeString = `${tree}`
  assert.ok(treeString.includes('foo'))
  assert.ok(treeString.includes('while'))
  assert.ok(treeString.includes('println'))
})

test('precedence table transitivity', () => {
  function transitive(t: PrecTable): boolean {
    let trans = true
    const N = t.ops.size
    const ops = t.ops
    for (let i = 0; i < N; i++) {
      for (let j = 0; j < N; j++) {
        if (i !== j) trans &&= (t.table[i][j]) === inverse(t.table[j][i])
        for (let k = 0; k < N; k++) {
          const ab = t.table[i][j]
          const bc = t.table[j][k]
          const ac = t.table[i][k]
          if (ab === bc && ab !== Prec.None) trans &&= ab === ac
        }
      }
    }
    return trans
  }
  assert.ok(transitive(table))
})

function lower(def: string, resolver: (x: ast.Symbol) => Type = x => { throw new Error('undefined') }) {
  const ex = parse('test', def)[0]
  if (!ast.isExpr(ex, 'Syntax') || asSymbol(ex.args[0].unwrap()).toString() !== 'fn')
    throw new Error('Expected function definition starting with "fn"')
  const params = ast.asExpr(ex.args[1], 'Call').args.slice(1)
  const body = ex.args[2]
  const sig = lowerpattern(ast.List(...params))
  return lowerfn(tag(''), sig, body, resolver)
}

test('lower simple function', () => {
  const ir = lower('fn foo(x) { x + 1 }')
  assert.equal(ir.toString(), `1: (%1)
  %2 = tuple :: 1
  %3 = pack tag"common.List", %1, %2
  %4 = call tag"".+, %3 # test:1:14 🔴
  %5 = call Method(tag"common.core.part"), %4, 1 # test:1:14
  %6 = return %5`)
})

test('lower control flow', () => {
  const ir = lower('fn test(x) { if x > 0 { x + 1 } else { x - 1 } }')
  assert.equal(ir.toString(), `1: (%1)
  %2 = tuple :: 0
  %3 = pack tag"common.List", %1, %2
  %4 = call tag"".>, %3 # test:1:18 🔴
  %5 = call Method(tag"common.core.part"), %4, 1 # test:1:18
  %6 = pack tag"common.List", %5
  %7 = call tag"common.condition", %6
  %8 = call Method(tag"common.core.part"), %7, 1
  %9 = br 2 if %8
  %10 = br 3
2:
  %11 = tuple :: 1
  %12 = pack tag"common.List", %1, %11
  %13 = call tag"".+, %12 # test:1:26 🔴
  %14 = call Method(tag"common.core.part"), %13, 1 # test:1:26
  %15 = br 4 (%14)
3:
  %16 = tuple :: 1
  %17 = pack tag"common.List", %1, %16
  %18 = call tag"".-, %17 # test:1:41 🔴
  %19 = call Method(tag"common.core.part"), %18, 1 # test:1:41
  %20 = br 4 (%19)
4: (%21)
  %22 = return %21`)
})

test('lower if let', () => {
  const ir = lower('fn option(x) { if let Some(y) = x { y } else { 0 } }', sym => {
    if (sym.toString() === 'Some') return tag('common.Some')
    throw new Error('undefined')
  })
  assert.equal(ir.toString(), `1: (%1)
  %2 = pack tag"common.List", %1, pack(tag"common.Constructor", tag"common.Some", pack(tag"common.Bind", tag"y", pack(tag"common.Hole")))
  %3 = call tag"common.match", %2
  %4 = call Method(tag"common.core.part"), %3, 1
  %5 = call Method(tag"common.core.nil?"), %4
  %6 = br 3 if %5
  %7 = br 2
2:
  %8 = pack tag"common.List", %4, tag"y"
  %9 = call tag"common.getkey", %8
  %10 = call Method(tag"common.core.part"), %9, 1
  %11 = br 4 (%10)
3:
  %12 = tuple :: 0
  %13 = br 4 (%12)
4: (%14)
  %15 = return %14`)
})

test('lower while loop', () => {
  const ir = lower('fn loop(x) { while x > 0 { x = x - 1 } }')
  assert.equal(ir.toString(), `1: (%1)
  %2 = br 2 (%1)
2: (%3)
  %4 = tuple :: 0
  %5 = pack tag"common.List", %3, %4
  %6 = call tag"".>, %5 # test:1:21 🔴
  %7 = call Method(tag"common.core.part"), %6, 1 # test:1:21
  %8 = pack tag"common.List", %7 # test:1:14
  %9 = call tag"common.condition", %8 # test:1:14
  %10 = call Method(tag"common.core.part"), %9, 1 # test:1:14
  %11 = br 3 if %10 # test:1:14
  %12 = br 4 # test:1:14
3:
  %13 = tuple :: 1
  %14 = pack tag"common.List", %3, %13
  %15 = call tag"".-, %14 # test:1:33 🔴
  %16 = call Method(tag"common.core.part"), %15, 1 # test:1:33
  %17 = br 2 (%16)
4:
  %18 = return tag"common".nil`)
})

test('lower toplevel expression', () => {
  const mod = new Module(tag('test'))
  mod.set('x', Type(42))
  const expr = parse('test', '{ x = x+1, y = y+1 }')[0]
  const [ir, _] = lower_toplevel(mod, expr, x => { throw new Error('nop') })
  assert.equal(ir.toString(), `1:
  %1 = tuple :: 1
  %2 = pack tag"common.List", tag"test".x, %1
  %3 = call tag"test".+, %2 # test:1:8 🔴
  %4 = call Method(tag"common.core.part"), %3, 1 # test:1:8
  %5 = tuple :: 1
  %6 = pack tag"common.List", tag"test".y, %5
  %7 = call tag"test".+, %6 # test:1:17 🔴
  %8 = call Method(tag"common.core.part"), %7, 1 # test:1:17
  %9 = set tag"test".x, %4
  %10 = return tag"common".nil`)
})

test('lower function with swap pattern', () => {
  const ir = lower('fn swap(&x, &y) { [x, y] = [y, x], return }')
  assert.equal(ir.toString(), `1: (%1, %2)
  %3 = pack tag"common.List", %2, %1 # test:1:28
  %4 = tuple :: pack(tag"common.Pack", pack(tag"common.Literal", tag"common.List"), pack(tag"common.Bind", tag"x", pack(tag"common.Hole")), pack(tag"common.Bind", tag"y", pack(tag"common.Hole")))
  %5 = pack tag"common.List", %3, %4
  %6 = call tag"common.match", %5
  %7 = call Method(tag"common.core.part"), %6, 1
  %8 = call Method(tag"common.core.nil?"), %7
  %9 = br 2 if %8
  %10 = br 3
2:
  %11 = ref "match failed: [x, y]" :: [int 32]
  %12 = call tag"common.JSObject", %11
  %13 = call tag"common.String", %12
  %14 = call Method(tag"common.core.part"), %13, 1
  %15 = pack tag"common.List", %14
  %16 = call tag"common.abort", %15
  %17 = call Method(tag"common.core.part"), %16, 1
3:
  %18 = call Method(tag"common.core.notnil"), %7
  %19 = pack tag"common.List", %18, tag"x"
  %20 = call tag"common.getkey", %19
  %21 = call Method(tag"common.core.part"), %20, 1
  %22 = pack tag"common.List", %18, tag"y"
  %23 = call tag"common.getkey", %22
  %24 = call Method(tag"common.core.part"), %23, 1
  %25 = pack tag"common.List", tag"common".nil, %21, %24
  %26 = return %25`)
})

test('lower list construction', () => {
  const ir = lower('fn test(x, y) { [x, y, 1] }')
  assert.equal(ir.toString(), `1: (%1, %2)
  %3 = tuple :: 1
  %4 = pack tag"common.List", %1, %2, %3 # test:1:17
  %5 = return %4`)
})

test('lower array indexing', () => {
  const ir = lower('fn test(arr, i) { arr[i] }')
  assert.equal(ir.toString(), `1: (%1, %2)
  %3 = pack tag"common.List", %1, %2 # test:1:22
  %4 = tuple :: tag"common.get"
  %5 = call %4, %3 # test:1:22 🔴
  %6 = call Method(tag"common.core.part"), %5, 1 # test:1:22
  %7 = return %6`)
})

test('lower template tag', () => {
  const ir = lower('fn test() { tag"hello.world" }')
  assert.equal(ir.toString(), `1:
  %1 = return tag"hello.world"`)
})

test('lower template bits', () => {
  const ir = lower('fn test() { bits"101" }')
  assert.equal(ir.toString(), `1:
  %1 = return bits"101"`)
})

test('lower for loop', () => {
  const ir = lower('fn iter(xs) { for x in xs { println(x) } }')
  assert.equal(ir.toString(), `1: (%1)
  %2 = pack tag"common.List", %1
  %3 = tuple :: tag"common.iterator"
  %4 = call %3, %2
  %5 = call Method(tag"common.core.part"), %4, 1
  %6 = br 2 (%5)
2: (%7)
  %8 = pack tag"common.List", tag"".true
  %9 = call tag"common.condition", %8
  %10 = call Method(tag"common.core.part"), %9, 1
  %11 = br 3 if %10
  %12 = br 6
3:
  %13 = pack tag"common.List", %7
  %14 = tuple :: tag"common.next"
  %15 = call %14, %13
  %16 = call Method(tag"common.core.part"), %15, 1
  %17 = call Method(tag"common.core.part"), %15, 2
  %18 = pack tag"common.List", %16
  %19 = call tag"".nil?, %18
  %20 = call Method(tag"common.core.part"), %19, 1
  %21 = pack tag"common.List", %20
  %22 = call tag"common.condition", %21
  %23 = call Method(tag"common.core.part"), %22, 1
  %24 = br 4 if %23
  %25 = br 5
4:
  %26 = br 6
5:
  %27 = tuple :: tag"common.Nil"
  %28 = pack tag"common.List", %27
  %29 = call tag"".pack, %28
  %30 = call Method(tag"common.core.part"), %29, 1
  %31 = pack tag"common.List", %16
  %32 = tuple :: tag"common.core.notnil"
  %33 = call %32, %31
  %34 = call Method(tag"common.core.part"), %33, 1
  %35 = tuple :: 1
  %36 = pack tag"common.List", %34, %35
  %37 = tuple :: tag"common.core.part"
  %38 = call %37, %36
  %39 = call Method(tag"common.core.part"), %38, 1
  %40 = pack tag"common.List", %39 # test:1:36
  %41 = call tag"".println, %40 # test:1:36 🔴
  %42 = call Method(tag"common.core.part"), %41, 1 # test:1:36
  %43 = br 2 (%17)
6:
  %44 = return tag"common".nil`)
})

test.run()
