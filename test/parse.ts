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
  %4 = global tag"".+
  %5 = call %4, %3 # test:1:14 🔴
  %6 = call Method(tag"common.core.part"), %5, 1 # test:1:14
  %7 = return %6`)
})

test('lower control flow', () => {
  const ir = lower('fn test(x) { if x > 0 { x + 1 } else { x - 1 } }')
  assert.equal(ir.toString(), `1: (%1)
  %2 = tuple :: 0
  %3 = pack tag"common.List", %1, %2
  %4 = global tag"".>
  %5 = call %4, %3 # test:1:18 🔴
  %6 = call Method(tag"common.core.part"), %5, 1 # test:1:18
  %7 = pack tag"common.List", %6
  %8 = call tag"common.condition", %7
  %9 = call Method(tag"common.core.part"), %8, 1
  %10 = br 2 if %9
  %11 = br 3
2:
  %12 = tuple :: 1
  %13 = pack tag"common.List", %1, %12
  %14 = global tag"".+
  %15 = call %14, %13 # test:1:26 🔴
  %16 = call Method(tag"common.core.part"), %15, 1 # test:1:26
  %17 = br 4 (%16)
3:
  %18 = tuple :: 1
  %19 = pack tag"common.List", %1, %18
  %20 = global tag"".-
  %21 = call %20, %19 # test:1:41 🔴
  %22 = call Method(tag"common.core.part"), %21, 1 # test:1:41
  %23 = br 4 (%22)
4: (%24)
  %25 = return %24`)
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
  const ir = lower('fn loop(x) { while x > 0 { x = x - 1 }, return }')
  assert.equal(ir.toString(), `1: (%1)
  %2 = br 2 (%1)
2: (%3)
  %4 = tuple :: 0
  %5 = pack tag"common.List", %3, %4
  %6 = global tag"".>
  %7 = call %6, %5 # test:1:21 🔴
  %8 = call Method(tag"common.core.part"), %7, 1 # test:1:21
  %9 = pack tag"common.List", %8 # test:1:14
  %10 = call tag"common.condition", %9 # test:1:14
  %11 = call Method(tag"common.core.part"), %10, 1 # test:1:14
  %12 = br 3 if %11 # test:1:14
  %13 = br 4 # test:1:14
3:
  %14 = tuple :: 1
  %15 = pack tag"common.List", %3, %14
  %16 = global tag"".-
  %17 = call %16, %15 # test:1:33 🔴
  %18 = call Method(tag"common.core.part"), %17, 1 # test:1:33
  %19 = br 2 (%18)
4:
  %20 = global tag"common".nil
  %21 = return %20`)
})

test('lower toplevel expression', () => {
  const mod = new Module(tag('test'))
  mod.set('x', Type(42))
  const expr = parse('test', '{ x = x+1, y = y+1 }')[0]
  const [ir, _] = lower_toplevel(mod, expr, x => { throw new Error('nop') })
  assert.equal(ir.toString(), `1:
  %1 = tuple :: 1
  %2 = global tag"test".x
  %3 = pack tag"common.List", %2, %1
  %4 = global tag"test".+
  %5 = call %4, %3 # test:1:8 🔴
  %6 = call Method(tag"common.core.part"), %5, 1 # test:1:8
  %7 = tuple :: 1
  %8 = global tag"test".y
  %9 = pack tag"common.List", %8, %7
  %10 = global tag"test".+
  %11 = call %10, %9 # test:1:17 🔴
  %12 = call Method(tag"common.core.part"), %11, 1 # test:1:17
  %13 = set tag"test".x, %6
  %14 = global tag"common".nil
  %15 = return %14`)
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
  %11 = "match failed: [x, y]" :: [int 32]
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
  %25 = global tag"common".nil
  %26 = pack tag"common.List", %25, %21, %24
  %27 = return %26`)
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
  const ir = lower('fn iter(xs) { for x = xs { println(x) }, return }')
  assert.equal(ir.toString(), `1: (%1)
  %2 = pack tag"common.List", %1
  %3 = tuple :: tag"common.iterator"
  %4 = call %3, %2
  %5 = call Method(tag"common.core.part"), %4, 1
  %6 = br 2 (%5)
2: (%7)
  %8 = global tag"".true
  %9 = pack tag"common.List", %8
  %10 = call tag"common.condition", %9
  %11 = call Method(tag"common.core.part"), %10, 1
  %12 = br 3 if %11
  %13 = br 6
3:
  %14 = pack tag"common.List", %7
  %15 = tuple :: tag"common.next"
  %16 = call %15, %14
  %17 = call Method(tag"common.core.part"), %16, 1
  %18 = call Method(tag"common.core.part"), %16, 2
  %19 = pack tag"common.List", %17
  %20 = global tag"".nil?
  %21 = call %20, %19
  %22 = call Method(tag"common.core.part"), %21, 1
  %23 = pack tag"common.List", %22
  %24 = call tag"common.condition", %23
  %25 = call Method(tag"common.core.part"), %24, 1
  %26 = br 4 if %25
  %27 = br 5
4:
  %28 = br 6
5:
  %29 = tuple :: tag"common.Nil"
  %30 = pack tag"common.List", %29
  %31 = global tag"".pack
  %32 = call %31, %30
  %33 = call Method(tag"common.core.part"), %32, 1
  %34 = pack tag"common.List", %17
  %35 = tuple :: tag"common.core.notnil"
  %36 = call %35, %34
  %37 = call Method(tag"common.core.part"), %36, 1
  %38 = tuple :: 1
  %39 = pack tag"common.List", %37, %38
  %40 = tuple :: tag"common.core.part"
  %41 = call %40, %39
  %42 = call Method(tag"common.core.part"), %41, 1
  %43 = pack tag"common.List", %42 # test:1:35
  %44 = global tag"".println
  %45 = call %44, %43 # test:1:35 🔴
  %46 = call Method(tag"common.core.part"), %45, 1 # test:1:35
  %47 = br 2 (%18)
6:
  %48 = global tag"common".nil
  %49 = return %48`)
})

test.run()
