import { test } from 'uvu'
import * as assert from 'assert'
import { parse, PrecTable, Prec, inverse, table } from '../src/frontend/parse.js'
import { lowerfn, lower_toplevel } from '../src/frontend/lower.js'
import { lowerpattern } from '../src/frontend/patterns.js'
import { tag, Type } from '../src/frontend/types.js'
import { asSymbol } from '../src/frontend/ast.js'
import * as ast from '../src/frontend/ast.js'
import { Module } from '../src/frontend/modules.js'
import { Def } from '../src/dwarf/index.js'

test('parse simple function definition', () => {
  const tree = parse('test', 'def foo(x) { while (true) { println(1 + 2) } }')
  assert.ok(tree, 'Parser should return an ast')
  const treeString = `${tree}`
  assert.ok(treeString.includes('foo'))
  assert.ok(treeString.includes('while'))
  assert.ok(treeString.includes('println'))
})

test('raw string literals', () => {
  const escaped = parse('test', '"\\n"')[0]
  const raw = parse('test', '`\\n`')[0]
  assert.equal(ast.asToken(escaped).unwrap(), '\n')
  assert.equal(ast.asToken(raw).unwrap(), '\\n')
})

test('raw string extended delimiter', () => {
  const src = "\\`a backtick ` inside`\\"
  const tree = parse('test', src)[0]
  assert.equal(ast.asToken(tree).unwrap(), 'a backtick ` inside')
})

test('escaped string extended delimiter', () => {
  const src = String.raw`\\"a quote " a newline \\n a backslash \n"\\`
  const tree = parse('test', src)[0]
  assert.equal(ast.asToken(tree).unwrap(), 'a quote " a newline \n a backslash \\n')
})

test('precedence table transitivity', () => {
  function transitive(t: PrecTable): boolean {
    let trans = true
    const N = t.ops.size
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
  return lowerfn(tag(''), sig, body, resolver, Def('test'))
}

test('lower simple function', () => {
  const ir = lower('fn foo(x) { x + 1 }')
  assert.equal(ir.toString(), `Function test at undefined
1: (%1)
  %2 = pack tag"common.List", %1, 1
  %3 = global tag"".+
  %4 = call %3, %2 # test:1:14 🔴
  %5 = call Method(tag"common.core.part"), %4, 1 # test:1:14
  %6 = return %5`)
})

test('lower control flow', () => {
  const ir = lower('fn test(x) { if x > 0 { x + 1 } else { x - 1 } }')
  assert.equal(ir.toString(), `Function test at undefined
1: (%1)
  %2 = pack tag"common.List", %1, 0
  %3 = global tag"".>
  %4 = call %3, %2 # test:1:18 🔴
  %5 = call Method(tag"common.core.part"), %4, 1 # test:1:18
  %6 = pack tag"common.List", %5
  %7 = call tag"common.condition", %6
  %8 = call Method(tag"common.core.part"), %7, 1
  %9 = br 2 if %8
  %10 = br 3
2:
  %11 = pack tag"common.List", %1, 1
  %12 = global tag"".+
  %13 = call %12, %11 # test:1:26 🔴
  %14 = call Method(tag"common.core.part"), %13, 1 # test:1:26
  %15 = br 4 (%14)
3:
  %16 = pack tag"common.List", %1, 1
  %17 = global tag"".-
  %18 = call %17, %16 # test:1:41 🔴
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
  assert.equal(ir.toString(), `Function test at undefined
1: (%1)
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
  %12 = br 4 (0)
4: (%13)
  %14 = return %13`)
})

test('lower while loop', () => {
  const ir = lower('fn loop(x) { while x > 0 { x = x - 1 }, return }')
  assert.equal(ir.toString(), `Function test at undefined
1: (%1)
  %2 = br 2 (%1)
2: (%3)
  %4 = pack tag"common.List", %3, 0
  %5 = global tag"".>
  %6 = call %5, %4 # test:1:21 🔴
  %7 = call Method(tag"common.core.part"), %6, 1 # test:1:21
  %8 = pack tag"common.List", %7 # test:1:14
  %9 = call tag"common.condition", %8 # test:1:14
  %10 = call Method(tag"common.core.part"), %9, 1 # test:1:14
  %11 = br 3 if %10 # test:1:14
  %12 = br 4 # test:1:14
3:
  %13 = pack tag"common.List", %3, 1
  %14 = global tag"".-
  %15 = call %14, %13 # test:1:33 🔴
  %16 = call Method(tag"common.core.part"), %15, 1 # test:1:33
  %17 = br 2 (%16)
4:
  %18 = return pack(tag"common.Nil")`)
})

test('lower toplevel expression', () => {
  const mod = new Module(tag('test'))
  mod.set('x', Type(42))
  const expr = parse('test', '{ x = x+1, y = y+1 }')[0]
  const [ir, _] = lower_toplevel(mod, expr, x => { throw new Error('nop') }, Def('common.core.main'))
  assert.equal(ir.toString(), `Function common.core.main at undefined
1:
  %1 = global tag"test".x
  %2 = pack tag"common.List", %1, 1
  %3 = global tag"test".+
  %4 = call %3, %2 # test:1:8 🔴
  %5 = call Method(tag"common.core.part"), %4, 1 # test:1:8
  %6 = global tag"test".y
  %7 = pack tag"common.List", %6, 1
  %8 = global tag"test".+
  %9 = call %8, %7 # test:1:17 🔴
  %10 = call Method(tag"common.core.part"), %9, 1 # test:1:17
  %11 = set tag"test".x, %5
  %12 = return pack(tag"common.Nil")`)
})

test('lower function with swap pattern', () => {
  const ir = lower('fn swap(&x, &y) { [x, y] = [y, x], return }')
  assert.equal(ir.toString(), `Function test at undefined
1: (%1, %2)
  %3 = pack tag"common.List", %2, %1 # test:1:28
  %4 = pack tag"common.List", %3, pack(tag"common.Pack", pack(tag"common.Literal", tag"common.List"), pack(tag"common.Bind", tag"x", pack(tag"common.Hole")), pack(tag"common.Bind", tag"y", pack(tag"common.Hole")))
  %5 = call tag"common.match", %4
  %6 = call Method(tag"common.core.part"), %5, 1
  %7 = call Method(tag"common.core.nil?"), %6
  %8 = br 2 if %7
  %9 = br 3
2:
  %10 = "match failed: [x, y]"
  %11 = pack tag"common.List", %10
  %12 = call tag"common.abort", %11
  %13 = call Method(tag"common.core.part"), %12, 1
3:
  %14 = call Method(tag"common.core.notnil"), %6
  %15 = pack tag"common.List", %14, tag"x"
  %16 = call tag"common.getkey", %15
  %17 = call Method(tag"common.core.part"), %16, 1
  %18 = pack tag"common.List", %14, tag"y"
  %19 = call tag"common.getkey", %18
  %20 = call Method(tag"common.core.part"), %19, 1
  %21 = pack tag"common.List", pack(tag"common.Nil"), %17, %20
  %22 = return %21`)
})

test('lower list construction', () => {
  const ir = lower('fn test(x, y) { [x, y, 1] }')
  assert.equal(ir.toString(), `Function test at undefined
1: (%1, %2)
  %3 = pack tag"common.List", %1, %2, 1 # test:1:17
  %4 = return %3`)
})

test('lower array indexing', () => {
  const ir = lower('fn test(arr, i) { arr[i] }')
  assert.equal(ir.toString(), `Function test at undefined
1: (%1, %2)
  %3 = pack tag"common.List", %1, %2 # test:1:22
  %4 = call tag"common.get", %3 # test:1:22 🔴
  %5 = call Method(tag"common.core.part"), %4, 1 # test:1:22
  %6 = return %5`)
})

test('lower template tag', () => {
  const ir = lower('fn test() { tag"hello.world" }')
  assert.equal(ir.toString(), `Function test at undefined
1:
  %1 = return tag"hello.world"`)
})

test('lower template bits', () => {
  const ir = lower('fn test() { bits"101" }')
  assert.equal(ir.toString(), `Function test at undefined
1:
  %1 = return bits"101"`)
})

test('lower for loop', () => {
  const ir = lower('fn iter(xs) { for x = xs { println(x) }, return }')
  assert.equal(ir.toString(), `Function test at undefined
1: (%1)
  %2 = pack tag"common.List", %1
  %3 = call tag"common.iterator", %2
  %4 = call Method(tag"common.core.part"), %3, 1
  %5 = br 2 (%4)
2: (%6)
  %7 = global tag"".true
  %8 = pack tag"common.List", %7
  %9 = call tag"common.condition", %8
  %10 = call Method(tag"common.core.part"), %9, 1
  %11 = br 3 if %10
  %12 = br 6
3:
  %13 = pack tag"common.List", %6
  %14 = call tag"common.next", %13
  %15 = call Method(tag"common.core.part"), %14, 1
  %16 = call Method(tag"common.core.part"), %14, 2
  %17 = pack tag"common.List", %15
  %18 = global tag"".nil?
  %19 = call %18, %17
  %20 = call Method(tag"common.core.part"), %19, 1
  %21 = pack tag"common.List", %20
  %22 = call tag"common.condition", %21
  %23 = call Method(tag"common.core.part"), %22, 1
  %24 = br 4 if %23
  %25 = br 5
4:
  %26 = br 6
5:
  %27 = pack tag"common.List", tag"common.Nil"
  %28 = global tag"".pack
  %29 = call %28, %27
  %30 = call Method(tag"common.core.part"), %29, 1
  %31 = pack tag"common.List", %15
  %32 = call tag"common.core.notnil", %31
  %33 = call Method(tag"common.core.part"), %32, 1
  %34 = pack tag"common.List", %33, 1
  %35 = call tag"common.core.part", %34
  %36 = call Method(tag"common.core.part"), %35, 1
  %37 = pack tag"common.List", %36 # test:1:35
  %38 = global tag"".println
  %39 = call %38, %37 # test:1:35 🔴
  %40 = call Method(tag"common.core.part"), %39, 1 # test:1:35
  %41 = br 2 (%16)
6:
  %42 = return pack(tag"common.Nil")`)
})

test.run()
