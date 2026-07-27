import { test } from 'vitest'
import * as assert from 'assert'
import { parse, expr, PrecTable, Prec, inverse, table } from '../src/frontend/parse.js'
import { lowerfn, lower_toplevel } from '../src/frontend/lower.js'
import { callpattern } from '../src/frontend/lower.js'
import { tag, Type } from '../src/frontend/types.js'
import { asSymbol } from '../src/frontend/ast.js'
import * as ast from '../src/frontend/ast.js'
import { MethodKey, Modules } from '../src/frontend/modules.js'
import { Def } from '../src/dwarf/index.js'

const parsed = (src: string, file = 'test') => parse(file, src).args
const first = (src: string, file = 'test') => parsed(src, file)[0]

test('parse simple function definition', () => {
  const tree = parse('test', 'def foo(x) { while (true) { println(1 + 2) } }')
  assert.ok(tree, 'Parser should return an ast')
  const treeString = `${tree}`
  assert.ok(treeString.includes('foo'))
  assert.ok(treeString.includes('while'))
  assert.ok(treeString.includes('println'))
})

test('raw string literals', () => {
  const escaped = first('"\\n"')
  const raw = first('`\\n`')
  assert.equal(ast.asToken(escaped).unwrap(), '\n')
  assert.equal(ast.asToken(raw).unwrap(), '\\n')
})

test('raw string extended delimiter', () => {
  const src = "\\`a backtick ` inside`\\"
  const tree = first(src)
  assert.equal(ast.asToken(tree).unwrap(), 'a backtick ` inside')
})

test('escaped string extended delimiter', () => {
  const src = String.raw`\\"a quote " a newline \\n a backslash \n"\\`
  const tree = first(src)
  assert.equal(ast.asToken(tree).unwrap(), 'a quote " a newline \n a backslash \\n')
})

test('triple-quoted string basic', () => {
  const src = '"""hello world"""'
  const tree = first(src)
  assert.equal(ast.asToken(tree).unwrap(), 'hello world')
})

test('triple-quoted string multiline', () => {
  const src = `"""
    hello
    world
    """`
  const tree = first(src)
  assert.equal(ast.asToken(tree).unwrap(), 'hello\nworld')
})

test('escaped newlines', () => {
  const src = `"""
    hello \\
    world
    """`
  const tree = first(src)
  assert.equal(ast.asToken(tree).unwrap(), 'hello world')
})

test('escaped newline preserves following indent', () => {
  const src = `"""
  hello\\n  world
  """`
  const tree = first(src)
  assert.equal(ast.asToken(tree).unwrap(), 'hello\n  world')
})

test('triple-quoted string preserves relative indent', () => {
  const src = `"""
    line1
      indented
    line2
    """`
  const tree = first(src)
  assert.equal(ast.asToken(tree).unwrap(), 'line1\n  indented\nline2')
})

test('triple-quoted string with embedded quotes', () => {
  const src = '"""say "hello" to the world"""'
  const tree = first(src)
  assert.equal(ast.asToken(tree).unwrap(), 'say "hello" to the world')
})

test('triple-quoted string escape sequences', () => {
  const src = '"""hello\\nworld"""'
  const tree = first(src)
  assert.equal(ast.asToken(tree).unwrap(), 'hello\nworld')
})

test('triple-quoted raw string', () => {
  const src = '\`\`\`hello\\nworld\`\`\`'
  const tree = first(src)
  assert.equal(ast.asToken(tree).unwrap(), 'hello\\nworld')
})

test('triple-quoted string extended delimiter', () => {
  const src = String.raw`\\"""contains """ triple quotes"""\\`
  const tree = first(src)
  assert.equal(ast.asToken(tree).unwrap(), 'contains """ triple quotes')
})

test('triple-quoted escape with extended delimiter', () => {
  const src = String.raw`\\"""a newline \\n here"""\\`
  const tree = first(src)
  assert.equal(ast.asToken(tree).unwrap(), 'a newline \n here')
})

test('triple-quoted tagged template', () => {
  const src = `"""js
  console.log("hello")
  """`
  const tree = first(src)
  assert.ok(ast.isExpr(tree, 'Template'))
  assert.equal(asSymbol(tree.args[0].unwrap()).toString(), 'js')
  assert.equal(ast.asToken(tree.args[1]).unwrap(), 'console.log("hello")')
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

test('non-associative operators are parse errors', () => {
  assert.throws(() => expr('1 == 2 == 3'), /ambiguous/)
})

test('numeric literal separators', () => {
  assert.equal(ast.asToken(expr('1_000')).unwrap(), 1000n)
  assert.equal(ast.asToken(expr('1_000.25_5')).unwrap(), 1000.255)
  assert.equal(String(expr('-1_000')), '(-1000)')
  assert.equal(String(expr('0xCAFE_BABE')), '0xCAFEBABE')
})

test('invalid numeric literal separators', () => {
  assert.throws(() => expr('1__000'), /Expected statement end/)
  assert.throws(() => expr('1_'), /Expected statement end/)
  assert.throws(() => expr('1_.0'), /Expected statement end/)
})

test('comparison binds tighter than logical and', () => {
  assert.equal(String(expr('true == true && false')), '((true == true) && false)')
})

test('prefix negation binds tighter than infix operators', () => {
  assert.equal(String(expr('-x * y')), '((-x) * y)')
  assert.equal(String(expr('-(x + y)')), '(-((x + y)))')
})

// Round-trip

test('trivia ownership', () => {
  const [lineTrail, lineLead] = parsed('a, # foo\nb')
  assert.equal(lineTrail.trivia.trailing, ', # foo\n')
  assert.equal(lineLead.trivia.leading, '')

  const [sameLineTrail, sameLineLead] = parsed('a,  b')
  assert.equal(sameLineTrail.trivia.trailing, ',')
  assert.equal(sameLineLead.trivia.leading, '  ')

  const [newlineTrail, newlineLead] = parsed('a,  \nb')
  assert.equal(newlineTrail.trivia.trailing, ',  \n')
  assert.equal(newlineLead.trivia.leading, '')

  const file = parse('test', 'x\n# eof')
  assert.equal(file.head, 'File')
  assert.equal(file.trivia.inner, '# eof')

  const operator = ast.asExpr(first('  x * y  \n'), 'Operator')
  const left = operator.args[0]
  const right = operator.args[2]
  assert.equal(operator.trivia.leading, '  ')
  assert.equal(left.trivia.leading, '')
  assert.equal(operator.trivia.trailing, '  \n')
  assert.equal(right.trivia.trailing, '')

  const callOperator = ast.asExpr(first('x * f(y)  \n'), 'Operator')
  const call = ast.asExpr(callOperator.args[2], 'Call')
  assert.equal(callOperator.trivia.trailing, '  \n')
  assert.equal(call.trivia.trailing, '')

  const continued = ast.asExpr(first('x + # foo\n  y'), 'Operator')
  const continuedOperator = continued.args[1]
  const continuedRight = continued.args[2]
  assert.equal(continuedOperator.trivia.trailing, ' # foo\n')
  assert.equal(continuedRight.trivia.leading, '  ')

  const bracket = ast.asExpr(first('[ x * y  ]'), 'List')
  const bracketedOperator = ast.asExpr(bracket.args[0], 'Operator')
  assert.equal(bracket.trivia.leading, '')
  assert.equal(bracket.trivia.trailing, '')
  assert.equal(bracketedOperator.trivia.leading, ' ')
  assert.equal(bracketedOperator.trivia.trailing, '  ')
})

test('source extents', () => {
  const token = new ast.Token(ast.symbol('value'), 'foo\nbar')
  assert.deepEqual(token.extent, [1, 3])

  const list = ast.List(token)
  assert.deepEqual(list.extent, [1, 4])

  const withTrivia = ast.trailing(ast.leading(list, '\n  '), '\n')
  assert.deepEqual(withTrivia.extent, [3, 0])

  const source = '  x + # note\n  y,\n'
  assert.deepEqual(first(source).extent, [2, 0])
  assert.deepEqual(parse('test', source).extent, [2, 0])
})

test('traversal locations account for replaced siblings', () => {
  const tree = ast.List(
    new ast.Token(ast.symbol('x')),
    ast.leading(new ast.Token(ast.symbol('y')), ' ')
  )
  const locations: ast.Cursor[] = []
  const out = new ast.Traverse(tree).map((child, index) => {
    locations.push(child.loc)
    return index === 0 ? new ast.Token(ast.symbol('long')) : child.node
  })

  assert.deepEqual(locations, [{ line: 1, column: 2 }, { line: 1, column: 7 }])
  assert.equal(ast.print(out), '[long y]')
})

const roundtrips = (src: string) => assert.equal(ast.print(parse('test', src)), src)

test('round-trip statements', () => {
  roundtrips('')
  roundtrips('x')
  roundtrips('a = 1')
  roundtrips('a = 1, b = 2')
  roundtrips('# lead\na = 1 # note\n\nb = 2\n')
  roundtrips('x\n# trailing comment lines\n# at eof')
})

test('round-trip brackets', () => {
  roundtrips('[1, 2, 3] # foo')
  roundtrips('[\n  1,\n  2,\n]')
  roundtrips('{ # TODO\n}')
  roundtrips('()')
  roundtrips('f( a , b )[ 1 ]')
  roundtrips('fn foo(x) { while (true) { println(1 + 2) } }')
})

test('round-trip operators', () => {
  roundtrips('a  +  b')
  roundtrips('a +\n  b')
  roundtrips('x = -y + !z')
  roundtrips('xs = [a..., b ...]')
  roundtrips('foo.bar(a).baz')
})

test('round-trip tokens', () => {
  roundtrips('x = 0xFF + 1_000 - 1.5 + 1.')
  roundtrips('s = "a\\nb"')
  roundtrips('r = `raw \\n`')
  roundtrips('t = js"code" ')
  roundtrips('u = """\n  hello\n  """')
  roundtrips('v = """tag\n  hello\n  """')
  roundtrips('w = \\"a quote " here"\\')
})

test('round-trip attributes', () => {
  roundtrips('@inline x\ndef f(a) { a }')
  roundtrips('@inline x, def f(a) { a }')
})

import { readFileSync, readdirSync } from 'node:fs'

test('round-trip common', () => {
  const files = readdirSync('common', { recursive: true, encoding: 'utf8' })
  for (const f of files.filter(f => f.endsWith('.rv'))) {
    const src = readFileSync(`common/${f}`, 'utf8')
    assert.equal(ast.print(parse(f, src)), src, `round-trip failed for ${f}`)
  }
})

// Lowering

function lower(def: string) {
  const ex = first(def)
  if (!ast.isSyntax(ex, 'fn'))
    throw new Error('Expected function definition starting with "fn"')
  const signature = ast.asExpr(ex.args[1], 'Call')
  const fn = tag(asSymbol(signature.args[0].unwrap()).toString())
  const params = signature.args.slice(1)
  const body = ex.args[2]
  const [sig] = callpattern(tag(''), fn, ast.List(fn, ...params))
  return lowerfn(new MethodKey(tag(''), fn), sig, body, Def('test'))
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
  const ir = lower('fn option(x) { if let Some(y) = x { y } else { 0 } }')
  assert.equal(ir.toString(), `Function test at undefined
1: (%1)
  %2 = pack tag"common.Hole"
  %3 = pack tag"common.Bind", tag"y", %2
  %4 = global tag"".Some
  %5 = pack tag"common.Constructor", %4, %3
  %6 = pack tag"common.List", %1, %5
  %7 = call tag"common.match", %6
  %8 = call Method(tag"common.core.part"), %7, 1
  %9 = call Method(tag"common.core.nil?"), %8
  %10 = br 3 if %9
  %11 = br 2
2:
  %12 = call Method(tag"common.core.notnil"), %8
  %13 = pack tag"common.List", %12, tag"y"
  %14 = call tag"common.getkey", %13
  %15 = call Method(tag"common.core.part"), %14, 1
  %16 = br 4 (%15)
3:
  %17 = br 4 (0)
4: (%18)
  %19 = return %18`)
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
  const sources = new Modules()
  const mod = sources.module(tag('test'))
  mod.set('x', Type(42))
  const expr = first('{ x = x+1, y = y+1 }')
  const [ir, _] = lower_toplevel(mod, expr, Def('common.core.main'))
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
  %4 = pack tag"common.Hole"
  %5 = pack tag"common.Bind", tag"x", %4
  %6 = pack tag"common.Hole"
  %7 = pack tag"common.Bind", tag"y", %6
  %8 = pack tag"common.Literal", tag"common.List"
  %9 = pack tag"common.Pack", %8, %5, %7
  %10 = pack tag"common.List", %3, %9
  %11 = call tag"common.match", %10
  %12 = call Method(tag"common.core.part"), %11, 1
  %13 = call Method(tag"common.core.nil?"), %12
  %14 = br 2 if %13
  %15 = br 3
2:
  %16 = "match failed: [x, y]"
  %17 = pack tag"common.List", %16
  %18 = call tag"common.abort", %17
  %19 = call Method(tag"common.core.part"), %18, 1
3:
  %20 = call Method(tag"common.core.notnil"), %12
  %21 = pack tag"common.List", %20, tag"x"
  %22 = call tag"common.getkey", %21
  %23 = call Method(tag"common.core.part"), %22, 1
  %24 = pack tag"common.List", %20, tag"y"
  %25 = call tag"common.getkey", %24
  %26 = call Method(tag"common.core.part"), %25, 1
  %27 = pack tag"common.List", pack(tag"common.Nil"), pack(tag"common.Nil"), %23, %26
  %28 = return %27`)
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
  %3 = pack tag"common.List", %2
  %4 = pack tag"common.List", %1, %3 # test:1:22
  %5 = call tag"common.get", %4 # test:1:22 🔴
  %6 = call Method(tag"common.core.part"), %5, 1 # test:1:22
  %7 = return %6`)
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
  %3 = call tag"common.iterate", %2
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
  %16 = call Method(tag"common.core.part"), %14, 3
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
