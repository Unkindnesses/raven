import * as fs from 'node:fs/promises'
import * as os from 'node:os'
import * as path from 'node:path'
import assert from 'node:assert'
import { afterEach, test } from 'vitest'
import * as ast from '../src/frontend/ast.js'
import { fmt, formatDiff } from '../src/cli/fmt.js'
import { indent, format, commas, syntaxArgs, operators, brackets, trailingWhitespace } from '../src/frontend/format.js'
import { parse } from '../src/frontend/parse.js'

const fixtures: string[] = []

async function fixture(): Promise<string> {
  const dir = await fs.mkdtemp(path.join(os.tmpdir(), 'raven-fmt-'))
  fixtures.push(dir)
  return dir
}

async function collectChanges(inputs: string[]) {
  const changes = []
  for await (const change of fmt(inputs)) changes.push(change)
  return changes
}

afterEach(async () => {
  await Promise.all(fixtures.splice(0).map(dir => fs.rm(dir, { recursive: true, force: true })))
})

test('format parses and prints the CST', () => {
  const source = '# hello\nf(a, b)\n'
  assert.equal(format('test.rv', source), source)
})

test('end file with a single newline owned by the final statement', () => {
  assert.equal(format('test.rv', ''), '')
  assert.equal(format('test.rv', '  \n\n'), '')
  assert.equal(format('test.rv', 'x'), 'x\n')
  assert.equal(format('test.rv', 'x\n\n'), 'x\n')
  assert.equal(format('test.rv', 'x\r\n\r\n'), 'x\r\n')

  const file = parse('test.rv', format('test.rv', 'x\n  \n'))
  assert.equal(file.args.at(-1)?.trivia.trailing, '\n')
  assert.equal(file.trivia.inner, '')
})

test('end trailing comments with a newline owned by the file', () => {
  const formatted = format('test.rv', 'x\n# trailing comment\n\n')
  assert.equal(formatted, 'x\n# trailing comment\n')
  assert.equal(format('test.rv', '# only comment'), '# only comment\n')

  const file = parse('test.rv', formatted)
  assert.equal(file.args.at(-1)?.trivia.trailing, '\n')
  assert.equal(file.trivia.inner, '# trailing comment\n')
})

test('trim trailing whitespace', () => {
  const source = '# hello  \n  \nf(  \n  a,\t\n) \t\nx = 1  \ny = 2\t\n'
  const tree = parse('test.rv', source)
  const formatted = trailingWhitespace(tree)

  assert.notEqual(formatted, tree)
  assert.equal(ast.print(tree), source)
  assert.equal(ast.print(formatted), '# hello\n\nf(\n  a\n)\nx = 1\ny = 2\n')
})

test('preserve commas at the end of comments', () => {
  const source = '# standalone,\nx # trailing,\nf(\n  a, # item,\n  b,\n)\n'
  const expected = '# standalone,\nx # trailing,\nf(\n  a # item,\n  b\n)\n'
  assert.equal(format('test.rv', source), expected)
})

test('normalize space before trailing comments', () => {
  const source = '# standalone\nx# none\ny   # spaces\nz\t# tab\nf(a,# comma\nb)\nfn f() {# opening\nx\n}\n'
  const expected = '# standalone\nx # none\ny # spaces\nz # tab\nf(a # comma\n  b)\nfn f() { # opening\n  x\n}\n'
  assert.equal(format('test.rv', source), expected)
})

test('normalize trailing commas', () => {
  const source = 'f(a,b,)\nf(\n  a, # keep, comment\n  b,\n)'
  const tree = parse('test.rv', source)
  const formatted = commas(tree)

  assert.notEqual(formatted, tree)
  assert.equal(ast.print(tree), source)
  assert.equal(ast.print(formatted), 'f(a, b)\nf(\n  a # keep, comment\n  b\n)')
})

test('normalize inline commas', () => {
  const source = 'f(  a ,   b)\nxs[ a ,b]\n[  a,b]\n[a,b]\n(  a ,b)\n(a,b)\n'
  const tree = parse('test.rv', source)
  const formatted = commas(tree)

  assert.notEqual(formatted, tree)
  assert.equal(ast.print(tree), source)
  assert.equal(ast.print(formatted), 'f(a, b)\nxs[a, b]\n[ a, b ]\n[a, b]\n( a, b )\n(a, b)\n')
})

test('remove inline trivia from empty brackets', () => {
  const source = 'f( )\nxs[  ]\n[ ]\n( \t)\nfn f() { }\n'
  const expected = 'f()\nxs[]\n[]\n()\nfn f() {}\n'
  assert.equal(ast.print(commas(parse('test.rv', source))), expected)
  assert.equal(ast.print(commas(parse('test.rv', 'f(\n)\n'))), 'f(\n)\n')
})

test('preserve multiline comma trivia', () => {
  const source = 'f( # opening\n  a, # next\n  b)\n[\n  a,\n  b]\n'
  const expected = 'f( # opening\n  a # next\n  b)\n[\n  a\n  b]\n'
  assert.equal(ast.print(commas(parse('test.rv', source))), expected)
})

test('normalize syntax args', () => {
  const source = 'fn   f()\t {}\nif\ttrue   { x }\nx = fn  (y)   { y }\n'
  const tree = parse('test.rv', source)
  const formatted = syntaxArgs(tree)

  assert.notEqual(formatted, tree)
  assert.equal(ast.print(tree), source)
  assert.equal(ast.print(formatted), 'fn f() {}\nif true { x }\nx = fn (y) { y }\n')
  assert.equal(format('test.rv', source), 'fn f() {}\nif true { x }\nx = fn (y) { y }\n')
})

test('normalize binary operators', () => {
  const source = 'x*y\nx  +\ty\nx= -y\nx :y\nx + # keep\n  y\n'
  const tree = parse('test.rv', source)
  const formatted = operators(tree)

  assert.notEqual(formatted, tree)
  assert.equal(ast.print(tree), source)
  assert.equal(ast.print(formatted), 'x * y\nx + y\nx = -y\nx: y\nx + # keep\n  y\n')
})

test('normalize trailing brackets', () => {
  const source = 'f(\na)\ng(b\n)\n[\nc]\n[d\n]\n'
  const tree = parse('test.rv', source)
  const formatted = brackets(tree)

  assert.notEqual(formatted, tree)
  assert.equal(ast.print(tree), source)
  assert.equal(ast.print(formatted), 'f(\na\n)\ng(\nb\n)\n[\nc\n]\n[\nd\n]\n')
})

test('match closing spaces to opening spaces', () => {
  const source = '[ a, b]\n( c)\nfn f() { x}\n[[ y]]\n'
  const expected = '[ a, b ]\n( c )\nfn f() { x }\n[[ y ]]\n'
  assert.equal(format('test.rv', source), expected)
})

test('normalize and indent all trailing bracket types', () => {
  const source = 'f(\na)\n[b\n]\n(\nc)\nxs[d\n]\nfn f() {\nx}\nfn g() {y\n}\n'
  const expected = 'f(\n  a\n)\n[\n  b\n]\n(\n  c\n)\nxs[\n  d\n]\nfn f() {\n  x\n}\nfn g() {\n  y\n}\n'
  assert.equal(format('test.rv', source), expected)
})

test('fix indentation of statements on their own line', () => {
  const source = '  x\nfn f() {\ny\n    if true {\n z\n  }\n}\n'
  const tree = parse('test.rv', source)
  const formatted = indent(tree)

  assert.notEqual(formatted, tree)
  assert.equal(ast.print(tree), source)
  assert.equal(ast.print(formatted), 'x\nfn f() {\n  y\n  if true {\n    z\n  }\n}\n')
})

test('indent binary operator continuations', () => {
  const source = 'x +\ny\nz + # keep\nw\nfn f() {\nx +\ny\n}\n'
  const expected = 'x +\n  y\nz + # keep\n  w\nfn f() {\n  x +\n    y\n}\n'
  assert.equal(ast.print(indent(parse('test.rv', source))), expected)
})

test('fix indentation of comments on their own line', () => {
  const source = ' # file\nfn f() { # opening\n # before\nif true {\n # nested\n}\nx # side\n    # after\n}\n  # eof\n'
  const expected = '# file\nfn f() { # opening\n  # before\n  if true {\n    # nested\n  }\n  x # side\n  # after\n}\n# eof\n'
  assert.equal(format('test.rv', source), expected)
  assert.equal(format('test.rv', 'fn f() {\n # only\n}\n'), 'fn f() {\n  # only\n}\n')
  assert.equal(format('test.rv', '  # only\n'), '# only\n')
})

test('indent all bracket types uniformly', () => {
  const source = 'f(\na,\n[\nb,\n],\n(\nc,\n),\n)[\nd,\n]\n'
  const expected = 'f(\n  a\n  [\n    b\n  ]\n  (\n    c\n  )\n)[\n  d\n]\n'
  assert.equal(format('test.rv', source), expected)
})

test('align continuations with an inline first list item', () => {
  const source = '1 + foo(a, b\nc, d)\nfn f() {\nfoo(a, b\nc, d)\n}\n'
  const expected = '1 + foo(a, b\n        c, d)\nfn f() {\n  foo(a, b\n      c, d)\n}\n'
  assert.equal(format('test.rv', source), expected)
})

test('align continuations after earlier formatting changes', () => {
  assert.equal(format('test.rv', 'f(a,b)(c,d\ne)\n'), 'f(a, b)(c, d\n        e)\n')
})

test('indent closing delimiters without statements', () => {
  const source = 'fn f() {\nif true {\n # only\n    }\nf(\n  )\n}\n'
  const expected = 'fn f() {\n  if true {\n    # only\n  }\n  f(\n  )\n}\n'
  assert.equal(format('test.rv', source), expected)
})

test('preserve whitespace in multiline strings', () => {
  const source = '"""\n  hello  \n  """  \n'
  assert.equal(format('test.rv', source), '"""\n  hello  \n  """\n')
})

test('preserve CRLF line endings', () => {
  assert.equal(format('test.rv', 'x  \r\ny\t\r\n'), 'x\r\ny\r\n')
  assert.equal(format('test.rv', 'fn f() {\r\nx\r\n    }\r\n'), 'fn f() {\r\n  x\r\n}\r\n')
})

test('fmt yields files with their source and formatted output', async () => {
  const dir = await fixture()
  const file = path.join(dir, 'main.rv')
  const source = '# hello  \nf( a , b )\t\n'
  await fs.writeFile(file, source)

  assert.deepEqual(await collectChanges([file]), [[file, source, '# hello\nf(a, b)\n']])
  assert.equal(await fs.readFile(file, 'utf8'), source)
})

test('fmt changes can be rendered as a diff', async () => {
  const dir = await fixture()
  const file = path.join(dir, 'main.rv')
  const source = 'fn f() {\nx  \n}\n'
  await fs.writeFile(file, source)

  const [[changed, original, formatted]] = await collectChanges([file])
  assert.equal(changed, file)
  assert.equal(formatDiff(path.basename(changed), original, formatted), [
    '--- main.rv',
    '+++ main.rv',
    '@@ -1,3 +1,3 @@',
    ' fn f() {',
    '-x  ',
    '+  x',
    ' }',
    ''
  ].join('\n'))
})

test('fmt recursively walks Raven files and skips other files', async () => {
  const dir = await fixture()
  await fs.mkdir(path.join(dir, 'nested'))
  await fs.writeFile(path.join(dir, 'main.rv'), 'x = 1\n')
  await fs.writeFile(path.join(dir, 'nested', 'lib.rv'), 'fn f() { 1 }\n')
  await fs.writeFile(path.join(dir, 'notes.txt'), '???')

  assert.deepEqual(await collectChanges([dir]), [])
  assert.deepEqual(await collectChanges([path.join(dir, 'notes.txt')]), [])
})

test('fmt reports parse errors in included Raven files', async () => {
  const dir = await fixture()
  const file = path.join(dir, 'bad.rv')
  await fs.writeFile(file, '???')
  await assert.rejects(collectChanges([dir]), /unexpected character/)
})
