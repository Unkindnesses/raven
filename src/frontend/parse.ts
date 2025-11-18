import * as ast from './ast.js'
import { some, only } from '../utils/map.js'
import { binding } from '../utils/options.js'

export { Prec, PrecTable, table, inverse, parse, expr }

const [withPath, path] = binding<string>('path')

function curstring(c: ast.Cursor) { return `${c.line}:${c.column}` }

// IO

type Position = [number, number, number] // [i, line, col]

class Reader {
  i = 0; line = 1; col = 1
  constructor(private src: string) { }
  eof() { return this.i >= this.src.length }
  cursor(): ast.Cursor { return { line: this.line, column: this.col } }
  get peek() { return some(this.src[this.i]) }
  read() {
    const c = some(this.src[this.i++])
    if (c === '\n') { this.line++; this.col = 1 } else this.col++
    return c
  }
  mark(): Position { return [this.i, this.line, this.col] }
  reset(p: Position) { [this.i, this.line, this.col] = p }

  parse<T>(...fs: ((r: Reader) => T)[]): T | undefined {
    if (this.eof()) return
    const p = this.mark()
    for (const f of [...fs]) {
      const result = f(this)
      if (result !== undefined) return result
      this.reset(p)
    }
  }

  skipLine() { while (!this.eof() && this.peek !== '\n') this.read() }

  // Skip whitespace, not including newlines.
  skipWhitespace() {
    while (!this.eof()) {
      const c = this.peek
      if (c === ' ' || c === '\t' || c === '\r') this.read()
      else if (c === '#') this.skipLine()
      else break
    }
  }

  // Skip whitespace and `,` to get to the next statement.
  skip() {
    while (!this.eof()) {
      const c = this.read()
      if (c === '#') { this.skipLine(); continue }
      if (!' \t\r,\n'.includes(c)) { this.i--; this.col--; break }
    }
  }
}

// Precedence Table

enum Prec { Left = 1, Right = -1, None = 0 }

function inverse(p: Prec): Prec { return -p as Prec }

class PrecTable {
  readonly ops = new Map<string, number>()
  readonly table: Prec[][]

  constructor(os: string[]) {
    os.forEach((op, i) => this.ops.set(op, i))
    this.table = Array.from({ length: os.length }, () => Array(os.length).fill(Prec.None))
  }
  get(a: string, b: string) { return this.table[this.ops.get(a)!][this.ops.get(b)!] }
  set(a: string, b: string, p: Prec) {
    const i = this.ops.get(a)!, j = this.ops.get(b)!
    const cur = this.table[i][j]
    if (cur !== Prec.None && cur !== p) console.warn(`overwriting precedence for ${a}, ${b}`)
    this.table[i][j] = p
    this.table[j][i] = inverse(p)
  }
  precedence(...ops: string[]) {
    for (let k = 0; k + 1 < ops.length; ++k) this.set(ops[k], ops[k + 1], Prec.Left)
  }
  closure() {
    const n = this.ops.size
    for (let i = 0; i < n; ++i)
      for (let j = 0; j < n; ++j)
        for (let k = 0; k < n; ++k) {
          const ab = this.table[i][j], bc = this.table[j][k]
          if (ab !== Prec.None && ab === bc) {
            this.table[i][k] = ab
            this.table[k][i] = inverse(ab)
          }
        }
  }
}

// Tokens

function exact(r: Reader, s: string): string | undefined {
  for (const c of s)
    if (r.eof() || r.read() !== c) return
  return s
}

function num(r: Reader): number | bigint | undefined {
  let num = '', float = false
  if (!/\d|\./.test(r.peek)) return
  while (!r.eof()) {
    const c = r.peek
    if (/\d/.test(c)) { num += r.read(); continue }
    if (c === '.') {
      if (float) throw new Error('invalid number')
      float = true
      num += r.read()
    } else break
  }
  if (num === '' || num === '.') return
  return float ? parseFloat(num) : BigInt(parseInt(num, 10))
}

function hex(r: Reader) {
  if (!(r.parse(r => exact(r, '0x')))) return
  let num = ''
  while (!r.eof() && /[0-9a-fA-F]/.test(r.peek)) num += r.read()
  if (num === '') throw new Error('invalid hex literal')
  return ast.Template(ast.symbol('hex'), num)
}

function negnum(r: Reader) {
  if (r.read() !== '-') return
  const x = num(r)
  if (x === undefined) return
  return -x
}

function number(r: Reader) {
  return r.parse<ast.Expr | bigint | number | undefined>(hex, negnum, num)
}

function symbol(r: Reader): ast.Symbol | undefined {
  let s = ''
  if (!/[A-Za-z_]/.test(r.peek)) return
  s += r.read()
  while (!r.eof() && /[A-Za-z0-9_!?]/.test(r.peek)) s += r.read()
  return ast.symbol(s)
}

const operators = [
  "=", "==", "!=", "+", "-", "*", "/", "^", ">", "<", ">=", "<=", ":", "&",
  "|", "|>", "&&", "||"
]
const opChars = [...new Set(operators.join(''))].join('')

function opsymbol(r: Reader): ast.Symbol | undefined {
  let s = ''
  while (!r.eof() && opChars.includes(r.peek)) s += r.read()
  if (!operators.includes(s)) return
  return ast.symbol(s)
}

// TODO unicode escapes
const escapes = new Map([
  ['0', '\0'],
  ['t', '\t'],
  ['n', '\n'],
  ['r', '\r'],
  ['"', '\"'],
  ["'", '\''],
  ['\\', '\\'],
])

function string(r: Reader): string | undefined {
  let slashes = 0
  while (!r.eof() && r.peek === '\\') { r.read(); slashes++ }
  if (r.eof()) return
  const open = r.read()
  if (open !== '"' && open !== '`') return
  const raw = open === '`'
  const escape = '\\'.repeat(raw ? 0 : Math.max(1, slashes))
  let s = ''
  while (!r.eof()) {
    if (r.parse(r => exact(r, open + '\\'.repeat(slashes))) !== undefined) return s
    if (!raw && r.parse(r => exact(r, escape))) {
      s += some(escapes.get(r.read()))
      continue
    }
    s += r.read()
  }
  throw new Error('unterminated string')
}

// Parsing

function swap(r: Reader): ast.Tree | undefined {
  const pos = r.cursor()
  if (r.read() !== '&') return
  const name = symbol(r)
  if (name === undefined) return
  return ast.Swap(name).withmeta({ file: path(), loc: pos })
}

function template(r: Reader): ast.Tree | undefined {
  const name = symbol(r)
  if (name === undefined || r.eof()) return
  const str = string(r)
  if (str === undefined) return
  return ast.Template(name, str)
}

function brackets(r: Reader, open: string, close: string): ast.Tree[] | undefined {
  if (r.read() !== open) return
  const xs: ast.Tree[] = []
  while (true) {
    r.skip()
    if (r.peek === close) break
    xs.push(some(statement(r)))
  }
  r.read()
  return xs
}

function bracketsTo<T>(r: Reader, open: string, close: string, f: (...xs: ast.Tree[]) => T): T | undefined {
  const xs = brackets(r, open, close)
  if (xs === undefined) return
  return f(...xs)
}

function group(r: Reader) { return bracketsTo(r, '(', ')', ast.Group) }
function list(r: Reader) { return bracketsTo(r, '[', ']', ast.List) }
function block(r: Reader) { return bracketsTo(r, '{', '}', ast.Block) }

// Combine all simple expressions with little backtracking
function item(r: Reader): ast.Tree {
  r.skipWhitespace()
  const pos = r.cursor()
  const ex = r.parse<ast.Tree | ast.Atom | undefined>(template, symbol, swap, string, number, opsymbol, group, list, block)
  if (ex === undefined) throw new Error(`unexpected character ${r.peek} at ${curstring(pos)}`)
  const tree = ast.isAtom(ex) ? new ast.Token(ex) : ex
  return tree.withmeta({ file: path(), loc: pos })
}

// The following parsers fall back to simpler ones, to avoid excessive
// backtracking / re-parsing. So they don't need to be called in sequence.

// Does calls and fields, so we can handle eg `foo.bar(a).baz`
function call(r: Reader): ast.Tree {
  let ex = item(r)
  while (true) {
    const cur = r.cursor()
    let args = r.parse(r => brackets(r, '(', ')'))
    if (args !== undefined) {
      ex = ast.Call(ex, ...args).withmeta({ file: path(), loc: cur })
      continue
    }
    args = r.parse(r => brackets(r, '[', ']'))
    if (args !== undefined) {
      ex = ast.Index(ex, ...args).withmeta({ file: path(), loc: cur })
      continue
    }
    const m = r.mark()
    if (r.parse(r => exact(r, '.'))) {
      if (r.peek === '.') { r.reset(m); break }
      const field = item(r) // TODO disallow whitespace
      ex = ast.Field(ex, field).withmeta({ file: path(), loc: cur })
      continue
    }
    break
  }
  return ex
}

const table = new PrecTable(operators)
table.precedence('^', '/', '*', '+', '-', '=');
['/', '*', '+', '-', '|', '&'].forEach(op => table.set(op, op, Prec.Left))
table.closure()

function precedence(a: ast.Symbol, b: ast.Symbol): Prec {
  return table.get(a.toString(), b.toString())
}

function operator(r: Reader, syn = true, prev?: ast.Symbol): ast.Tree {
  let left = call(r)
  while (true) {
    const cur = r.cursor()
    const mark = r.mark()
    r.skipWhitespace()
    const op = r.parse(opsymbol)
    if (op === undefined) return left
    const prec = prev ? precedence(prev, op) : Prec.Right
    if (prec === Prec.Left) { r.reset(mark); return left }
    if (prec === Prec.None) { throw new Error(`Operators ${prev} and ${op} are ambiguous at ${curstring(r.cursor())}`) }
    r.skip()
    let right = syn ? r.parse(syntax) : undefined // TODO parse needed?
    if (right === undefined) right = operator(r, syn, op)
    left = ast.Operator(op, left, right).withmeta({ file: path(), loc: cur })
  }
}

function splat(r: Reader, syn = true): ast.Tree {
  let ex = operator(r, syn)
  r.skipWhitespace()
  if (r.parse(r => exact(r, '...'))) ex = ast.Splat(ex)
  return ex
}

// Syntax blocks
// TODO try to avoid as much re-parsing as possible.
const terminators = new Set(['}', ')', ']', ',', '\n'])

function syntax(r: Reader): ast.Tree {
  const pos = r.cursor()
  const name = splat(r)
  if (!(name.unwrap() instanceof ast.Symbol)) return name
  const args: ast.Tree[] = []
  while (!r.eof()) {
    r.skipWhitespace()
    if (terminators.has(r.peek)) break
    // `syn` fixes eg `fn x + y {}`, where `y {}` would be
    // parsed as an argument to `+` otherwise.
    const arg = splat(r, false)
    args.push(arg)
  }
  if (!args.length) return name
  return ast.Syntax(name, ...args).withmeta({ file: path(), loc: pos })
}

function anno(r: Reader): ast.Tree | undefined {
  const pos = r.cursor()
  if (r.read() !== '@') return
  const name = symbol(r)
  if (name === undefined) return
  const args: ast.Tree[] = []
  while (!r.eof()) {
    r.skipWhitespace()
    if (terminators.has(r.peek)) break
    const arg = splat(r, false)
    args.push(arg)
  }
  r.skip()
  let body = r.parse(anno, syntax)!
  return ast.Annotation(name, ...args, body).withmeta({ file: path(), loc: pos })
}

function statement(r: Reader): ast.Tree | undefined {
  r.skip()
  if (r.eof()) return
  let ex = r.parse(anno, syntax)!
  r.skipWhitespace()
  if (!r.eof() && !terminators.has(r.peek)) throw new Error(`Expected statement end at ${curstring(r.cursor())}`)
  return ex
}

function parse(path: string, src: string): ast.Tree[] {
  return withPath(path, () => {
    let result: ast.Tree[] = []
    let r = new Reader(src)
    while (true) {
      r.skip()
      let next = statement(r)
      if (next === undefined) break
      result.push(next)
    }
    return result
  })
}

function expr(src: string): ast.Tree {
  return only(parse('', src))
}
