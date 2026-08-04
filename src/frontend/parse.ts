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
  get char() { return some(this.src[this.i]) }
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

  peek<T>(...fs: ((r: Reader) => T)[]): T | undefined {
    if (this.eof()) return
    const p = this.mark()
    for (const f of [...fs]) {
      const result = f(this)
      this.reset(p)
      if (result !== undefined) return result
    }
  }

  some<T>(f: (r: Reader) => T | undefined): T {
    const result = f(this)
    if (result === undefined) throw new Error(`unexpected character '${this.char}' at ${path()}:${curstring(this.cursor())}`)
    return result
  }

  text(from: Position) { return this.src.slice(from[0], this.i) }

  skipLine() { while (!this.eof() && this.char !== '\n') this.read() }

  // Skip whitespace, not including newlines.
  skipWhitespace(): string {
    const from = this.mark()
    while (!this.eof()) {
      const c = this.char
      if (c === ' ' || c === '\t' || c === '\r') this.read()
      else if (c === '#') this.skipLine()
      else break
    }
    return this.text(from)
  }

  // End of statement trivia. Includes whitespace after a separator only
  // if the rest of the line is clear.
  skipTrailing(): string {
    const from = this.mark()
    let afterComma: Position | undefined
    while (!this.eof()) {
      const c = this.char
      if (c === '\n') { this.read(); return this.text(from) }
      if (c === '#') { this.skipLine(); continue }
      if (c === ',') { this.read(); afterComma = this.mark(); continue }
      if (!' \t\r'.includes(c)) break
      this.read()
    }
    if (afterComma) this.reset(afterComma)
    return this.text(from)
  }

  // Skip whitespace and `,` to get to the next statement.
  skip() {
    const from = this.mark()
    while (!this.eof()) {
      const c = this.char
      if (c === '#') { this.skipLine(); continue }
      if (!' \t\r,\n'.includes(c)) { break }
      this.read()
    }
    return this.text(from)
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
    this.table[j][i] = inverse(p)
    this.table[i][j] = p
  }
  precedence(...ops: string[]) {
    for (let k = 0; k + 1 < ops.length; ++k) this.set(ops[k], ops[k + 1], Prec.Left)
  }
  closure() {
    const n = this.ops.size
    let changed = true
    while (changed) {
      changed = false
      for (let i = 0; i < n; ++i)
        for (let j = 0; j < n; ++j)
          for (let k = 0; k < n; ++k) {
            const ab = this.table[i][j], bc = this.table[j][k]
            if (ab === Prec.None || ab !== bc) continue
            if (this.table[i][k] === ab) continue
            this.table[i][k] = ab
            this.table[k][i] = inverse(ab)
            changed = true
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

function newline(r: Reader): boolean {
  if (r.char === '\r') r.read()
  return r.read() === '\n'
}

function digit(r: Reader, pattern: RegExp): string | undefined {
  if (r.eof() || !pattern.test(r.char)) return
  return r.read()
}

function separatedDigit(r: Reader, pattern: RegExp): string | undefined {
  if (!r.parse(r => exact(r, '_'))) return
  return digit(r, pattern)
}

function digits(r: Reader, pattern: RegExp): string {
  const first = digit(r, pattern)
  if (first === undefined) return ''
  let result = first
  while (true) {
    const next = r.parse(r => digit(r, pattern), r => separatedDigit(r, pattern))
    if (next === undefined) return result
    result += next
  }
}

function num(r: Reader): number | bigint | undefined {
  const whole = digits(r, /\d/)
  if (!r.peek(r => exact(r, '..')) && r.parse(r => exact(r, '.'))) {
    const frac = digits(r, /\d/)
    if (whole === '' && frac === '') return
    return parseFloat(`${whole}.${frac}`)
  }
  if (whole === '') return
  return BigInt(whole)
}

function hex(r: Reader): ast.Hex | undefined {
  if (!(r.parse(r => exact(r, '0x')))) return
  const num = digits(r, /[0-9a-fA-F]/)
  if (num === '') throw new Error('invalid hex literal')
  return new ast.Hex(num)
}

function negnum(r: Reader) {
  if (r.read() !== '-') return
  const x = num(r)
  if (x === undefined) return
  return -x
}

function number(r: Reader) {
  return r.parse<ast.Hex | bigint | number | undefined>(hex, negnum, num)
}

function symbol(r: Reader): ast.Symbol | undefined {
  let s = ''
  if (!/[A-Za-z_]/.test(r.char)) return
  s += r.read()
  while (!r.eof() && /[A-Za-z0-9_!?]/.test(r.char)) s += r.read()
  return ast.symbol(s)
}

const operators = [
  "=", "==", "!=", "+", "-", "*", "/", "^", ">", "<", ">=", "<=", ":", "&",
  "|", "|>", "&&", "||", ".."
]
const opChars = [...new Set(operators.join(''))].join('')

function opsymbol(r: Reader): ast.Symbol | undefined {
  let s = ''
  while (!r.eof() && opChars.includes(r.char)) s += r.read()
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
  ['\n', ''],
])

function processEscapes(s: string, escape: string): string {
  let result = ''
  let i = 0
  while (i < s.length) {
    if (s.startsWith(escape, i)) {
      i += escape.length
      result += some(escapes.get(s[i++]))
    } else {
      result += s[i++]
    }
  }
  return result
}

function trimCommonIndent(s: string): string {
  const lines = s.split('\n')
  if (!lines[0].trim()) lines.shift()
  if (lines.length === 1) return s
  const indent = lines.at(-1)!
  if (indent.trim()) return s
  lines.pop()
  for (const line of lines)
    if (line.trim() && !line.startsWith(indent))
      throw new Error('insufficient indent in triple-quoted string')
  return lines.map(l => l.slice(indent.length)).join('\n')
}

function tripleString(r: Reader): ast.Tree | undefined {
  const start = r.mark()
  let slashes = 0
  while (!r.eof() && r.char === '\\') { r.read(); slashes++ }
  if (r.eof()) return
  const q = r.char
  if (q !== '"' && q !== '`') return
  if (!r.parse(r => exact(r, q + q + q))) return
  const raw = q === '`'
  const escape = '\\'.repeat(raw ? 0 : Math.max(1, slashes))
  const close = q + q + q + '\\'.repeat(slashes)
  const mark = r.mark()
  let tag = symbol(r)
  if (tag === undefined || !r.parse(newline)) {
    tag = undefined
    r.reset(mark)
  }
  const tagRaw = r.text(start)
  const body = r.mark()
  let s = ''
  while (!r.eof()) {
    if (r.parse(r => exact(r, close))) {
      const trimmed = trimCommonIndent(s)
      const t = raw ? trimmed : processEscapes(trimmed, escape)
      if (tag === undefined) return new ast.Token(t, r.text(start))
      const tagTok = new ast.Token(tag, tagRaw)
      const strTok = new ast.Token(t, r.text(body))
      return ast.Template(tagTok, strTok)
    }
    if (!raw && r.parse(r => exact(r, escape))) {
      s += escape + r.read()
      continue
    }
    s += r.read()
  }
  throw new Error('unterminated triple-quoted string')
}

function string(r: Reader): string | undefined {
  let slashes = 0
  while (!r.eof() && r.char === '\\') { r.read(); slashes++ }
  if (r.eof()) return
  const open = r.read()
  if (open !== '"' && open !== '`') return
  const raw = open === '`'
  const escape = '\\'.repeat(raw ? 0 : Math.max(1, slashes))
  let s = ''
  while (!r.eof()) {
    if (r.parse(r => exact(r, open + '\\'.repeat(slashes))))
      return raw ? s : processEscapes(s, escape)
    if (!raw && r.parse(r => exact(r, escape))) {
      s += escape + r.read()
      continue
    }
    s += r.read()
  }
  throw new Error('unterminated string')
}

// Parsing

function template(r: Reader): ast.Tree | undefined {
  const name = symbol(r)
  if (name === undefined || r.eof()) return
  const from = r.mark()
  const str = string(r)
  if (str === undefined) return
  return ast.Template(name, new ast.Token(str, r.text(from)))
}

function sequence(r: Reader, close?: string): [ast.Tree[], string] {
  const xs: ast.Tree[] = []
  while (true) {
    const pending = r.skip()
    if (close !== undefined && r.char === close) { r.read(); return [xs, pending] }
    const x = statement(r)
    if (x === undefined) return [xs, pending]
    xs.push(ast.leading(x, pending))
  }
}

function brackets(r: Reader, open: string, close: string): [ast.Tree[], string] | undefined {
  if (r.read() !== open) return
  return sequence(r, close)
}

function bracketsTo(r: Reader, open: string, close: string, f: (...xs: ast.Tree[]) => ast.Expr): ast.Expr | undefined {
  const res = brackets(r, open, close)
  if (res === undefined) return
  return ast.inner(f(...res[0]), res[1])
}

function group(r: Reader) { return bracketsTo(r, '(', ')', ast.Group) }
function list(r: Reader) { return bracketsTo(r, '[', ']', ast.List) }
function block(r: Reader) { return bracketsTo(r, '{', '}', ast.Block) }

function token(r: Reader): ast.Tree | undefined {
  const from = r.mark()
  const x = r.parse<ast.Atom | undefined>(symbol, string, number, opsymbol)
  if (x === undefined) return
  return new ast.Token(x, r.text(from))
}

function ellipsis(r: Reader): ast.Tree | undefined {
  if (exact(r, '...') === undefined) return
  return ast.Splat()
}

// Combine all simple expressions with little backtracking
function item(r: Reader): ast.Tree | undefined {
  const pos = r.cursor()
  const ex = r.parse(template, tripleString, ellipsis, token, group, list, block)
  if (ex === undefined) return
  return ex.withmeta({ file: path(), loc: pos })
}

// The following parsers fall back to simpler ones, to avoid excessive
// backtracking / re-parsing. So they don't need to be called in sequence.

// Does calls and fields, so we can handle eg `foo.bar(a).baz`
function postfix(r: Reader): ast.Tree | undefined {
  let ex = item(r)
  if (ex === undefined) return
  while (true) {
    const cur = r.cursor()
    let args = r.parse(r => brackets(r, '(', ')'))
    if (args !== undefined) {
      ex = ast.Call(ex, ...args[0]).withmeta({ file: path(), loc: cur })
      ex = ast.inner(ex, args[1])
      continue
    }
    args = r.parse(r => brackets(r, '[', ']'))
    if (args !== undefined) {
      ex = ast.Index(ex, ...args[0]).withmeta({ file: path(), loc: cur })
      ex = ast.inner(ex, args[1])
      continue
    }
    if (r.peek(r => exact(r, '..'))) { break }
    if (r.parse(r => exact(r, '.'))) {
      const field = r.some(r => item(r))
      ex = ast.Field(ex, field).withmeta({ file: path(), loc: cur })
      continue
    }
    break
  }
  return ex
}

function prefix(r: Reader): ast.Tree | undefined {
  if (r.eof()) return
  if (r.peek(r => exact(r, '!='))) return postfix(r)
  const loc = r.cursor()
  if (r.char === '!' || r.char === '-' || r.char === '$') {
    const op = r.read()
    let ex = prefix(r)
    if (ex === undefined) return new ast.Token(ast.symbol(op))
    return ast.Operator(ast.symbol(op), ex).withmeta({ file: path(), loc })
  }
  if (r.char === '&') {
    r.read()
    let ex = prefix(r)
    if (ex === undefined) return new ast.Token(ast.symbol('&'))
    return ast.Swap(ex).withmeta({ file: path(), loc })
  }
  return postfix(r)
}

const table = new PrecTable(operators)
table.precedence('^', '/', '*', '+', '-');
['>', '<', '>=', '<=', '==', '!='].forEach(op => table.precedence('-', op, '&&'))
table.precedence('&&', '||', '=');

['/', '*', '+', '-', '|', '&', '||', '&&'].forEach(op => table.set(op, op, Prec.Left))
table.closure()

function precedence(a: ast.Symbol, b: ast.Symbol): Prec {
  return table.get(a.toString(), b.toString())
}

function takeTrailing<T extends ast.Tree>(tree: T): [T, string] {
  return [ast.trailing(tree, ''), tree.trivia.trailing]
}

function infix(r: Reader, syn = true, prev?: ast.Symbol): [ast.Tree, boolean] {
  let left = r.some(r => prefix(r))
  while (true) {
    const cur = r.cursor()
    const mark = r.mark()
    const ws = r.skipWhitespace()
    const opcur = r.cursor()
    const op = r.parse(opsymbol)
    if (op === undefined) { r.reset(mark); return [left, false] }
    const prec = prev ? precedence(prev, op) : Prec.Right
    if (prec === Prec.Left) { r.reset(mark); return [left, true] }
    if (prec === Prec.None) { throw new Error(`Operators ${prev} and ${op} are ambiguous at ${path()}:${curstring(r.cursor())}`) }
    left = ast.trail(left, ws)
    const optok = ast.trail(new ast.Token(op, undefined, { file: path(), loc: opcur }), r.skipTrailing())
    const pending = r.skip()
    let right = ast.leading(syn ? syntax(r, op) : infix(r, syn, op)[0], pending)
    let trailing
    [right, trailing] = takeTrailing(right)
    left = ast.Operator(left, optok, right).withmeta({ file: path(), loc: cur })
    left = ast.trail(left, trailing)
  }
}

function splat(r: Reader, syn = true, op?: ast.Symbol): [ast.Tree, boolean] {
  let [ex, backedOut] = infix(r, syn, op)
  if (backedOut) return [ex, backedOut]
  ex = ast.trail(ex, r.skipWhitespace())
  if (r.parse(r => exact(r, '...'))) ex = ast.Splat(ex)
  return [ex, false]
}

// Syntax blocks
// TODO try to avoid as much re-parsing as possible.
const terminators = new Set(['}', ')', ']', ',', '\n'])

function syntax(r: Reader, op?: ast.Symbol): ast.Tree {
  const pos = r.cursor()
  let [name, backedOut] = splat(r, true, op)
  if (backedOut || !(name.unwrap() instanceof ast.Symbol)) return name
  const args: ast.Tree[] = []
  while (!r.eof()) {
    if (args.length) args[args.length - 1] = ast.trail(args.at(-1)!, r.skipWhitespace())
    else name = ast.trail(name, r.skipWhitespace())
    if (terminators.has(r.char)) break
    // `syn` fixes eg `fn x + y {}`, where `y {}` would be
    // parsed as an argument to `+` otherwise.
    const [arg] = splat(r, false)
    args.push(arg)
  }
  if (!args.length) return name
  let trailing
  [args[args.length - 1], trailing] = takeTrailing(args[args.length - 1])
  return ast.trail(ast.Syntax(name, ...args).withmeta({ file: path(), loc: pos }), trailing)
}

function attr(r: Reader): ast.Tree | undefined {
  const pos = r.cursor()
  if (r.read() !== '@') return
  const sym = symbol(r)
  if (sym === undefined) return
  let name = new ast.Token(sym)
  const args: ast.Tree[] = []
  while (!r.eof()) {
    if (args.length) args[args.length - 1] = ast.trail(args.at(-1)!, r.skipWhitespace())
    else name = ast.trail(name, r.skipWhitespace())
    if (terminators.has(r.char)) break
    const [arg] = splat(r, false)
    args.push(arg)
  }
  const pending = r.skip()
  let body = r.parse(attr, syntax)!
  body = ast.leading(body, pending)
  let trailing
  [body, trailing] = takeTrailing(body)
  return ast.trail(ast.Attribute(name, ...args, body).withmeta({ file: path(), loc: pos }), trailing)
}

function statement(r: Reader): ast.Tree | undefined {
  if (r.eof()) return
  let ex = r.parse(attr, syntax)!
  ex = ast.trail(ex, r.skipWhitespace())
  if (!r.eof() && !terminators.has(r.char)) throw new Error(`Expected statement end at ${curstring(r.cursor())}`)
  return ast.trail(ex, r.skipTrailing())
}

function parse(path: string, src: string): ast.Expr {
  return withPath(path, () => {
    const [result, pending] = sequence(new Reader(src))
    return ast.inner(ast.File(...result), pending)
  })
}

function expr(src: string): ast.Tree {
  return only(parse('', src).args)
}
