import { Tag } from './types.js'

export {
  Cursor, Symbol, symbol, gensym, Hex, asSymbol, asString, asNumber, Atom, isAtom,
  Meta, Tree, Token, ExprHead, Expr, isExpr, asExpr, asToken, token, repr, print, isSyntax,
  Trivia, leading, trailing, inner, trail, callargs
}

interface Cursor {
  readonly line: number
  readonly column: number
}

class Symbol {
  constructor(readonly name: string) { }
  toString() { return this.name }
  isEqual(other: unknown): other is Symbol {
    return other instanceof Symbol && this.name === other.name
  }
}

function symbol(name: string): Symbol {
  return new Symbol(name)
}

let counter = 0
function gensym(name = '') { return symbol(`${name}_${counter++}`) }

class Hex {
  constructor(readonly digits: string) { }
}

function unwrapToken<T>(x: unknown, check: (v: unknown) => v is T, type: string): T {
  if (x instanceof Token) x = x.unwrap()
  if (!check(x)) throw new Error(`Expected ${type}`)
  return x
}

const asSymbol = (x: unknown) => unwrapToken(x, (v): v is Symbol => v instanceof Symbol, 'Symbol')
const asString = (x: unknown) => unwrapToken(x, (v): v is string => typeof v === 'string', 'string')
const asNumber = (x: unknown) => unwrapToken(x, (v): v is number => typeof v === 'number', 'number')

type Atom = Tag | Symbol | Hex | string | number | bigint

function isAtom(x: unknown): x is Atom {
  return x instanceof Tag ||
    x instanceof Symbol ||
    x instanceof Hex ||
    typeof x === 'string' ||
    typeof x === 'number' ||
    typeof x === 'bigint'
}

interface Meta {
  readonly file: string
  readonly loc: Cursor
}

// Source text around a node: `leading` before its first character, `trailing`
// from its last character to the end of the line (including separators and the
// line break), `inner` before its closing delimiter, after all children.
interface Trivia {
  readonly leading: string
  readonly trailing: string
  readonly inner: string
}

type Extent = readonly [number, number]

type Tree = Token | Expr

const emptyTrivia: Trivia = { leading: '', trailing: '', inner: '' }

function leading<T extends Tree>(node: T, text: string): T {
  return node.withtrivia({ leading: text }) as T
}

function trailing<T extends Tree>(node: T, text: string): T {
  return node.withtrivia({ trailing: text }) as T
}

function trail<T extends Tree>(node: T, text: string): T {
  return trailing(node, node.trivia.trailing + text)
}

function inner<T extends Tree>(node: T, text: string): T {
  return node.withtrivia({ inner: text }) as T
}

class Token {
  readonly extent: Extent
  constructor(readonly value: Atom, readonly raw?: string, readonly meta?: Meta, readonly trivia = emptyTrivia) {
    this.extent = extent(this)
  }
  unwrap(): Atom { return this.value }
  map(_: (tree: Tree, index: number) => Tree): Token { return this }
  withmeta(meta: Meta | undefined): Token { return new Token(this.value, this.raw, meta, this.trivia) }
  withtrivia(value: Partial<Trivia>): Token { return new Token(this.value, this.raw, this.meta, { ...this.trivia, ...value }) }
  toString(): string { return repr(this) }
  ungroup(): Token { return this }
}

type ExprHead =
  | 'File' | 'Group' | 'List' | 'Splat' | 'Call' | 'Index' | 'Field'
  | 'Operator' | 'Swap' | 'Block' | 'Syntax' | 'Quote' | 'Template' | 'Attribute'

class Expr {
  readonly extent: Extent
  constructor(readonly head: ExprHead, readonly args: readonly Tree[], readonly meta?: Meta, readonly trivia = emptyTrivia) {
    this.extent = extent(this)
  }
  get length(): number { return this.args.length }
  unwrap(): Expr { return this }
  map(f: (tree: Tree, index: number) => Tree) { return new Expr(this.head, this.args.map(f), this.meta, this.trivia) }
  withmeta(m: Meta | undefined) { return new Expr(this.head, this.args, m, this.trivia) }
  withtrivia(value: Partial<Trivia>) { return new Expr(this.head, this.args, this.meta, { ...this.trivia, ...value }) }
  toString(): string { return repr(this) }
  ungroup(): Tree {
    if (this.head === 'Group' && this.args.length === 1) return this.args[0].ungroup()
    return this
  }
}

function isExpr<T extends ExprHead>(x: Tree, head: T): x is Expr & { head: T } {
  return x instanceof Expr && x.head === head
}

function isSyntax(ex: Tree, name: string): ex is Expr & { head: 'Syntax' } {
  return isExpr(ex, 'Syntax') && symbol(name).isEqual(ex.args[0].unwrap())
}

function asExpr<T extends ExprHead>(x: Tree, head?: T): Expr & { head: T } {
  if (!(x instanceof Expr && (head === undefined || x.head === head))) throw new Error(`Expected ${head} expression`)
  return x as Expr & { head: T }
}

function asToken(x: Tree): Token {
  if (!(x instanceof Token)) throw new Error('Expected Token')
  return x
}

function token(x: Atom | Tree): Tree {
  return x instanceof Expr || x instanceof Token ? x : new Token(x)
}

function callargs(ex: Expr & { head: 'Call' | 'Operator' }): readonly Tree[] {
  if (ex.head === 'Call' || ex.args.length === 2) return ex.args
  return [ex.args[1], ex.args[0], ex.args[2]]
}

const constructor = (head: ExprHead) => (...args: (Tree | Atom)[]) => new Expr(head, args.map(token))

export const [File, Group, List, Splat, Call, Index, Field, Operator, Swap, Block, Syntax, Quote, Template, Attribute] =
  (['File', 'Group', 'List', 'Splat', 'Call', 'Index', 'Field', 'Operator', 'Swap', 'Block', 'Syntax', 'Quote', 'Template', 'Attribute'] as const)
    .map(constructor)

function repr(item: Tree, indent: number = 0): string {
  const _repr = (item: Tree, i?: number) => repr(item, i || indent)
  if (item instanceof Token) {
    let value = item.value
    if (value instanceof Symbol) return value.toString()
    if (value instanceof Hex) return `0x${value.digits}`
    if (typeof value === 'number' || typeof value === 'bigint') return String(value)
    if (typeof value === 'string') return JSON.stringify(value)
    if (value instanceof Tag) return value.toString()
    let _: never = value
  } else if (item instanceof Expr) {
    switch (item.head) {
      case 'File': return item.args.map(_repr).join("\n")
      case 'Group': return `(${item.args.map(_repr).join(", ")})`
      case 'List': return `[${item.args.map(_repr).join(", ")}]`
      case 'Call': return `${_repr(item.args[0])}(${item.args.slice(1).map(_repr).join(", ")})`
      case 'Index': return `${_repr(item.args[0])}[${item.args.slice(1).map(_repr).join(", ")}]`
      case 'Field': return `${_repr(item.args[0])}.${_repr(item.args[1])}`
      case 'Splat': return `${_repr(item.args[0])}...`
      case 'Operator': {
        if (item.args.length === 2) return `(${String(item.args[0].unwrap())}${_repr(item.args[1])})`
        return `(${_repr(item.args[0])} ${String(item.args[1].unwrap())} ${_repr(item.args[2])})`
      }
      case 'Swap': return `&${_repr(item.args[0])}`
      case 'Quote': return `\`${String(item.args[0].unwrap())}\``
      case 'Template': return `${_repr(item.args[0])}${_repr(item.args[1])}`
      case 'Block':
        let blockStr = "{"
        if (item.meta) blockStr += ` # ${item.meta.file}:${item.meta.loc.line}`
        const innerIndent = '  '.repeat(indent + 1)
        for (const arg of item.args) {
          blockStr += `\n${innerIndent}${_repr(arg, indent + 1)}`
        }
        blockStr += `\n${'  '.repeat(indent)}}`
        return blockStr
      case 'Syntax':
        let syntaxStr = _repr(item.args[0])
        for (const arg of item.args.slice(1)) syntaxStr += ` ${_repr(arg)}`
        return syntaxStr
      case 'Attribute': {
        const params = item.args.slice(0, -1).map(_repr)
        const target = _repr(item.args[item.args.length - 1])
        return `@${params.join(' ')}\n${target}`
      }
      default: let _: never = item.head
    }
  }
  throw new Error('unreachable')
}

// Printing and source extents

type Source = string | Tree | readonly Source[]

function textExtent(text: string): Extent {
  const lines = text.split('\n')
  return [lines.length - 1, lines.at(-1)!.length]
}

function addExtents(...extents: Extent[]): Extent {
  let lines = 0, cols = 0
  for (const [l, c] of extents) {
    lines += l
    cols = l ? c : cols + c
  }
  return [lines, cols]
}

function sourceExtent(source: Source): Extent {
  if (typeof source === 'string') return textExtent(source)
  if (source instanceof Token || source instanceof Expr) return source.extent
  let extent: Extent = [0, 0]
  for (const part of source) extent = addExtents(extent, sourceExtent(part))
  return extent
}

function bodySource(tree: Tree, inner: string): Source {
  if (tree instanceof Token) return tree.raw ?? repr(tree)
  const args = tree.args
  switch (tree.head) {
    case 'File': return [args, inner]
    case 'Group': return ['(', args, inner, ')']
    case 'List': return ['[', args, inner, ']']
    case 'Block': return ['{', args, inner, '}']
    case 'Call': return [args[0], '(', args.slice(1), inner, ')']
    case 'Index': return [args[0], '[', args.slice(1), inner, ']']
    case 'Field': return [args[0], '.', args[1]]
    case 'Splat': return [args[0], '...']
    case 'Swap': return ['&', args[0]]
    case 'Operator':
    case 'Syntax':
    case 'Template': return args
    case 'Attribute': return ['@', args]
    case 'Quote': return repr(tree)
    default:
      let _: never = tree.head
      throw new Error('unreachable')
  }
}

function extent(tree: Tree): Extent {
  return sourceExtent([tree.trivia.leading, bodySource(tree, tree.trivia.inner), tree.trivia.trailing])
}

function printSource(source: Source): string {
  if (typeof source === 'string') return source
  if (source instanceof Token || source instanceof Expr) return print(source)
  return source.map(printSource).join('')
}

function print(x: Tree | readonly Tree[]): string {
  if (!(x instanceof Token || x instanceof Expr)) return x.map(t => print(t)).join('')
  return x.trivia.leading + printSource(bodySource(x, x.trivia.inner)) + x.trivia.trailing
}
