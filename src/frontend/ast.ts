import { Tag } from './types.js'

export {
  Cursor, Symbol, symbol, gensym, Hex, asSymbol, asString, asNumber, Atom, isAtom,
  Meta, Tree, Token, ExprHead, Expr, isExpr, asExpr, asToken, token, repr, isSyntax
}

interface Cursor {
  line: number
  column: number
}

class Symbol {
  constructor(public name: string) { }
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
  constructor(public digits: string) { }
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
  file: string
  loc: Cursor
}

type Tree = Token | Expr

class Token {
  constructor(public value: Atom, public meta?: Meta) { }
  unwrap(): Atom { return this.value }
  withmeta(m: Meta): Token { return new Token(this.value, m) }
  toString(): string { return repr(this) }
  ungroup(): Token { return this }
}

type ExprHead =
  | 'Group' | 'List' | 'Splat' | 'Call' | 'Index' | 'Field'
  | 'Operator' | 'Swap' | 'Block' | 'Syntax' | 'Quote' | 'Template' | 'Attribute'

class Expr {
  constructor(public head: ExprHead, public args: Tree[], public meta?: Meta) { }

  get length(): number { return this.args.length }
  unwrap(): Expr { return this }
  withmeta(m: Meta | undefined): Expr { return new Expr(this.head, this.args, m) }
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

const constructor = (head: ExprHead) => (...args: (Tree | Atom)[]) => new Expr(head, args.map(token))

export const [Group, List, Splat, Call, Index, Field, Operator, Swap, Block, Syntax, Quote, Template, Attribute] =
  (['Group', 'List', 'Splat', 'Call', 'Index', 'Field', 'Operator', 'Swap', 'Block', 'Syntax', 'Quote', 'Template', 'Attribute'] as const)
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
      case 'Group': return `(${item.args.map(_repr).join(", ")})`
      case 'List': return `[${item.args.map(_repr).join(", ")}]`
      case 'Call': return `${_repr(item.args[0])}(${item.args.slice(1).map(_repr).join(", ")})`
      case 'Index': return `${_repr(item.args[0])}[${item.args.slice(1).map(_repr).join(", ")}]`
      case 'Field': return `${_repr(item.args[0])}.${_repr(item.args[1])}`
      case 'Splat': return `${_repr(item.args[0])}...`
      case 'Operator': {
        const op = String(item.args[0].unwrap())
        if (item.args.length === 2) return `(${op}${_repr(item.args[1])})`
        return `(${item.args.slice(1).map(_repr).join(` ${op} `)})`
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
