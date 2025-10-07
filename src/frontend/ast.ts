import { Tag } from './types'

export interface Cursor {
  line: number
  column: number
}

export class Symbol {
  constructor(public name: string) { }
  toString() { return this.name }
}

export function symbol(name: string): Symbol {
  return new Symbol(name)
}

let counter = 0
export function gensym(name = '') { return symbol(`${name}_${counter++}`) }

function unwrapToken<T>(x: unknown, check: (v: unknown) => v is T, type: string): T {
  if (x instanceof Token) x = x.unwrap()
  if (!check(x)) throw new Error(`Expected ${type}`)
  return x
}

export const asSymbol = (x: unknown) => unwrapToken(x, (v): v is Symbol => v instanceof Symbol, 'Symbol')
export const asString = (x: unknown) => unwrapToken(x, (v): v is string => typeof v === 'string', 'string')
export const asNumber = (x: unknown) => unwrapToken(x, (v): v is number => typeof v === 'number', 'number')

export type Atom = Tag | Symbol | string | number | bigint

export function isAtom(x: unknown): x is Atom {
  return x instanceof Tag ||
    x instanceof Symbol ||
    typeof x === 'string' ||
    typeof x === 'number' ||
    typeof x === 'bigint'
}

export interface Meta {
  file: string
  loc: Cursor
}

export type Tree = Token | Expr

export class Token {
  constructor(public value: Atom, public meta?: Meta) { }
  unwrap(): Atom { return this.value }
  withmeta(m: Meta): Token { return new Token(this.value, m) }
  toString(): string { return repr(this) }
  ungroup(): Token { return this }
}

export type ExprHead =
  | 'Group' | 'List' | 'Splat' | 'Call' | 'Index' | 'Field'
  | 'Operator' | 'Swap' | 'Block' | 'Syntax' | 'Quote' | 'Template' | 'Annotation'

export class Expr {
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

export function isExpr<T extends ExprHead>(x: Tree, head: T): x is Expr & { head: T } {
  return x instanceof Expr && x.head === head
}

export function asExpr<T extends ExprHead>(x: Tree, head?: T): Expr & { head: T } {
  if (!(x instanceof Expr && (head === undefined || x.head === head))) throw new Error(`Expected ${head} expression`)
  return x as Expr & { head: T }
}

export function asToken(x: Tree): Token {
  if (!(x instanceof Token)) throw new Error('Expected Token')
  return x
}

export function token(x: Atom | Tree): Tree {
  return x instanceof Expr || x instanceof Token ? x : new Token(x)
}

const constructor = (head: ExprHead) => (...args: (Tree | Atom)[]) => new Expr(head, args.map(token))

export const [Group, List, Splat, Call, Index, Field, Operator, Swap, Block, Syntax, Quote, Template, Annotation] =
  (['Group', 'List', 'Splat', 'Call', 'Index', 'Field', 'Operator', 'Swap', 'Block', 'Syntax', 'Quote', 'Template', 'Annotation'] as const)
    .map(constructor)

export function repr(item: Tree, indent: number = 0): string {
  const _repr = (item: Tree, i?: number) => repr(item, i || indent)
  if (item instanceof Token) {
    let value = item.value
    if (value instanceof Symbol) return value.toString()
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
      case 'Operator': return `(${item.args.slice(1).map(_repr).join(` ${String(item.args[0].unwrap())} `)})`
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
      case 'Annotation': {
        const params = item.args.slice(0, -1).map(_repr)
        const target = _repr(item.args[item.args.length - 1])
        return `@${params.join(' ')}\n${target}`
      }
      default: let _: never = item.head
    }
  }
  throw new Error('unreachable')
}
