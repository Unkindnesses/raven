import isEqual from "lodash/isEqual.js"
import * as ast from "./ast.js"
import { asSymbol, Symbol, symbol } from "./ast.js"
import { Type, Tag, tag, pack, part, parts, tagOf, asTag } from "./types.js"
import { Signature, IRValue } from "./modules.js"

export { Pattern, IRValue, modtag, lowerpattern, patternType, pattern }

// Pattern types

type Pattern =
  | { kind: 'hole' }
  | { kind: 'literal', value: Type }
  | { kind: 'bind', name: string, pattern: Pattern }
  | { kind: 'repeat', pattern: Pattern }
  | { kind: 'trait', trait: Type }
  | { kind: 'pack', parts: Pattern[] }
  | { kind: 'or', patterns: Pattern[] }
  | { kind: 'and', patterns: Pattern[] }
  | { kind: 'constructor', func: Tag, args: Pattern[] }

const hole: Pattern = { kind: 'hole' }
const Literal = (x: Type): Pattern => ({ kind: 'literal', value: x })
const Bind = (name: string, pattern: Pattern): Pattern => ({ kind: 'bind', name, pattern })
const Repeat = (pattern: Pattern): Pattern => ({ kind: 'repeat', pattern })
const Trait = (trait: Type): Pattern => ({ kind: 'trait', trait })
const Pack = (...parts: Pattern[]): Pattern => ({ kind: 'pack', parts })
const Or = (...patterns: Pattern[]): Pattern => ({ kind: 'or', patterns })
const And = (...patterns: Pattern[]): Pattern => ({ kind: 'and', patterns })
const Constructor = (func: Tag, ...args: Pattern[]): Pattern => ({ kind: 'constructor', func, args })

// Raven versions

function patternType(x: Pattern): Type {
  switch (x.kind) {
    case 'hole': return pack(tag('common.Hole'))
    case 'literal': return pack(tag('common.Literal'), x.value)
    case 'bind': return pack(tag('common.Bind'), tag(x.name.toString()), patternType(x.pattern))
    case 'repeat': return pack(tag('common.Repeat'), patternType(x.pattern))
    case 'trait': return pack(tag('common.Trait'), x.trait)
    case 'pack': return pack(tag('common.Pack'), ...x.parts.map(patternType))
    case 'or': return pack(tag('common.Or'), ...x.patterns.map(patternType))
    case 'and': return pack(tag('common.And'), ...x.patterns.map(patternType))
    case 'constructor': return pack(tag('common.Constructor'), x.func, ...x.args.map(patternType))
    default:
      let _: never = x
      throw new Error('unreachable')
  }
}

function pattern(x: Type): Pattern {
  const t = asTag(tagOf(x)).path
  if (t === 'common.Hole') return hole
  if (t === 'common.Literal') return Literal(part(x, 1))
  if (t === 'common.Bind') return Bind(asTag(part(x, 1)).path, pattern(part(x, 2)))
  if (t === 'common.Pack') return Pack(...parts(x).map(pattern))
  if (t === 'common.And') return And(...parts(x).map(pattern))
  if (t === 'common.Trait') return Trait(part(x, 1))
  throw new Error(`unsupported pattern ${t}`)
}

// Pattern lowering

function modtag(mod: Tag, tag: string): Tag {
  const prefix = tag.startsWith('.') ? mod : ""
  return new Tag(prefix, tag)
}

function resolvetags(ex: ast.Tree, mod: Tag): ast.Tree {
  if (ex instanceof ast.Token) return ex
  if (ex.head == 'Template') {
    if (!isEqual(ex.args[0].unwrap(), symbol('tag'))) return ex
    return new ast.Token(modtag(mod, ast.asString(ex.args[1])), ex.meta)
  }
  return new ast.Expr(ex.head, ex.args.map(x => resolvetags(x, mod)), ex.meta)
}

function lowerisa(ex: ast.Tree, as: string[], resolve: (x: Symbol) => Type): Pattern {
  ex = ex.ungroup()
  if (ex.unwrap() instanceof Symbol) return Trait(resolve(asSymbol(ex.unwrap())))
  if (ex instanceof ast.Token) return _lowerpattern(ex, as, resolve)
  if (ex.head == 'Index') {
    const args = ex.args.map(x => x.unwrap()).map(x => typeof x === 'bigint' ? Type(x) : resolve(asSymbol(x)))
    return Trait(pack(tag('common.Params'), ...args))
  }
  if (ex.head == 'Operator' && ex.args[0].unwrap() == '|')
    return Or(...ex.args.slice(1).map(x => lowerisa(x, as, resolve)))
  if (ex.head == 'Operator' && ex.args[0].unwrap() == '&')
    return And(...ex.args.slice(1).map(x => lowerisa(x, as, resolve)))
  return _lowerpattern(ex, as, resolve)
}

function _lowerpattern(ex: ast.Tree, as: string[], resolve: (x: Symbol) => Type): Pattern {
  let x = ex.ungroup().unwrap()
  if (x instanceof Symbol) {
    if (x.toString() == '_') return hole
    if (!as.includes(x.toString())) as.push(x.toString())
    return Bind(x.toString(), hole)
  }
  if (typeof x === 'bigint' || typeof x === 'number' || x instanceof Tag)
    return Literal(Type(x))
  if (typeof x === 'string') throw new Error(`Unsupported string literal ${x}`)
  if (x.head === 'List') return Pack(Literal(tag('common.List')), ...x.args.map(x => _lowerpattern(x, as, resolve)))
  if (x.head === 'Operator' && x.args[0].unwrap() == ':') {
    let name = asSymbol(x.args[1].unwrap())
    let T = x.args[2]
    if (name.toString() == '_') return lowerisa(T, as, resolve)
    if (!as.includes(name.toString())) as.push(name.toString())
    return Bind(name.toString(), lowerisa(T, as, resolve))
  }
  if (x.head === 'Splat') return Repeat(_lowerpattern(x.args[0], as, resolve))
  if (x.head === 'Call') {
    let name = x.args[0].unwrap()
    let f = asTag(name instanceof Symbol ? resolve(name) : name)
    return f.path == 'common.core.pack' ?
      Pack(...x.args.slice(1).map(x => _lowerpattern(x, as, resolve))) :
      Constructor(f, ...x.args.slice(1).map(x => _lowerpattern(x, as, resolve)))
  }
  throw new Error(`Invalid pattern syntax ${x}`)
}

// At the top level, &x is allowed.
// TODO: swap should be part of the pattern, so we can reject swaps that mismatch
// the signature.
function _lowersig(ex: ast.Tree, as: string[], swaps: Map<number, string>, resolve: (x: Symbol) => Type): Pattern {
  if (!(ex instanceof ast.Expr && ex.head === 'List')) return _lowerpattern(ex, as, resolve)
  const args = ex.args.map((x, i) => {
    if (x instanceof ast.Token) return _lowerpattern(x, as, resolve)
    if (x.head === 'Swap') {
      swaps.set(i + 1, asSymbol(x.args[0].unwrap()).toString())
      return _lowerpattern(x.args[0], as, resolve)
    } else if (x.head === 'Operator' && asSymbol(x.args[0].unwrap()).toString() == ':' && x.args[1] instanceof ast.Expr && x.args[1].head === 'Swap') {
      swaps.set(i + 1, asSymbol(x.args[1].args[0].unwrap()).toString())
      return _lowerpattern(ast.Operator(symbol(':'), x.args[1].args[0], ...x.args.slice(2)), as, resolve)
    } else {
      return _lowerpattern(x, as, resolve)
    }
  })
  return Pack(Literal(tag('common.List')), ...args)
}

function lowerpattern(ex: ast.Tree, mod = tag(''), resolve: (x: Symbol) => Type = x => { throw new Error(`Couldn't statically resolve ${x}`) }): Signature {
  ex = resolvetags(ex, mod)
  const as: string[] = []
  const swaps = new Map<number, string>()
  const p = _lowersig(ex, as, swaps, resolve)
  return { pattern: p, args: as, swap: swaps }
}
