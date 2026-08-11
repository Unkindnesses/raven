import * as ast from './ast.js'
import * as types from './types.js'
import { Type } from './types.js'
import { LIR } from './lower.js'
import { Val } from '../utils/ir.js'
import { asBigInt } from '../utils/map.js'

export { Pattern, Term, Builder, pattern, term, signature, swaps as processSwaps, lowerPattern }

type Term =
  | { kind: 'const', value: Type }
  | { kind: 'var', name: string }

type Pattern =
  | { kind: 'hole' }
  | { kind: 'literal', value: Type }
  | { kind: 'bind', name: string, pattern: Pattern }
  | { kind: 'repeat', pattern: Pattern }
  | { kind: 'trait', trait: Term, args: Pattern[] }
  | { kind: 'pack', parts: Pattern[] }
  | { kind: 'or', patterns: Pattern[] }
  | { kind: 'and', patterns: Pattern[] }
  | { kind: 'constructor', value: Type }

function term(x: Type): Term {
  const t = types.asTag(types.tagOf(x)).path
  if (t === 'common.patterns/Term.Const') return { kind: 'const', value: types.part(x, 1) }
  if (t === 'common.patterns/Term.Var') return { kind: 'var', name: types.asTag(types.part(x, 1)).path }
  throw new Error(`unsupported term ${t}`)
}

function pattern(x: Type): Pattern {
  const t = types.asTag(types.tagOf(x)).path
  if (t === 'common.patterns/Pattern.Hole') return { kind: 'hole' }
  if (t === 'common.patterns/Pattern.Literal') return { kind: 'literal', value: types.part(x, 1) }
  if (t === 'common.patterns/Pattern.Bind')
    return { kind: 'bind', name: types.asTag(types.part(x, 1)).path, pattern: pattern(types.part(x, 2)) }
  if (t === 'common.patterns/Pattern.Repeat') return { kind: 'repeat', pattern: pattern(types.part(x, 1)) }
  if (t === 'common.patterns/Pattern.Pack') return { kind: 'pack', parts: types.parts(x).map(pattern) }
  if (t === 'common.patterns/Pattern.Or') return { kind: 'or', patterns: types.parts(x).map(pattern) }
  if (t === 'common.patterns/Pattern.And') return { kind: 'and', patterns: types.parts(x).map(pattern) }
  if (t === 'common.patterns/Pattern.Trait') {
    const [T, ...args] = types.parts(x)
    return { kind: 'trait', trait: term(T), args: args.map(pattern) }
  }
  if (t === 'common.patterns/Pattern.Constructor') return { kind: 'constructor', value: x }
  throw new Error(`unsupported pattern ${t}`)
}

function stripSwap(ex: ast.Tree, position: number, swaps: Map<number, string>): ast.Tree {
  if (ast.isExpr(ex, 'Swap')) {
    const name = ast.asSymbol(ex.args[0].unwrap()).toString()
    swaps.set(position, name)
    return ex.args[0]
  }
  if (ast.isExpr(ex, 'Operator') && ast.symbol(':').isEqual(ex.args[1].unwrap()))
    return ex.map((part, i) => i === 0 ? stripSwap(part, position, swaps) : part)
  return ex
}

function swaps(ex: ast.Tree): [ast.Tree, Map<number, string>] {
  const swaps = new Map<number, string>()
  if (!ast.isExpr(ex, 'List')) return [ex, swaps]
  const stripped = ex.map((arg, i) => stripSwap(arg, i + 1, swaps))
  return [stripped, swaps]
}

interface Builder {
  expr(ex: ast.Tree): Val<LIR>
  node(name: string, ...parts: Val<LIR>[]): Val<LIR>
}

interface Lowering extends Builder {
  args: string[]
}

function lowering(builder: Builder): Lowering {
  return { ...builder, args: [] }
}

function addBinding(cx: Lowering, name: string): void {
  if (name !== '_' && !cx.args.includes(name)) cx.args.push(name)
}

function lowerTerm(cx: Lowering, ex: ast.Tree): Val<LIR> {
  const x = ex.ungroup().unwrap()
  if (x instanceof ast.Symbol && cx.args.includes(x.toString()))
    return cx.node('Term.Var', types.tag(x.toString()))
  return cx.node('Term.Const', cx.expr(ex))
}

function lowerIsa(cx: Lowering, ex: ast.Tree): Val<LIR> {
  ex = ex.ungroup()
  if (ex.unwrap() instanceof ast.Symbol) return cx.node('Pattern.Trait', lowerTerm(cx, ex))
  if (ex instanceof ast.Token) return lowerExpr(cx, ex)
  if (ex.head === 'Field' && ex.args[0].unwrap() instanceof ast.Symbol) return cx.node('Pattern.Trait', lowerTerm(cx, ex))
  if (ex.head === 'Index')
    return cx.node('Pattern.Trait', lowerTerm(cx, ex.args[0]), ...ex.args.slice(1).map(x => lowerIsa(cx, x)))
  if (ast.isSyntax(ex, 'bits'))
    return cx.node('Pattern.Trait', cx.node('Term.Const', types.bits(Number(asBigInt(ex.args[1].unwrap())), 0n)))
  if (ex.head === 'Operator' && ast.symbol('|').isEqual(ex.args[1].unwrap()))
    return cx.node('Pattern.Or', lowerIsa(cx, ex.args[0]), lowerIsa(cx, ex.args[2]))
  if (ex.head === 'Operator' && ast.symbol('&').isEqual(ex.args[1].unwrap()))
    return cx.node('Pattern.And', lowerIsa(cx, ex.args[0]), lowerIsa(cx, ex.args[2]))
  return lowerExpr(cx, ex)
}

function lowerExpr(cx: Lowering, ex: ast.Tree): Val<LIR> {
  const x = ex.ungroup().unwrap()
  if (x instanceof ast.Symbol) {
    if (x.toString() === '_') return cx.node('Pattern.Hole')
    addBinding(cx, x.toString())
    return cx.node('Pattern.Bind', types.tag(x.toString()), cx.node('Pattern.Hole'))
  }
  if (typeof x === 'string') throw new Error(`Unsupported string literal ${x}`)
  if (ast.isAtom(x)) return cx.node('Pattern.Literal', types.atomValue(x))
  if (x.head === 'Operator' && x.args.length === 2 &&
    ast.symbol('$').isEqual(x.args[0].unwrap()))
    return cx.node('Pattern.Literal', cx.expr(x.args[1]))
  if (x.head === 'Template') return cx.node('Pattern.Literal', cx.expr(x))
  if (x.head === 'List') {
    const parts = x.args.map(x => lowerExpr(cx, x))
    return cx.node('Pattern.Pack', cx.node('Pattern.Literal', types.tag('common.list/List')), ...parts)
  }
  if (x.head === 'Operator' && ast.symbol(':').isEqual(x.args[1].unwrap())) {
    const name = ast.asSymbol(x.args[0].unwrap()).toString()
    const inner = lowerIsa(cx, x.args[2])
    if (name === '_') return inner
    addBinding(cx, name)
    return cx.node('Pattern.Bind', types.tag(name), inner)
  }
  if (x.head === 'Splat') return cx.node('Pattern.Repeat', lowerExpr(cx, x.args[0]))
  if (x.head === 'Call')
    return cx.node('Pattern.Constructor', cx.expr(x.args[0]), ...x.args.slice(1).map(x => lowerExpr(cx, x)))
  throw new Error(`Invalid pattern syntax ${x}`)
}

function lowerPattern(builder: Builder, ex: ast.Tree): [Val<LIR>, string[]] {
  const cx = lowering(builder)
  return [lowerExpr(cx, ex), cx.args]
}

function signature(ex: ast.Tree): { args: string[], swap: Map<number, string> } {
  const [pattern, swap] = swaps(ex)
  const [, args] = lowerPattern({ expr: () => 0, node: () => 0 }, pattern)
  return { args, swap }
}
