import * as ast from './ast.js'
import * as types from './types.js'
import { Type } from './types.js'
import { LIR } from './lower.js'
import { Val } from '../utils/ir.js'

export { Pattern, Builder, pattern, signature, swaps as processSwaps, lowerPattern }

type Pattern =
  | { kind: 'hole' }
  | { kind: 'literal', value: Type }
  | { kind: 'bind', name: string, pattern: Pattern }
  | { kind: 'repeat', pattern: Pattern }
  | { kind: 'trait', trait: Type }
  | { kind: 'pack', parts: Pattern[] }
  | { kind: 'or', patterns: Pattern[] }
  | { kind: 'and', patterns: Pattern[] }
  | { kind: 'constructor', value: Type }

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
  if (t === 'common.patterns/Pattern.Trait') return { kind: 'trait', trait: types.part(x, 1) }
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

function lowerIsa(cx: Lowering, ex: ast.Tree): Val<LIR> {
  ex = ex.ungroup()
  if (ex.unwrap() instanceof ast.Symbol) return cx.node('Trait', cx.expr(ex))
  if (ex instanceof ast.Token) return lowerExpr(cx, ex)
  if (ex.head === 'Field' && ex.args[0].unwrap() instanceof ast.Symbol) return cx.node('Trait', cx.expr(ex))
  if (ex.head === 'Index') return cx.node('Trait', cx.node('Params', ...ex.args.map(x => cx.expr(x))))
  if (ex.head === 'Operator' && ast.symbol('|').isEqual(ex.args[1].unwrap()))
    return cx.node('Or', lowerIsa(cx, ex.args[0]), lowerIsa(cx, ex.args[2]))
  if (ex.head === 'Operator' && ast.symbol('&').isEqual(ex.args[1].unwrap()))
    return cx.node('And', lowerIsa(cx, ex.args[0]), lowerIsa(cx, ex.args[2]))
  return lowerExpr(cx, ex)
}

function lowerExpr(cx: Lowering, ex: ast.Tree): Val<LIR> {
  const x = ex.ungroup().unwrap()
  if (x instanceof ast.Symbol) {
    if (x.toString() === '_') return cx.node('Hole')
    addBinding(cx, x.toString())
    return cx.node('Bind', types.tag(x.toString()), cx.node('Hole'))
  }
  if (typeof x === 'string') throw new Error(`Unsupported string literal ${x}`)
  if (ast.isAtom(x)) return cx.node('Literal', types.atomValue(x))
  if (x.head === 'Operator' && x.args.length === 2 &&
    ast.symbol('$').isEqual(x.args[0].unwrap()))
    return cx.node('Literal', cx.expr(x.args[1]))
  if (x.head === 'Template') return cx.node('Literal', cx.expr(x))
  if (x.head === 'List') {
    const parts = x.args.map(x => lowerExpr(cx, x))
    return cx.node('Pack', cx.node('Literal', types.tag('common.list/List')), ...parts)
  }
  if (x.head === 'Operator' && ast.symbol(':').isEqual(x.args[1].unwrap())) {
    const name = ast.asSymbol(x.args[0].unwrap()).toString()
    const inner = lowerIsa(cx, x.args[2])
    if (name === '_') return inner
    addBinding(cx, name)
    return cx.node('Bind', types.tag(name), inner)
  }
  if (x.head === 'Splat') return cx.node('Repeat', lowerExpr(cx, x.args[0]))
  if (x.head === 'Call')
    return cx.node('Constructor', cx.expr(x.args[0]), ...x.args.slice(1).map(x => lowerExpr(cx, x)))
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
