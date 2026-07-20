import * as ast from "./ast.js"
import * as ir from "../utils/ir.js"
import { Val, fuseblocks, prune, ssa } from "../utils/ir.js"
import { asSymbol, asString, Symbol, symbol, gensym, token } from "./ast.js"
import * as types from "./types.js"
import { Type, Tag, tag, pack, bits, nil, atomValue } from "./types.js"
import { ValueType, AbsHeapType, i32, i64, f32, f64, externref } from "../wasm/wasm.js"
import { Module, Signature, Binding, MIR, xstring, xjs, Method, xglobal, xset, SetGlobal, Invoke, Wasm, Modules } from "./modules.js"
import { Def } from "../dwarf/index.js"
import { asBigInt, some } from "../utils/map.js"
import { isnil_method, notnil_method, part_method, packcat_method } from "../middle/primitives.js"
import { modtag } from "./patterns.js"
import { Cache, Caching } from "../utils/cache.js"

export {
  Lowered, lower_toplevel, bundlemacro, expand, lowerfn, source,
  globals, assigned_globals, xlist, xpart, xcall, xtuple, attrs
}

// Built-in macros

const s = symbol

function namify(x: ast.Tree, suffix = ""): ast.Expr | ast.Symbol {
  if (x.unwrap() instanceof Symbol) return ast.symbol(x.unwrap().toString() + suffix)
  if (ast.isExpr(x, 'Operator')) return namify(ast.callargs(x)[1], suffix)
  if (ast.isExpr(x, 'Splat')) return ast.Splat(namify(x.args[0], suffix))
  throw new Error(`Unsupported namify argument ${ast.repr(x)}`)
}

function patternArgExpr(x: ast.Tree): ast.Tree {
  if (x instanceof ast.Token && x.unwrap() instanceof Symbol)
    return ast.Call(tag('common.Bind'), tag(x.unwrap().toString()), ast.Call(tag('common.Hole')))
  if (ast.isExpr(x, 'Operator')) {
    const name = asSymbol(x.args[0].unwrap())
    return ast.Call(tag('common.Bind'), tag(name.toString()), ast.Call(tag('common.Trait'), x.args[2]))
  }
  throw new Error(`Unsupported bundle pattern argument ${ast.repr(x)}`)
}

function bundlemacro(ex: ast.Expr): ast.Expr {
  const [superSpec, spec] = ex.args.length === 2 ? [undefined, ex.args[1]] : [ex.args[1], ex.args[2]]
  const specs = ast.isExpr(spec, 'Block') ? spec.args : [spec]
  const body: ast.Tree[] = []
  const names: ast.Symbol[] = []
  for (const spec of specs) {
    if (!(ast.isExpr(spec, 'Call'))) throw new Error(`bundlemacro: spec must be a call, got ${ast.repr(spec)}`)
    const name = asSymbol(spec.args[0].unwrap())
    names.push(name)
    const T = ast.Template(ast.symbol("tag"), `.${name}`)
    const args = spec.args.slice(1)
    const hasSplat = args.some(a => ast.isExpr(a, 'Splat'))
    const argNames = args.map(a => namify(a))
    body.push(
      ast.Syntax(s('fn'), spec,
        ast.Block(ast.Call(tag('common.core.pack'), T, ...argNames))))
    body.push(
      ast.Syntax(s('fn'),
        ast.Call(tag('common.matchTrait'), T,
          ast.Operator(s('_val'), s(':'), ast.Call(tag('common.core.pack'), T, ...argNames))),
        ast.Block(ast.Call(s('Some'), s('_val')))))
    body.push(
      ast.Syntax(s('fn'), ast.Call(tag('common.constructorPattern'), T, ...argNames),
        ast.Block(
          ast.Call(tag('common.Pack'),
            ast.Call(tag('common.Literal'), T), ...argNames))))
    if (hasSplat) continue
    let pat = ast.Call(tag('common.Pack'), ast.Call(tag('common.Literal'), T), ...args.map(patternArgExpr))
    body.push(
      ast.Syntax(s('fn'), ast.Call(tag('common.castTrait'), T, s('_val')),
        ast.Block(
          ast.Operator(s('_match'), s('='), ast.Call(tag('common._match'), s('_val'), pat, s('true'))),
          ast.Syntax(s('if'), ast.Operator(s('!'), ast.Call(tag('common.core.nil?'), s('_match'))),
            ast.Block(
              ast.Call(tag('common.Some'),
                ast.Call(tag('common.core.part'), ast.Call(tag('common.core.notnil'), s('_match')), 1n)))))))
    body.push(
      ast.Syntax(s('fn'), ast.Call(tag('common.show'), ast.Call(name, ...argNames)),
        ast.Block(
          ast.Call(s('print'), `${name}(`),
          ...argNames.map((n, i) =>
            i === argNames.length - 1
              ? ast.Call(tag('common.show'), n)
              : ast.Group(ast.Call(tag('common.show'), n), ast.Call(s('print'), ', '))),
          ast.Call(s('print'), ')'))))
    const lhs = args.map(a => namify(a, '_1'))
    const rhs = args.map(a => namify(a, '_2'))
    const comps = lhs.map((l, i) => ast.Operator(l, s('=='), rhs[i]))
    const eqBody = comps.length === 0 ? s('true') : comps.reduce((a, b) => ast.Operator(a, s('&&'), b))
    body.push(
      ast.Syntax(s('fn'),
        ast.Call(tag('common.=='), ast.Call(name, ...lhs), ast.Call(name, ...rhs)),
        ast.Block(eqBody)))
  }
  if (superSpec) {
    const superTag = ast.Template(symbol('tag'), `.${asSymbol(superSpec.unwrap())}`)
    body.push(ast.Operator(superSpec, symbol('='), superTag))
    body.push(
      ast.Syntax(s('fn'),
        ast.Call(tag('common.matchTrait'), superTag,
          ast.Operator(s('_val'), symbol(':'),
            names.slice(1).reduce((a, b) => ast.Operator(a, symbol('|'), b), token(names[0])))),
        ast.Block(ast.Call(tag('common.Some'), s('_val')))))
  }
  return ast.Group(...body)
}

function attrs(x: ast.Tree, as = new Map<string, ast.Tree[]>()): [ast.Tree, Map<string, ast.Tree[]>] {
  if (!ast.isExpr(x, 'Attribute')) return [x, as]
  const name = asSymbol(x.args[0]).toString()
  const args = x.args.slice(1, -1)
  as.set(name, args)
  const [inner] = attrs(x.args[x.args.length - 1], as)
  return [inner, as]
}

function withAttrs(target: ast.Tree, as: Map<string, ast.Tree[]>): ast.Tree {
  let result = target
  for (const [name, args] of [...as].reverse()) {
    result = ast.Attribute(symbol(name), ...args, result)
  }
  return result
}

function formacro(ex: ast.Expr): ast.Expr {
  let [forExpr, as] = attrs(ex)
  forExpr = ast.asExpr(forExpr, 'Syntax')
  const assign = ast.asExpr(forExpr.args[1], 'Operator')
  if (!symbol('=').isEqual(assign.args[1].unwrap()))
    throw new Error('for syntax expects `=` assignment')
  const [x, xs, body] = [assign.args[0], assign.args[2], forExpr.args[2]]
  const [itr, val] = [gensym("itr"), gensym("val")]
  return ast.Block(
    ast.Operator(itr, s("="), ast.Call(tag("common.iterate"), xs)),
    withAttrs(ast.Syntax(s("while"), s("true"), ast.Block(
      ast.Operator(val, s("="), ast.Call(tag("common.next"), ast.Swap(itr))),
      ast.Syntax(s("if"), ast.Call(symbol("nil?"), val), ast.Block(s("break"))),
      ast.Syntax(s("let"),
        ast.Operator(x, s("="), ast.Call(tag("common.core.part"), ast.Call(tag("common.core.notnil"), val), 1n)),
        ast.asExpr(body, 'Block')))), as))
}

function matchmacro(ex: ast.Expr): ast.Expr {
  const val = asSymbol(ex.args[1].unwrap())
  const clauses = ast.asExpr(ex.args[2], 'Block').args
  const body: ast.Tree[] = [token(s('if'))]
  for (let i = 0; i < clauses.length; i++) {
    const clause = clauses[i]
    if (i > 0) body.push(token(s('else')), token(s('if')))
    if (!ast.isSyntax(clause, 'let'))
      throw new Error('matchmacro: clause must be a let')
    body.push(token(s('let')), ast.Operator(clause.args[1], s('='), val), ast.asExpr(clause.args[2], 'Block'))
  }
  body.push(token(s('else')), ast.Block(ast.Call(tag('common.abort'), "Match clause failed")))
  return ast.Syntax(...body)
}

function showmacro(ex: ast.Expr, pack = false): ast.Expr {
  const arg = ex.args[1]
  const name = gensym()
  return ast.Block(
    ast.Operator(name, s('='), arg),
    ast.Call(s('print'), new ast.Token(ast.repr(arg) + " = ")),
    ast.Call(pack ? s('showPack') : s('show'), name),
    ast.Call(s('println')),
    name
  )
}

function testmacro(ex: ast.Expr): ast.Tree {
  const arg = ex.args[1]
  return ast.Syntax(s('if'), arg, ast.Block(
    ast.Call(s('println'), `pass: ${ast.repr(arg)}`)
  ), s('else'), ast.Block(
    ast.Call(s('println'), `fail: ${ast.repr(arg)}`)
  ))
}

interface SelectCase {
  pattern?: ast.Tree
  call: ast.Expr
  body: ast.Tree
}

function parseSelectCall(ex: ast.Tree): { pattern?: ast.Tree, call: ast.Expr } {
  ex = ex.ungroup()
  if (ast.isExpr(ex, 'Operator') && symbol('=').isEqual(ex.args[1].unwrap())) {
    const call = ex.args[2].ungroup()
    if (!ast.isExpr(call, 'Call')) throw new Error('select case assignment must be of the form `x = f(...)`')
    return { pattern: ex.args[0], call }
  }
  if (!ast.isExpr(ex, 'Call')) throw new Error('select case must be of the form `f(...)` or `x = f(...)`')
  return { call: ex }
}

function parseSelectCase(ex: ast.Tree): SelectCase {
  if (!ast.isSyntax(ex, 'case'))
    throw new Error('select block entries must be cases')
  if (ex.args.length !== 3) throw new Error('select case must have an operation and a body')
  const { pattern, call } = parseSelectCall(ex.args[1])
  return { pattern, call, body: ast.asExpr(ex.args[2], 'Block') }
}

function selectDescriptor(c: SelectCase): ast.Expr {
  return ast.List(c.call.args[0], ast.List(...c.call.args.slice(1)))
}

function allocsmacro(ex: ast.Expr): ast.Expr {
  const arg = ex.args[1]
  const before = gensym("before")
  return ast.Group(
    ast.Operator(before, s('='), ast.Call(tag('common.core.allocs'), ast.Call(s('Int32'), 0n))),
    arg,
    ast.Operator(ast.Call(tag('common.core.allocs'), ast.Call(s('Int32'), 0n)), s('-'), before)
  )
}

function asyncmacro(ex: ast.Expr): ast.Tree {
  if (ex.args.length !== 2 || !ast.isExpr(ex.args[1], 'Block'))
    throw new Error('async syntax expects a single block')
  return ast.Call(s('async'), ast.Syntax(s('fn'), ex.args[1])).withmeta(ex.meta)
}

const macros = new Map<string, (ex: ast.Expr) => ast.Tree>([
  ['bundle', bundlemacro],
  ['for', formacro],
  ['show', showmacro],
  ['showPack', ex => showmacro(ex, true)],
  ['test', testmacro],
  ['match', matchmacro],
  ['allocs', allocsmacro],
  ['async', asyncmacro],
])

function macroName(ex: ast.Tree): string | undefined {
  const [x] = attrs(ex)
  if (!ast.isExpr(x, 'Syntax')) return
  return asSymbol(x.args[0].unwrap()).toString()
}

function expand(ex: ast.Tree): ast.Tree {
  if (ex instanceof ast.Token) return ex
  const name = macroName(ex)
  if (name && macros.has(name)) return expand(macros.get(name)!(ex))
  return ex.map(expand)
}

// Expr -> IR lowering

type IRValue = Type | ir.Slot | Binding
type LIR = ir.IR<IRValue, Type>

function showIRValue(x: IRValue): string {
  if (x instanceof ir.Slot) return x.toString()
  if (x instanceof Binding) return `${x.mod}.${x.name}`
  return types.repr(x)
}

function LIR(meta: Def): LIR {
  return new ir.IR<IRValue, never>(meta, _ => ir.unreachable, showIRValue)
}

function toMIR(lir: LIR): MIR {
  const mir = MIR(lir.meta)
  const env = new Map<number, Val<MIR>>()
  const rename = (x: Val<LIR>): Val<MIR> =>
    typeof x === 'number' ? some(env.get(x)) : x as Val<MIR>
  for (const block of lir.blocks()) {
    if (block.id !== 0) mir.newBlock()
    for (const [arg, type] of block.bb.args)
      env.set(arg, mir.block().argument(type))
    for (const [v, st] of block)
      env.set(v, mir.push({ ...st, expr: st.expr.map(rename as any) as any }))
  }
  return mir
}

function source(m: ast.Meta): ir.Source {
  return { file: m.file, line: m.loc.line, col: m.loc.column }
}

function xcall<T>(head: Method | T | number, ...args: (T | number)[]) {
  if (head instanceof Method) return new Invoke<T>(head, args)
  return ir.expr<T>("call", head, ...args)
}
function xtuple<T>(...args: (T | number)[]) {
  return ir.expr("tuple", ...args)
}
function xpack<T>(...args: (T | number)[]) {
  return ir.expr("pack", ...args)
}
function xlist<T>(...args: (T | number)[]) {
  return xpack<T | Tag>(tag("common.List"), ...args)
}
function xpart<T>(x: T | number, i: T | number) {
  return xcall(part_method, x, i)
}

function rcall(code: LIR, f: Val<LIR>, args: Val<LIR>[], { src, bp }: { src?: ast.Meta, bp?: boolean } = {}): Val<LIR> {
  const arglist = _push(code, xlist(...args), { src })
  const result = _push(code, xcall(f, arglist), { src, bp })
  return _push(code, xpart(result, Type(1n)), { src })
}

interface Scope {
  has(x: string): boolean
  get(x: string): ir.Slot | Binding
  set(x: string, v: ir.Slot): Scope
  var(name?: string): Binding | ir.Slot
  swaps(): Map<number, string> | undefined // slight hack; store for `return` lowering
  loops: { kind: 'loop' | 'block', label: string | undefined }[]
}

class Lowering {
  constructor(readonly mod: Tag, readonly sc = GlobalScope(mod)) { }
  scope(swap?: Map<number, string>): Lowering {
    return new Lowering(this.mod, Scope(this.sc, swap))
  }
}

function GlobalScope(mod: Tag): Scope {
  return {
    has: (_: string) => false,
    get: (name: string) => new Binding(mod, name),
    set: (name: string, v: ir.Slot) => { throw new Error(`Cannot set ${name} in global scope`) },
    var: (name: string) => new Binding(mod, name),
    swaps: () => undefined,
    loops: [],
  }
}

function Scope(parent: Scope, swap?: Map<number, string>): Scope {
  const env = new Map<string, ir.Slot>()
  const sc = {
    has: (x: string) => env.has(x) || parent.has(x),
    get: (x: string) => env.get(x) ?? parent.get(x),
    set(x: string, v: ir.Slot) { env.set(x, v); return this },
    var: (name: string) => {
      if (!sc.has(name)) sc.set(name, ir.slot(name))
      return sc.get(name)!
    },
    swaps: () => swap ?? parent.swaps(),
    loops: parent.loops,
  }
  return sc
}

// don't continue lowering after return
// e.g. `f(return 1)`
function _push<T, A>(code: ir.IR<T, A>, x: ir.Expr<T>, { type, src, bp }: { type?: ir.Anno<A>, src?: ast.Meta, bp?: boolean } = {}): T | number {
  if (!code.block().canbranch()) throw new Error("Pushing into finished block")
  return code.push(code.stmt(x, { type, src: src && source(src), bp }))
}

function swapreturn(code: LIR, val: Val<LIR>, swaps?: Map<number, string>, { src, bp }: { src?: ast.Meta, bp?: boolean } = {}): void {
  if (swaps && swaps.size > 0) {
    const maxArgs = Math.max(...swaps.keys())
    const args: Val<LIR>[] = [val]
    for (let i = 1; i <= maxArgs; i++) {
      if (swaps.has(i)) {
        args.push(ir.slot(swaps.get(i)!))
      } else {
        args.push(nil)
      }
    }
    const ret = _push(code, xlist(...args), { src })
    code.return(ret, { src: src && source(src), bp })
  } else {
    code.return(val, { src: src && source(src), bp })
  }
}

function string(code: LIR, x: string) {
  return code.push(code.stmt(xstring(x)))
}

function patternArg(as: string[], name: string): void {
  if (!as.includes(name)) as.push(name)
}

function patternNode(code: LIR, name: string, ...parts: Val<LIR>[]): Val<LIR> {
  return _push(code, xpack(tag(`common.${name}`), ...parts))
}

function lowerPatternIsa(cx: Lowering, code: LIR, ex: ast.Tree, as: string[]): Val<LIR> {
  ex = ex.ungroup()
  if (ex.unwrap() instanceof Symbol) return patternNode(code, 'Trait', lower(cx, code, ex))
  if (ex instanceof ast.Token) return lowerPatternExpr(cx, code, ex, as)
  if (ex.head === 'Index') {
    const params = ex.args.map(x => lower(cx, code, x))
    const trait = _push(code, xpack(tag('common.Params'), ...params))
    return patternNode(code, 'Trait', trait)
  }
  if (ex.head === 'Operator' && ex.args[1].unwrap() === '|')
    return patternNode(code, 'Or', lowerPatternIsa(cx, code, ex.args[0], as), lowerPatternIsa(cx, code, ex.args[2], as))
  if (ex.head === 'Operator' && ex.args[1].unwrap() === '&')
    return patternNode(code, 'And', lowerPatternIsa(cx, code, ex.args[0], as), lowerPatternIsa(cx, code, ex.args[2], as))
  return lowerPatternExpr(cx, code, ex, as)
}

function lowerPatternExpr(cx: Lowering, code: LIR, ex: ast.Tree, as: string[]): Val<LIR> {
  const x = ex.ungroup().unwrap()
  if (x instanceof Symbol) {
    if (x.toString() === '_') return patternNode(code, 'Hole')
    patternArg(as, x.toString())
    return patternNode(code, 'Bind', tag(x.toString()), patternNode(code, 'Hole'))
  }
  if (typeof x === 'string') throw new Error(`Unsupported string literal ${x}`)
  if (ast.isAtom(x)) return patternNode(code, 'Literal', atomValue(x))
  if (x.head === 'List') {
    const parts = x.args.map(x => lowerPatternExpr(cx, code, x, as))
    return patternNode(code, 'Pack', patternNode(code, 'Literal', tag('common.List')), ...parts)
  }
  if (x.head === 'Operator' && x.args[1].unwrap() === ':') {
    const name = asSymbol(x.args[0].unwrap())
    const pat = lowerPatternIsa(cx, code, x.args[2], as)
    if (name.toString() === '_') return pat
    patternArg(as, name.toString())
    return patternNode(code, 'Bind', tag(name.toString()), pat)
  }
  if (x.head === 'Splat') return patternNode(code, 'Repeat', lowerPatternExpr(cx, code, x.args[0], as))
  if (x.head === 'Call') {
    const args = x.args.slice(1).map(x => lowerPatternExpr(cx, code, x, as))
    return patternNode(code, 'Constructor', lower(cx, code, x.args[0]), ...args)
  }
  throw new Error(`Invalid pattern syntax ${x}`)
}

function lowerpattern(cx: Lowering, code: LIR, ex: ast.Tree) {
  const args: string[] = []
  return [lowerPatternExpr(cx, code, ex, args), args] as const
}

function _lowermatch(cx: Lowering, code: LIR, val: Val<LIR>, pattern: Val<LIR>, args: string[], pat: ast.Tree): Val<LIR> {
  const m = rcall(code, tag('common.match'), [val, pattern])
  const isnil = _push(code, xcall(isnil_method, m))
  code.branch(code._blocks.length + 1, [], { when: isnil })
  code.branch(code._blocks.length + 2)
  code.newBlock()
  rcall(code, tag('common.abort'), [string(code, `match failed: ${ast.repr(pat)}`)])
  code.newBlock()
  const matched = _push(code, xcall(notnil_method, m))
  for (const arg of args) {
    _push(code, ir.expr('set', cx.sc.var(arg), rcall(code, tag('common.getkey'), [matched, tag(arg)])))
  }
  return matched
}

function lowermatch(cx: Lowering, code: LIR, val: Val<LIR>, pat: ast.Tree): Val<LIR> {
  const [pattern, args] = lowerpattern(cx, code, pat)
  return _lowermatch(cx, code, val, pattern, args, pat)
}

function lower(cx: Lowering, code: LIR, x: ast.Tree | readonly ast.Tree[], value = true): Val<LIR> {
  if (!(x instanceof ast.Token || x instanceof ast.Expr)) {
    if (x.length === 0) return nil
    x.slice(0, -1).forEach(item => lower(cx, code, item, false))
    return lower(cx, code, x[x.length - 1], value)
  }

  if (x instanceof ast.Token) {
    const val = x.unwrap()
    if (val instanceof Symbol) {
      if (['break', 'continue'].includes(val.toString())) {
        return loopbranch(cx, code, val.toString())
      } else if (val.toString() === 'return') {
        const result = nil
        // TODO debug info
        swapreturn(code, result, cx.sc.swaps(), { bp: true })
        return result
      } else {
        return cx.sc.get(val.toString())
      }
    } else if (typeof val === 'string') {
      return string(code, val)
    } else {
      return atomValue(val)
    }
  }

  if (x instanceof ast.Expr) {
    let [ex, _] = attrs(x)
    ex = ast.asExpr(ex)
    if (ex.head === 'Group') return lower(cx, code, ex.args, value)
    if (ex.head === 'Block') return lowerBlock(cx, code, x, value)
    if (ex.head === 'Operator') return lowerOperator(cx, code, x, value)
    if (ex.head === 'Call') return lowerCall(cx, code, x)
    if (ex.head === 'Index') return lowerIndex(cx, code, x)
    if (ex.head === 'Field') return lowerField(cx, code, x)
    if (ex.head === 'List') return lowerList(cx, code, x)
    if (ex.head === 'Template') return lowerTemplate(cx, code, x)
    if (ex.head === 'Syntax') return lowerSyntax(cx, code, x, value)
    throw new Error(`Unimplemented ast.Expr lowering for head: ${ex.head}`)
  }

  throw new Error(`Unimplemented lowering for: ${x}`)
}

function lowerOperator(cx: Lowering, code: LIR, ex: ast.Expr, value = true): Val<LIR> {
  const [operator, ...args] = ast.callargs(ast.asExpr(ex, 'Operator'))
  const op = asSymbol(operator.unwrap()).toString()
  if (op === '=' && args[0] instanceof ast.Token && args[0].unwrap() instanceof Symbol) {
    // Simple assignment: x = value
    const y = lower(cx, code, args[1])
    // Globals are side effects (if they error) so don't let the SSA transform move them around
    const ySlot = y instanceof Binding ? _push(code, xglobal(y)) : y
    const x = cx.sc.var(args[0].unwrap().toString())
    _push(code, ir.expr('set', x, ySlot))
    return x
  } else if (op === '=' && ast.isExpr(args[0], 'Index')) {
    // Index assignment: xs[i, ...] = x
    const [xs, ...idxs] = ast.asExpr(args[0]).args
    const x = args[1]
    return lower(cx, code, ast.Call(tag('common.set'), ast.Swap(xs), ast.List(...idxs), x).withmeta(ex.meta))
  } else if (op === '=') {
    // Pattern assignment: pat = val
    const pat = args[0]
    const val = lower(cx, code, args[1])
    return lowermatch(cx, code, val, pat)
  } else if (op === '&&' || op === '||') {
    const condVar = gensym('cond')
    const clauses = op === '&&' ? [args[1], condVar] : [condVar, args[1]]
    const letStmt = ast.Block(
      ast.Operator(condVar, symbol('='), args[0]),
      ast.Syntax(symbol('if'), condVar, clauses[0], symbol('else'), clauses[1]))
    return lower(cx, code, letStmt, value)
  } else {
    // General operator call
    const func = lower(cx, code, operator)
    const arglist = _push(code, xlist(...args.map(x => lower(cx, code, x))))
    const result = _push(code, xcall(func, arglist), { src: ex.meta, bp: true })
    return _push(code, xpart(result, Type(1n)), { src: ex.meta })
  }
}

function argtuple(cx: Lowering, code: LIR, args: readonly ast.Tree[], src?: ast.Meta): [Val<LIR>, Map<string, number>] {
  const swaps = new Map<string, number>()
  const parts: Val<LIR>[] = []
  let idx = 1
  let splat = false
  const argQueue = [...args]
  while (argQueue.length > 0) {
    if (ast.isExpr(argQueue[0], 'Splat')) {
      const splatArg = argQueue.shift() as ast.Expr
      parts.push(lower(cx, code, splatArg.args[0]))
      splat = true
    } else {
      const as: Val<LIR>[] = []
      while (argQueue.length > 0 && !ast.isExpr(argQueue[0], 'Splat')) {
        let arg = argQueue.shift()!
        if (ast.isExpr(arg, 'Swap') && !splat) {
          arg = arg.args[0]
          swaps.set(asSymbol(arg.unwrap()).toString(), idx)
        }
        as.push(lower(cx, code, arg))
        idx += 1
      }
      parts.push(_push(code, xlist(...as), { src }))
    }
  }
  const result = parts.length === 0
    ? _push(code, xpack(tag('common.List')))
    : parts.length === 1
      ? parts[0]
      : _push(code, xcall(packcat_method, _push(code, xlist(...parts))))
  return [result, swaps]
}

function lowerCall(cx: Lowering, code: LIR, ex: ast.Expr): Val<LIR> {
  // Handle Field calls: obj.method(...) -> common.method(obj, tag, ...)
  if (ast.isExpr(ex.args[0], 'Field')) {
    const [obj, methodName] = ex.args[0].args
    const callExpr = ast.Call(tag('common.method'), obj, tag(asSymbol(methodName.unwrap()).toString()), ...ex.args.slice(1))
    return lower(cx, code, callExpr.withmeta(ex.meta))
  }
  // Regular function call
  const [args, swaps] = argtuple(cx, code, ex.args.slice(1), ex.meta)
  const func = lower(cx, code, ex.args[0])
  const result = _push(code, xcall(func, args), { src: ex.meta, bp: true })
  const val = _push(code, xpart(result, Type(1n)), { src: ex.meta })
  for (const [name, i] of swaps) {
    _push(code, ir.expr('set', cx.sc.var(name), _push(code, xpart(result, Type(BigInt(i + 1))))))
  }
  return val
}

function lowerIndex(cx: Lowering, code: LIR, ex: ast.Expr): Val<LIR> {
  const [x, ...idxs] = ex.args
  const callExpr = ast.Call(tag('common.get'), x, ast.List(...idxs))
  return lower(cx, code, callExpr.withmeta(ex.meta))
}

function lowerField(cx: Lowering, code: LIR, ex: ast.Expr): Val<LIR> {
  const [obj, fieldName] = ex.args
  const callExpr = ast.Call(tag('common.field'), obj, tag(asSymbol(fieldName.unwrap()).toString()))
  return lower(cx, code, callExpr.withmeta(ex.meta))
}

function lowerList(cx: Lowering, code: LIR, ex: ast.Expr): Val<LIR> {
  // TODO: should use the `list` function, but this puts off the need for special argument inference
  const [args] = argtuple(cx, code, ex.args, ex.meta)
  return args
}

function lowerTemplate(cx: Lowering, code: LIR, ex: ast.Expr): Val<LIR> {
  const template = asSymbol(ex.args[0]).toString()
  if (template === 'tag') {
    const tagName = asString(ex.args[1])
    return modtag(cx.mod, tagName)
  } else if (template === 'bits') {
    const bitString = asString(ex.args[1])
    const value = bitString === '' ? 0n : BigInt('0b' + bitString)
    return bits(bitString.length, value)
  } else if (template === 'c') {
    const val = some(asString(ex.args[1]).codePointAt(0))
    return pack(tag('common.Char'), pack(tag('common.UInt'), bits(21, BigInt(val))))
  } else if (template === 'r') {
    const pattern = asString(ex.args[1])
    return rcall(code, tag('common.Regex'), [string(code, pattern)], { src: ex.meta })
  } else if (template === 'js') {
    const [js, params] = jsinline(ex)
    const args = params.map(name => rcall(code, tag('common.jsref'), [cx.sc.get(name)], { src: ex.meta }))
    const ref = _push(code, xjs(js, params, args), { type: types.Ref, src: ex.meta })
    return rcall(code, tag('common.JSObject'), [ref], { src: ex.meta })
  }
  throw new Error(`Unimplemented template type: ${template}`)
}

const nonNullExternref: ValueType = { null: false, type: { kind: 'abstract', type: AbsHeapType.extern } }

const wtypes = new Map<string, [Type, ValueType[]]>([
  ['i32', [types.bits(32), [i32]]],
  ['i64', [types.bits(64), [i64]]],
  ['f32', [types.float32(), [f32]]],
  ['f64', [types.float64(), [f64]]],
  ['ref', [types.Ref, [externref]]],
  ['ref!', [types.Ref, [nonNullExternref]]]
])

function intrinsic(ex: ast.Tree): [string | [string, string], ir.Anno<Type>, ValueType[]?] {
  let T: ir.Anno<Type> = types.nil
  let ret: ValueType[] | undefined
  if (ast.isExpr(ex, 'Operator') && symbol(':').isEqual(ex.args[1].unwrap())) {
    const type = ex.args[2]
    if (symbol('unreachable').isEqual(type.unwrap())) T = ir.unreachable
    else if (ast.isExpr(type, 'Group')) {
      const specs = type.args.map(t => some(wtypes.get(ast.asSymbol(t).name)))
      T = types.list(...specs.map(s => s[0]))
      ret = specs.flatMap(s => s[1])
    } else {
      [T, ret] = some(wtypes.get(ast.asSymbol(type).name))
    }
    ex = ex.args[0]
  }
  let op = ast.asExpr(ex).args[0].ungroup()
  if (symbol('call').isEqual(op.unwrap())) {
    let name = ast.asExpr(ast.asExpr(ex).args[1], 'Field').args.map(t => t.unwrap().toString())
    return [[name[0], name[1]], T, ret]
  } else {
    const namify = (x: ast.Tree): string =>
      ast.isExpr(x, 'Field') ? x.args.map(namify).join('.') : ast.asSymbol(x).name
    return [namify(op), T, ret]
  }
}

function intrinsic_args(ex: ast.Tree): ast.Tree[] {
  if (ast.isExpr(ex, 'Operator') && symbol(':').isEqual(ex.args[1].unwrap()))
    return intrinsic_args(ex.args[0])
  const e = ast.asExpr(ex)
  const op = e.args[0].ungroup()
  const start = symbol('call').isEqual(op.unwrap()) ? 2 : 1
  return e.args.slice(start).map(x =>
    ast.isExpr(x, 'Operator') && symbol(':').isEqual(x.args[1].unwrap()) ? x.args[0] : x)
}

function jsinline(ex: ast.Expr): [string, string[]] {
  const js = asString(ex.args[1])
  const params: string[] = []
  // TODO don't parse within string/regex/comment
  const code = js.replace(/\\([a-zA-Z_][a-zA-Z0-9_]*)/g, (_, name) => {
    if (!params.includes(name)) params.push(name)
    return `(${name})`
  })
  return [code, params]
}

function lowerSyntax(cx: Lowering, code: LIR, ex: ast.Expr, value = true): Val<LIR> {
  const syntax = asSymbol(ast.asExpr(attrs(ex)[0]).args[0]).toString()
  if (syntax === 'bits') {
    const size = Number(asBigInt(ex.args[1].unwrap()))
    return bits(size, 0n)
  } else if (syntax === 'int') {
    const size = Number(asBigInt(ex.args[1].unwrap()))
    return pack(tag('common.Int'), bits(size, 0n))
  } else if (syntax === 'uint') {
    const size = Number(asBigInt(ex.args[1].unwrap()))
    return pack(tag('common.UInt'), bits(size, 0n))
  } else if (syntax === 'return') {
    const result = lower(cx, code, ex.args[1])
    swapreturn(code, result, cx.sc.swaps(), { src: ex.meta, bp: true })
    return result
  } else if (['break', 'continue'].includes(syntax)) {
    return loopbranch(cx, code, syntax, asSymbol(ex.args[1]).toString(), ex.meta)
  } else if (syntax === 'while') {
    return lowerWhile(cx, code, ex, value)
  } else if (syntax === 'if') {
    return lowerIf(cx, code, parseIf(ex), value)
  } else if (syntax === 'select') {
    return lowerSelect(cx, code, ex, value)
  } else if (syntax === 'wasm') {
    const [op, T, ret] = intrinsic(ex.args[1])
    const args = intrinsic_args(ex.args[1]).map(arg => lower(cx, code, arg))
    return _push(code, new Wasm(op, args, ret), { src: ex.meta, type: T, bp: true })
  } else if (syntax === 'let') {
    return lowerLet(cx, code, ex, value)
  } else {
    throw new Error(`unrecognised syntax: ${syntax}`)
  }
}

function lowerSelect(cx: Lowering, code: LIR, ex: ast.Expr, value = true): Val<LIR> {
  if (ex.args.length !== 2) throw new Error('select expects a single block')
  const cases = ast.asExpr(ex.args[1], 'Block').args.map(parseSelectCase)
  if (cases.length === 0) throw new Error('select requires at least one case')
  const index = lower(cx, code, ast.Call(tag('common.select'), ...cases.map(selectDescriptor)).withmeta(ex.meta))
  const targets: ir.Block<LIR>[] = []
  const values: Val<LIR>[] = []

  const lowerBody = (c: SelectCase): Val<LIR> => {
    const branch = cx.scope()
    const selected = lower(branch, code, ast.Call(tag('common.selectAccept'), c.call.args[0], ...c.call.args.slice(1)))
    if (c.pattern) {
      const pat = c.pattern.ungroup()
      if (pat instanceof ast.Token && pat.unwrap() instanceof Symbol)
        _push(code, ir.expr('set', branch.sc.var(pat.unwrap().toString()), selected))
      else
        lowermatch(branch, code, selected, c.pattern)
    }
    return lower(branch, code, c.body, value)
  }

  for (let i = 0; i < cases.length; i++) {
    const cond = rcall(code, tag('common.=='), [index, Type(BigInt(i + 1))], { src: ex.meta })
    const condBlock = code.block()
    const bodyStart = code.newBlock()
    const bodyValue = lowerBody(cases[i])
    if (value) values.push(bodyValue)
    targets.push(code.block())
    const next = code.newBlock()
    condBlock.branch(bodyStart, [], { when: cond, src: ex.meta && source(ex.meta) })
    condBlock.branch(next)
  }

  rcall(code, tag('common.abort'), [string(code, 'select failed')], { src: ex.meta })
  const after = code.newBlock()
  for (let i = 0; i < targets.length; i++)
    if (targets[i].canbranch())
      if (value) targets[i].branch(after, [values[i]])
      else targets[i].branch(after)
  return value ? after.argument(ir.unreachable) : nil
}

type LetCond = { kind: 'let', ex: ast.Tree }
type IfCondition = ast.Tree | LetCond | true

interface IfStmt {
  cond: IfCondition[]
  body: ast.Tree[]
}

function parseIf(ex: ast.Expr): IfStmt {
  const cond: IfCondition[] = []
  const body: ast.Tree[] = []
  const args = [...ex.args]
  while (args.length > 0) {
    if (symbol('if').isEqual(args[0].unwrap())) {
      args.shift()
      let c: IfCondition = args.shift()!
      if (symbol('let').isEqual(c.unwrap()))
        c = { kind: 'let', ex: args.shift()! }
      cond.push(c)
    } else cond.push(true)
    body.push(args.shift()!)
    if (args.length > 0 && asSymbol(args.shift()!.unwrap()).toString() !== 'else')
      throw new Error('Broken if block - expected else')
  }
  if (cond[cond.length - 1] !== true) {
    cond.push(true)
    body.push(ast.Call(symbol("pack"), tag("common.Nil")))
  }
  return { cond, body }
}

function sentinel(id: number) {
  return [-(id * 2 + 1), -(id * 2 + 2)]
}

function loopbranch(cx: Lowering, code: LIR, kind: string, label?: string, meta?: ast.Meta): Val<LIR> {
  const id = label ?
    cx.sc.loops.findIndex(l => l.label === label) :
    cx.sc.loops.map((l, i) => l.kind === 'loop' ? i : -1).filter(i => i >= 0).pop() ?? -1
  if (id < 0) throw new Error('no loop in scope')
  const [brk, cnt] = sentinel(id)
  code.branch(kind === 'break' ? brk : cnt, [], meta && { src: source(meta) })
  return nil
}

function rewriteJumps(cx: Lowering, code: LIR, header: [number, Val<LIR>[]], after: [number, Val<LIR>[]]): void {
  const [brk, cnt] = sentinel(cx.sc.loops.length - 1)
  for (const block of code.blocks()) {
    for (const [v, st] of block) {
      if (!(st.expr instanceof ir.Branch)) continue
      if (st.expr.target === brk)
        code.setStmt(v, { ...st, expr: new ir.Branch(...after, st.expr.when) })
      else if (st.expr.target === cnt)
        code.setStmt(v, { ...st, expr: new ir.Branch(...header, st.expr.when) })
    }
  }
}

function lowerWhile(cx: Lowering, code: LIR, _ex: ast.Expr, value = true): Val<LIR> {
  cx = cx.scope()
  let [ex, as] = attrs(_ex)
  const label = as.get('label')?.[0]?.toString()
  ex = ast.asExpr(ex)
  const prevBlock = code.block()
  const header = code.newBlock()
  prevBlock.branch(header, value ? [types.list()] : [])
  let out = value ? header.argument(ir.unreachable) : nil as Val<LIR>
  let ret = out
  const cond = lower(cx, code, ex.args[1])
  const condResult = rcall(code, tag('common.condition'), [cond], { src: ex.meta })
  const condBlock = code.block()
  const bodyStart = code.newBlock()
  cx.sc.loops.push({ kind: 'loop', label })
  const val = lower(cx, code, ast.asExpr(ex.args[2], 'Block'), value)
  if (value) out = rcall(code, tag('common.append'), [out, val])
  const bodyEnd = code.block()
  const after = code.newBlock()
  rewriteJumps(cx, code, [header.id + 1, value ? [out] : []], [after.id + 1, []])
  condBlock.branch(bodyStart, [], { when: condResult, src: ex.meta && source(ex.meta) })
  condBlock.branch(after, [], { src: ex.args[0].meta && source(ex.args[0].meta) })
  if (bodyEnd.canbranch()) bodyEnd.branch(header, value ? [out] : [])
  cx.sc.loops.pop()
  return ret
}

function lowerBlock(cx: Lowering, code: LIR, _ex: ast.Expr, value = true): Val<LIR> {
  let [ex, as] = attrs(_ex)
  const label = as.get('label')?.[0]?.toString()
  ex = ast.asExpr(ex)
  if (!label) return lower(cx.scope(), code, ex.args, value)
  if (value) throw new Error('not implemented')
  const prevBlock = code.block()
  const header = code.newBlock()
  prevBlock.branch(header, [])
  cx.sc.loops.push({ kind: 'block', label })
  lower(cx.scope(), code, ex.args, value)
  const bodyEnd = code.block()
  const after = code.newBlock()
  rewriteJumps(cx, code, [header.id + 1, []], [after.id + 1, []])
  if (bodyEnd.canbranch()) bodyEnd.branch(after)
  cx.sc.loops.pop()
  return nil
}

function lowerLet(cx: Lowering, code: LIR, _ex: ast.Expr, value = true): Val<LIR> {
  let [ex, as] = attrs(_ex)
  ex = ast.asExpr(ex)
  const assignments = ex.args.slice(1, -1).map(x => ast.asExpr(x, 'Operator'))
  if (!assignments.every(x => asSymbol(x.args[1].unwrap()).toString() === '='))
    throw new Error('let statement: all assignments must be of the form (= var val)')
  const pats = assignments.map(x => x.args[0])
  const vals = assignments.map(x => lower(cx, code, x.args[2]))
  cx = cx.scope()
  for (let i = 0; i < pats.length; i++) {
    const pat = pats[i].ungroup()
    if (pat instanceof ast.Token && pat.unwrap() instanceof Symbol) {
      const name = pat.unwrap().toString()
      cx.sc.set(name, ir.slot(gensym(name).toString()))
      _push(code, ir.expr('set', cx.sc.get(name), vals[i]))
    } else {
      const [pattern, args] = lowerpattern(cx, code, pats[i])
      for (const arg of args) cx.sc.set(arg, ir.slot(gensym(arg).toString()))
      _lowermatch(cx, code, vals[i], pattern, args, pats[i])
    }
  }
  return lower(cx, code, withAttrs(ex.args[ex.args.length - 1], as), value)
}

function lowerIf(cx: Lowering, code: LIR, ex: IfStmt, value = true): Val<LIR> {
  cx = cx.scope()
  const ts: ir.Block<LIR>[] = []
  const vs: Val<LIR>[] = []
  const body = (ir: LIR, ex: ast.Tree): void => {
    if (value) vs.push(lower(cx, ir, ex))
    else lower(cx, ir, ex, false)
  }
  for (let i = 0; i < ex.cond.length; i++) {
    const cond = ex.cond[i]
    const bodyExpr = ex.body[i]
    if (cond === true) {
      body(code, bodyExpr)
      ts.push(code.block())
      code.newBlock()
      break
    }
    if ('kind' in cond && cond.kind === 'let') {
      const [patternExpr, _, valueExpr] = ast.asExpr(cond.ex).args
      const val = lower(cx, code, valueExpr)
      const [pattern, args] = lowerpattern(cx, code, patternExpr)
      let match = rcall(code, tag('common.match'), [val, pattern])
      const isnil = _push(code, xcall(isnil_method, match))
      const c = code.block()
      const t = code.newBlock()
      match = _push(code, xcall(notnil_method, match))
      for (const arg of args) {
        _push(code, ir.expr('set', cx.sc.var(arg), rcall(code, tag('common.getkey'), [match, tag(arg)])))
      }
      body(code, bodyExpr)
      ts.push(code.block())
      const f = code.newBlock()
      c.branch(f, [], { when: isnil })
      c.branch(t, [])
    } else {
      const condVal = lower(cx, code, cond as ast.Tree)
      const condResult = rcall(code, tag('common.condition'), [condVal])
      const c = code.block()
      const t = code.newBlock()
      body(code, bodyExpr)
      ts.push(code.block())
      const f = code.newBlock()
      c.branch(t, [], { when: condResult })
      c.branch(f)
    }
  }
  const b = code.block()
  for (let i = 0; i < ts.length; i++) {
    if (ts[i].canbranch())
      if (value) ts[i].branch(b, [vs[i]])
      else ts[i].branch(b)
  }
  if (value) return b.argument(ir.unreachable)
  else return nil
}

function lowerfn(mod: Tag, sig: Signature, body: ast.Tree, meta: Def): MIR {
  body = expand(body)
  const cx = new Lowering(mod).scope(sig.swap)
  const code = LIR(meta)
  for (const arg of sig.args) {
    const slot = ir.slot(arg)
    cx.sc.set(arg, slot)
    _push(code, ir.expr('set', slot, code.argument(ir.unreachable)))
  }
  const out = lower(cx, code, body)
  if (code.block().canbranch()) swapreturn(code, out, sig.swap)
  return toMIR(globals(prune(ssa(fuseblocks(code)))))
}

function assignments(code: LIR): Set<string> {
  const locals = new Set<string>()
  for (const block of code.blocks())
    for (const [_, stmt] of block)
      if (stmt.expr.head === 'set' && stmt.expr.body[0] instanceof ir.Slot)
        locals.add(stmt.expr.body[0].name)
  return locals
}

// Assumes all globals are in the same module
function rewriteGlobals(code: LIR, cx: Module): [LIR, Set<string>] {
  const globals = new Set<string>()
  const locals = Array.from(assignments(code)).filter(x => cx.has(x))
  const pr = new ir.Pipe(code)
  locals.forEach(x => pr.push(pr.stmt(ir.expr('set', ir.slot(x), new Binding(cx.name, x)))))
  for (const [v, st] of pr) {
    // Global loads use the new slot
    let ex = st.expr.map((x: Val<LIR>) => x instanceof Binding && (locals.includes(x.name) || globals.has(x.name)) ? ir.slot(x.name) : x)
    // Global stores use the new slot
    if (ex.head === 'set' && ex.body[0] instanceof Binding) {
      globals.add(ex.body[0].name)
      ex = ir.expr('set', ir.slot(ex.body[0].name), ex.body[1])
    }
    pr.set(v, ex)
  }
  [...locals, ...globals].forEach(x => pr.push(pr.stmt(xset(new Binding(cx.name, x), ir.slot(x)))))
  return [pr.finish(), globals]
}

function assigned_globals(code: MIR): Map<Binding, Type> {
  const out = new Map<Binding, Type>()
  for (const [_, st] of code)
    if (st.expr instanceof SetGlobal)
      out.set(st.expr.binding, ir.asType(code.type(st.expr.value)))
  return out
}

function lower_toplevel(mod: Module, ex: ast.Tree, meta: Def): [MIR, Set<string>] {
  ex = expand(ex)
  const cx = new Lowering(mod.name)
  const code = LIR(meta)
  lower(cx, code, ex, false)
  code.return(nil)
  let [code2, defs] = rewriteGlobals(code, mod)
  return [toMIR(globals(prune(ssa(fuseblocks(code2))))), defs]
}

// Turn global references into explicit load instructions
function globals(code: LIR): LIR {
  const pr = new ir.Pipe(code)
  const transform = (x: Val<LIR>): Val<LIR> =>
    x instanceof Binding ? pr.push(pr.stmt(xglobal(x))) : x
  for (const [v, st] of pr) {
    const ex = st.expr
    if (ex.head === 'global') continue
    pr.delete(v)
    pr.replace(v, pr.push({ ...st, expr: ex.map(transform) }))
  }
  return pr.finish()
}

class Lowered implements Caching {
  readonly irs: Cache<Method, MIR>

  constructor(readonly sources: Modules) {
    this.irs = new Cache<Method, MIR>(method => this.lower(method))
  }

  get subcaches() { return [this.irs] }

  ir(method: Method): MIR {
    return this.irs.get(method)
  }

  private lower(method: Method): MIR {
    const source = this.sources.source(method)
    if (source.kind === 'ir') return source.body
    return lowerfn(method.mod, method.sig, source.body, source.meta)
  }
}
