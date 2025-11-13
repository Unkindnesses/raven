import isEqual from "lodash/isEqual"
import * as ast from "./ast"
import * as ir from "../utils/ir"
import { Val, fuseblocks, prune, ssa } from "../utils/ir"
import { asSymbol, asString, Symbol, symbol, gensym, token } from "./ast"
import * as types from "./types"
import { Type, Tag, tag, pack, bits, nil } from "./types"
import { Module, Signature, Binding, MIR, xstring, Method, xglobal, xset, SetGlobal, Invoke, Wasm } from "./modules"
import { Def } from "../dwarf"
import { asBigInt, some } from "../utils/map"
import { binding, options } from "../utils/options"
import * as parse from "./parse"
import { isnil_method, notnil_method, part_method, packcat_method } from "../middle/primitives"
import { lowerpattern, modtag, patternType } from "./patterns"

export {
  lower_toplevel, bundlemacro, lowerfn, source,
  globals, assigned_globals, xlist, xpart, xcall, xtuple, annos
}

// Built-in macros

const s = symbol

function namify(x: ast.Tree, suffix = ""): ast.Expr | ast.Symbol {
  if (x.unwrap() instanceof Symbol) return ast.symbol(x.unwrap().toString() + suffix)
  if (ast.isExpr(x, 'Operator')) return namify(x.args[1], suffix)
  if (ast.isExpr(x, 'Splat')) return ast.Splat(namify(x.args[0], suffix))
  throw new Error(`Unsupported namify argument ${ast.repr(x)}`)
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
          ast.Operator(s(':'), s('val'), ast.Call(tag('common.core.pack'), T, ...argNames))),
        ast.Block(ast.Call(s('Some'), s('val')))))
    body.push(
      ast.Syntax(s('fn'), ast.Call(tag('common.constructorPattern'), T, ...argNames),
        ast.Block(
          ast.Call(tag('common.Pack'),
            ast.Call(tag('common.Literal'), T), ...argNames))))
    if (hasSplat) continue
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
    const comps = lhs.map((l, i) => ast.Operator(s('=='), l, rhs[i]))
    const eqBody = comps.length === 0 ? s('true') : comps.reduce((a, b) => ast.Operator(s('&&'), a, b))
    body.push(
      ast.Syntax(s('fn'),
        ast.Call(tag('common.=='), ast.Call(name, ...lhs), ast.Call(name, ...rhs)),
        ast.Block(eqBody)))
  }
  if (superSpec) {
    const superTag = ast.Template(symbol('tag'), `.${asSymbol(superSpec.unwrap())}`)
    body.push(ast.Operator(symbol('='), superSpec, superTag))
    body.push(
      ast.Syntax(s('fn'),
        ast.Call(tag('common.matchTrait'), superTag,
          ast.Operator(symbol(':'), s('val'),
            ast.Operator(symbol('|'), ...names))),
        ast.Block(ast.Call(s('Some'), s('val')))))
  }
  return ast.Group(...body)
}

function annos(x: ast.Tree, as = new Map<string, ast.Tree[]>()): [ast.Tree, Map<string, ast.Tree[]>] {
  if (!ast.isExpr(x, 'Annotation')) return [x, as]
  const name = asSymbol(x.args[0]).toString()
  const args = x.args.slice(1, -1)
  as.set(name, args)
  const [inner] = annos(x.args[x.args.length - 1], as)
  return [inner, as]
}

function withAnnos(target: ast.Tree, as: Map<string, ast.Tree[]>): ast.Tree {
  let result = target
  for (const [name, args] of [...as].reverse()) {
    result = ast.Annotation(symbol(name), ...args, result)
  }
  return result
}

function formacro(ex: ast.Expr): ast.Expr {
  let [forExpr, as] = annos(ex)
  forExpr = ast.asExpr(forExpr, 'Syntax')
  const assign = ast.asExpr(forExpr.args[1], 'Operator')
  if (!isEqual(assign.args[0].unwrap(), symbol('=')))
    throw new Error('for syntax expects `=` assignment')
  const [x, xs, body] = [assign.args[1], assign.args[2], forExpr.args[2]]
  const [itr, val] = [gensym("itr"), gensym("val")]
  return ast.Block(
    ast.Operator(s("="), itr, ast.Call(tag("common.iterator"), xs)),
    withAnnos(ast.Syntax(s("while"), s("true"), ast.Block(
      ast.Operator(s("="), val, ast.Call(tag("common.next"), ast.Swap(itr))),
      ast.Syntax(s("if"), ast.Call(symbol("nil?"), val), ast.Block(s("break"))),
      ast.Syntax(s("let"),
        ast.Operator(s("="), x, ast.Call(tag("common.core.part"), ast.Call(tag("common.core.notnil"), val), 1n)),
        ast.asExpr(body, 'Block')))), as))
}

function matchmacro(ex: ast.Expr): ast.Expr {
  const val = asSymbol(ex.args[1].unwrap())
  const clauses = ast.asExpr(ex.args[2], 'Block').args
  const body: ast.Tree[] = [token(s('if'))]
  for (let i = 0; i < clauses.length; i++) {
    const clause = clauses[i]
    if (i > 0) body.push(token(s('else')), token(s('if')))
    if (!(ast.isExpr(clause, 'Syntax') && asSymbol(clause.args[0].unwrap()).toString() == 'let'))
      throw new Error('matchmacro: clause must be a let')
    body.push(token(s('let')), ast.Operator(s('='), clause.args[1], val), ast.asExpr(clause.args[2], 'Block'))
  }
  body.push(token(s('else')), ast.Block(ast.Call(tag('common.abort'), "Match clause failed")))
  return ast.Syntax(...body)
}

function showmacro(ex: ast.Expr, pack = false): ast.Expr {
  const arg = ex.args[1]
  const name = gensym()
  return ast.Block(
    ast.Operator(s('='), name, arg),
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

const macros = new Map<string, (ex: ast.Expr) => ast.Tree>([
  ['bundle', bundlemacro],
  ['for', formacro],
  ['show', showmacro],
  ['showPack', ex => showmacro(ex, true)],
  ['test', testmacro],
  ['match', matchmacro],
])

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
  mod(): Tag
  swaps(): Map<number, string> | undefined // slight hack; store for `return` lowering
  loops: { kind: 'loop' | 'block', label: string | undefined }[]
}

function GlobalScope(mod: Tag): Scope {
  return {
    has: (_: string) => false,
    get: (name: string) => new Binding(mod, name),
    set: (name: string, v: ir.Slot) => { throw new Error(`Cannot set ${name} in global scope`) },
    var: (name: string) => new Binding(mod, name),
    mod: () => mod,
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
    mod: () => parent.mod(),
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

function string(sc: Scope, code: LIR, x: string) {
  return code.push(code.stmt(xstring(x)))
}

function lowermatch(sc: Scope, code: LIR, val: Val<LIR>, pat: ast.Tree): Val<LIR> {
  const sig = lowerpattern(pat, sc.mod(), resolve_static)
  const pattern = patternType(sig.pattern)
  const m = rcall(code, tag('common.match'), [val, pattern])
  const isnil = _push(code, xcall(isnil_method, m))
  code.branch(code._blocks.length + 1, [], { when: isnil })
  code.branch(code._blocks.length + 2)
  code.newBlock()
  rcall(code, tag('common.abort'), [string(sc, code, `match failed: ${ast.repr(pat)}`)])
  code.newBlock()
  const matched = _push(code, xcall(notnil_method, m))
  for (const arg of sig.args) {
    _push(code, ir.expr('set', sc.var(arg), rcall(code, tag('common.getkey'), [matched, tag(arg)])))
  }
  return matched
}

function lower(sc: Scope, code: LIR, x: ast.Tree | ast.Tree[], value = true): Val<LIR> {
  if (Array.isArray(x)) {
    if (x.length === 0) return nil
    x.slice(0, -1).forEach(item => lower(sc, code, item, false))
    return lower(sc, code, x[x.length - 1], value)
  }

  if (x instanceof ast.Token) {
    const val = x.unwrap()
    if (val instanceof Symbol) {
      if (['break', 'continue'].includes(val.toString())) {
        return loopbranch(sc, code, val.toString())
      } else if (val.toString() === 'return') {
        const result = nil
        // TODO debug info
        swapreturn(code, result, sc.swaps(), { bp: true })
        return result
      } else {
        return sc.get(val.toString())
      }
    } else if (typeof val === 'string') {
      return string(sc, code, val)
    } else {
      return Type(val)
    }
  }

  if (x instanceof ast.Expr) {
    let [ex, _] = annos(x)
    ex = ast.asExpr(ex)
    if (ex.head === 'Group') return lower(sc, code, ex.args, value)
    if (ex.head === 'Block') return lowerBlock(sc, code, x, value)
    if (ex.head === 'Operator') return lowerOperator(sc, code, x, value)
    if (ex.head === 'Call') return lowerCall(sc, code, x)
    if (ex.head === 'Index') return lowerIndex(sc, code, x)
    if (ex.head === 'Field') return lowerField(sc, code, x)
    if (ex.head === 'List') return lowerList(sc, code, x)
    if (ex.head === 'Template') return lowerTemplate(sc, code, x)
    if (ex.head === 'Syntax') return lowerSyntax(sc, code, x, value)
    throw new Error(`Unimplemented ast.Expr lowering for head: ${ex.head}`)
  }

  throw new Error(`Unimplemented lowering for: ${x}`)
}

function lowerOperator(sc: Scope, code: LIR, ex: ast.Expr, value = true): Val<LIR> {
  const op = asSymbol(ex.args[0].unwrap()).toString()
  if (op === '=' && ex.args[1] instanceof ast.Token && ex.args[1].unwrap() instanceof Symbol) {
    // Simple assignment: x = value
    const y = lower(sc, code, ex.args[2])
    // Globals are side effects (if they error) so don't let the SSA transform move them around
    const ySlot = y instanceof Binding ? _push(code, xglobal(y)) : y
    const x = sc.var(ex.args[1].unwrap().toString())
    _push(code, ir.expr('set', x, ySlot))
    return x
  } else if (op === '=' && ast.isExpr(ex.args[1], 'Index')) {
    // Index assignment: xs[i] = x
    const [xs, i] = ast.asExpr(ex.args[1]).args
    const x = ex.args[2]
    return lower(sc, code, ast.Call(tag('common.set'), ast.Swap(xs), x, i).withmeta(ex.meta))
  } else if (op === '=') {
    // Pattern assignment: pat = val
    const pat = ex.args[1]
    const val = lower(sc, code, ex.args[2])
    return lowermatch(sc, code, val, pat)
  } else if (op === '&&' || op === '||') {
    const condVar = gensym('cond')
    const clauses = op === '&&' ? [ex.args[2], condVar] : [condVar, ex.args[2]]
    const letStmt = ast.Block(
      ast.Operator(symbol('='), condVar, ex.args[1]),
      ast.Syntax(symbol('if'), condVar, clauses[0], symbol('else'), clauses[1]))
    return lower(sc, code, letStmt, value)
  } else {
    // General operator call
    const func = lower(sc, code, ex.args[0])
    const args = _push(code, xlist(...ex.args.slice(1).map(x => lower(sc, code, x))))
    const result = _push(code, xcall(func, args), { src: ex.meta, bp: true })
    return _push(code, xpart(result, Type(1n)), { src: ex.meta })
  }
}

function argtuple(sc: Scope, code: LIR, args: ast.Tree[], src?: ast.Meta): [Val<LIR>, Map<string, number>] {
  const swaps = new Map<string, number>()
  const parts: Val<LIR>[] = []
  let idx = 1
  let splat = false
  const argQueue = [...args]
  while (argQueue.length > 0) {
    if (ast.isExpr(argQueue[0], 'Splat')) {
      const splatArg = argQueue.shift() as ast.Expr
      parts.push(lower(sc, code, splatArg.args[0]))
      splat = true
    } else {
      const as: Val<LIR>[] = []
      while (argQueue.length > 0 && !ast.isExpr(argQueue[0], 'Splat')) {
        let arg = argQueue.shift()!
        if (ast.isExpr(arg, 'Swap') && !splat) {
          arg = arg.args[0]
          swaps.set(asSymbol(arg.unwrap()).toString(), idx)
        }
        as.push(lower(sc, code, arg))
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

function lowerCall(sc: Scope, code: LIR, ex: ast.Expr): Val<LIR> {
  // Handle Field calls: obj.method(...) -> common.method(obj, tag, ...)
  if (ast.isExpr(ex.args[0], 'Field')) {
    const [obj, methodName] = ex.args[0].args
    const callExpr = ast.Call(tag('common.method'), obj, tag(asSymbol(methodName.unwrap()).toString()), ...ex.args.slice(1))
    return lower(sc, code, callExpr.withmeta(ex.meta))
  }
  // Regular function call
  const [args, swaps] = argtuple(sc, code, ex.args.slice(1), ex.meta)
  const func = lower(sc, code, ex.args[0])
  const result = _push(code, xcall(func, args), { src: ex.meta, bp: true })
  const val = _push(code, xpart(result, Type(1n)), { src: ex.meta })
  for (const [name, i] of swaps) {
    _push(code, ir.expr('set', sc.var(name), _push(code, xpart(result, Type(BigInt(i + 1))))))
  }
  return val
}

function lowerIndex(sc: Scope, code: LIR, ex: ast.Expr): Val<LIR> {
  const callExpr = ast.Call(tag('common.get'), ...ex.args)
  return lower(sc, code, callExpr.withmeta(ex.meta))
}

function lowerField(sc: Scope, code: LIR, ex: ast.Expr): Val<LIR> {
  const [obj, fieldName] = ex.args
  const callExpr = ast.Call(tag('common.field'), obj, tag(asSymbol(fieldName.unwrap()).toString()))
  return lower(sc, code, callExpr.withmeta(ex.meta))
}

function lowerList(sc: Scope, code: LIR, ex: ast.Expr): Val<LIR> {
  // TODO: should use the `list` function, but this puts off the need for special argument inference
  const [args] = argtuple(sc, code, ex.args, ex.meta)
  return args
}

function lowerTemplate(sc: Scope, code: LIR, ex: ast.Expr): Val<LIR> {
  const templateType = asSymbol(ex.args[0]).toString()
  if (templateType === 'tag') {
    const tagName = asString(ex.args[1])
    return modtag(sc.mod(), tagName)
  } else if (templateType === 'bits') {
    const bitString = asString(ex.args[1])
    const value = bitString === '' ? 0n : BigInt('0b' + bitString)
    return bits(bitString.length, value)
  }
  throw new Error(`Unimplemented template type: ${templateType}`)
}

const wtypes = new Map<string, Type>([
  ['i32', types.bits(32)],
  ['i64', types.bits(64)],
  ['f32', types.float32()],
  ['f64', types.float64()],
  ['externref', types.Ref]])

function wtype(name: string): Type {
  if (name === 'ref') return types.Ref
  return some(wtypes.get(name))
}

function intrinsic(ex: ast.Tree): [string | [string, string], ir.Anno<Type>] {
  let T: ir.Anno<Type> = types.nil
  if (ast.isExpr(ex, 'Operator') && isEqual(ex.args[0].unwrap(), ast.symbol(':'))) {
    const type = ex.args[2]
    if (isEqual(type.unwrap(), ast.symbol('unreachable'))) T = ir.unreachable
    else if (ast.isExpr(type, 'Group'))
      T = types.list(...type.args.map(t => some(wtype(ast.asSymbol(t).name))))
    else T = some(wtype(ast.asSymbol(type).name))
    ex = ex.args[1]
  }
  let op = ast.asExpr(ex).args[0].ungroup()
  if (isEqual(op.unwrap(), ast.symbol('call'))) {
    let name = ast.asExpr(ast.asExpr(ex).args[1], 'Field').args.map(t => t.unwrap().toString())
    return [[name[0], name[1]], T]
  } else if (isEqual(op, parse.expr('global.get'))) {
    throw new Error('not supported')
  } else {
    const namify = (x: ast.Tree): string =>
      ast.isExpr(x, 'Field') ? x.args.map(namify).join('.') : ast.asSymbol(x).name
    return [namify(op), T]
  }
}

function intrinsic_args(ex: ast.Tree): ast.Tree[] {
  if (ast.isExpr(ex, 'Operator') && isEqual(ex.args[0].unwrap(), ast.symbol(':')))
    return intrinsic_args(ex.args[1])
  const args = ast.asExpr(ex).args.slice(1).filter(x =>
    ast.isExpr(x, 'Operator') && isEqual(x.args[0].unwrap(), ast.symbol(':'))) as ast.Expr[]
  return args.map(x => x.args[1])
}

function lowerSyntax(sc: Scope, code: LIR, ex: ast.Expr, value = true): Val<LIR> {
  const syntax = asSymbol(ast.asExpr(annos(ex)[0]).args[0]).toString()
  if (syntax === 'bits') {
    const size = Number(asBigInt(ex.args[1].unwrap()))
    return bits(size, 0n)
  } else if (syntax === 'int') {
    const size = Number(asBigInt(ex.args[1].unwrap()))
    return pack(tag('common.Int'), bits(size, 0n))
  } else if (syntax === 'return') {
    const result = lower(sc, code, ex.args[1])
    swapreturn(code, result, sc.swaps(), { src: ex.meta, bp: true })
    return result
  } else if (['break', 'continue'].includes(syntax)) {
    return loopbranch(sc, code, syntax, asSymbol(ex.args[1]).toString(), ex.meta)
  } else if (syntax === 'while') {
    return lowerWhile(sc, code, ex, value)
  } else if (syntax === 'if') {
    return lowerIf(sc, code, parseIf(ex), value)
  } else if (syntax === 'wasm') {
    const wasmExpr = ast.asExpr(ex.args[1], 'Block').args[0]
    const [op, ret] = intrinsic(wasmExpr)
    const args = intrinsic_args(wasmExpr).map(arg => lower(sc, code, arg))
    return _push(code, new Wasm(op, args), { src: ex.meta, type: ret, bp: true })
  } else if (syntax === 'let') {
    return lowerLet(sc, code, ex, value)
  } else if (macros.has(syntax)) {
    const macro = macros.get(syntax)!
    const expanded = macro(ex)
    return lower(sc, code, expanded, value)
  } else {
    throw new Error(`unrecognised syntax: ${syntax}`)
  }
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
    if (isEqual(args[0].unwrap(), symbol('if'))) {
      args.shift()
      let c: IfCondition = args.shift()!
      if (isEqual(c.unwrap(), symbol('let')))
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

function loopbranch(sc: Scope, code: LIR, kind: string, label?: string, meta?: ast.Meta): Val<LIR> {
  const id = label ?
    sc.loops.findIndex(l => l.label === label) :
    sc.loops.map((l, i) => l.kind === 'loop' ? i : -1).filter(i => i >= 0).pop() ?? -1
  if (id < 0) throw new Error('no loop in scope')
  const [brk, cnt] = sentinel(id)
  code.branch(kind === 'break' ? brk : cnt, [], meta && { src: source(meta) })
  return nil
}

function rewriteJumps(sc: Scope, code: LIR, header: [number, Val<LIR>[]], after: [number, Val<LIR>[]]): void {
  const [brk, cnt] = sentinel(sc.loops.length - 1)
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

function lowerWhile(sc: Scope, code: LIR, _ex: ast.Expr, value = true): Val<LIR> {
  sc = Scope(sc)
  let [ex, as] = annos(_ex)
  const label = as.get('label')?.[0]?.toString()
  ex = ast.asExpr(ex)
  const prevBlock = code.block()
  const header = code.newBlock()
  prevBlock.branch(header, value ? [types.list()] : [])
  let out = value ? header.argument(ir.unreachable) : nil as Val<LIR>
  let ret = out
  const cond = lower(sc, code, ex.args[1])
  const condResult = rcall(code, tag('common.condition'), [cond], { src: ex.meta })
  const condBlock = code.block()
  const bodyStart = code.newBlock()
  sc.loops.push({ kind: 'loop', label })
  const val = lower(sc, code, ast.asExpr(ex.args[2], 'Block'), value)
  if (value) out = rcall(code, tag('common.append'), [out, val])
  const bodyEnd = code.block()
  const after = code.newBlock()
  rewriteJumps(sc, code, [header.id + 1, value ? [out] : []], [after.id + 1, []])
  condBlock.branch(bodyStart, [], { when: condResult, src: ex.meta && source(ex.meta) })
  condBlock.branch(after, [], { src: ex.args[0].meta && source(ex.args[0].meta) })
  if (bodyEnd.canbranch()) bodyEnd.branch(header, value ? [out] : [])
  sc.loops.pop()
  return ret
}

function lowerBlock(sc: Scope, code: LIR, _ex: ast.Expr, value = true): Val<LIR> {
  let [ex, as] = annos(_ex)
  const label = as.get('label')?.[0]?.toString()
  ex = ast.asExpr(ex)
  if (!label) return lower(Scope(sc), code, ex.args, value)
  if (value) throw new Error('not implemented')
  const prevBlock = code.block()
  const header = code.newBlock()
  prevBlock.branch(header, [])
  sc.loops.push({ kind: 'block', label })
  lower(Scope(sc), code, ex.args, value)
  const bodyEnd = code.block()
  const after = code.newBlock()
  rewriteJumps(sc, code, [header.id + 1, []], [after.id + 1, []])
  if (bodyEnd.canbranch()) bodyEnd.branch(after)
  sc.loops.pop()
  return nil
}

// TODO support pattern matching
function lowerLet(sc: Scope, code: LIR, _ex: ast.Expr, value = true): Val<LIR> {
  let [ex, as] = annos(_ex)
  ex = ast.asExpr(ex)
  const assignments = ex.args.slice(1, -1)
  if (!assignments.every(x => ast.isExpr(x, 'Operator') && asSymbol(x.args[0].unwrap()).toString() === '='))
    throw new Error('let statement: all assignments must be of the form (= var val)')
  const vars = assignments.map(x => asSymbol(ast.asExpr(x, 'Operator').args[1]).toString())
  const vals = assignments.map(x => lower(sc, code, ast.asExpr(x, 'Operator').args[2]))
  sc = Scope(sc)
  for (const v of vars) sc.set(v, ir.slot(gensym(v).toString()))
  for (let i = 0; i < vars.length; i++)
    _push(code, ir.expr('set', sc.get(vars[i]), vals[i]))
  return lower(sc, code, withAnnos(ex.args[ex.args.length - 1], as), value)
}

function lowerIf(sc: Scope, code: LIR, ex: IfStmt, value = true): Val<LIR> {
  sc = Scope(sc)
  const ts: ir.Block<LIR>[] = []
  const vs: Val<LIR>[] = []
  const body = (ir: LIR, ex: ast.Tree): void => {
    if (value) vs.push(lower(sc, ir, ex))
    else lower(sc, ir, ex, false)
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
      const [_, patternExpr, valueExpr] = ast.asExpr(cond.ex).args
      const val = lower(sc, code, valueExpr)
      const sig = lowerpattern(patternExpr, sc.mod(), resolve_static)
      const pat = patternType(sig.pattern)
      const match = rcall(code, tag('common.match'), [val, pat])
      const isnil = _push(code, xcall(isnil_method, match))
      const c = code.block()
      const t = code.newBlock()
      for (const arg of sig.args) {
        _push(code, ir.expr('set', sc.var(arg), rcall(code, tag('common.getkey'), [match, tag(arg)])))
      }
      body(code, bodyExpr)
      ts.push(code.block())
      const f = code.newBlock()
      c.branch(f, [], { when: isnil })
      c.branch(t, [])
    } else {
      const condVal = lower(sc, code, cond as ast.Tree)
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

// This is only used (hackily) by `lowermatch!`, so we avoid cluttering the code.
// TODO: remove the need for `resolve` in lowering entirely
const [withResolve, getResolve] = binding<(x: Symbol) => Type>('resolve')
const resolve_static = (x: Symbol): Type => getResolve()(x)

function lowerfn(mod: Tag, sig: Signature, body: ast.Tree, resolver: (x: Symbol) => Type, meta: Def): MIR {
  const sc = Scope(GlobalScope(mod), sig.swap)
  const code = LIR(meta)
  for (const arg of sig.args) {
    const slot = ir.slot(arg)
    sc.set(arg, slot)
    _push(code, ir.expr('set', slot, code.argument(ir.unreachable)))
  }
  const out = withResolve(resolver, () => lower(sc, code, body))
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
      out.set(st.expr.binding, ir.asType(st.type))
  return out
}

function lower_toplevel(mod: Module, ex: ast.Tree, resolve: (x: Symbol) => Type, meta: Def): [MIR, Set<string>] {
  const sc = GlobalScope(mod.name)
  const code = LIR(meta)
  withResolve(resolve, () => { lower(sc, code, ex, false) })
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
