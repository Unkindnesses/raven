import isEqual from "lodash/isEqual"
import * as ast from "./ast"
import * as ir from "../utils/ir"
import { fuseblocks, prune, ssa, renumber } from "../utils/ir"
import { asSymbol, asString, asNumber, Symbol, symbol, gensym, token } from "./ast"
import * as types from "./types"
import { Type, Tag, tag, pack, bits, int32, asType } from "./types"
import { Module, Signature, Binding, FuncInfo, IRValue, WIntrinsic, MIR, WImport } from "./modules"
import { asBigInt, some } from "../utils/map"
import { binding } from "../utils/options"
import * as parse from "./parse"
import { isnil_method, notnil_method, part_method, packcat_method } from "../middle/primitives"
import { lowerpattern, modtag, patternType } from "./patterns"

export { IRValue, WIntrinsic, lower_toplevel, bundlemacro, lowerfn, source, LIR, globals, assigned_globals, xlist, xpart, xcall, xtuple }

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

function formacro(ex: ast.Expr): ast.Expr {
  const [x, xs, body] = [ex.args[1], ex.args[3], ex.args[4]]
  const [itr, val] = [gensym("itr"), gensym("val")]
  return ast.Block(
    ast.Operator(s("="), itr, ast.Call(tag("common.iterator"), xs)),
    ast.Syntax(s("while"), s("true"), ast.Block(
      ast.Operator(s("="), val, ast.Call(tag("common.next"), ast.Swap(itr))),
      ast.Syntax(s("if"), ast.Call(symbol("nil?"), val), ast.Block(s("break"))),
      ast.Operator(s("="), x, ast.Call(tag("common.core.part"), ast.Call(tag("common.core.notnil"), val), 1n)),
      body)))
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

type LIR = MIR

const nil = new Binding(tag('common'), 'nil')

function source(m: ast.Meta): ir.Source {
  return { file: m.file, line: m.loc.line, col: m.loc.column }
}

function xcall(...args: (IRValue | number)[]): ir.Expr<IRValue> {
  return ir.expr("call", ...args)
}
function xtuple(...args: (IRValue | number)[]): ir.Expr<IRValue> {
  return ir.expr("tuple", ...args)
}
function xpack(...args: (IRValue | number)[]): ir.Expr<IRValue> {
  return ir.expr("pack", ...args)
}
function xlist(...args: (IRValue | number)[]): ir.Expr<IRValue> {
  return xpack(tag("common.List"), ...args)
}
function xpart(x: IRValue | number, i: IRValue | number): ir.Expr<IRValue> {
  return xcall(part_method, x, i)
}

function rcall(code: LIR, f: IRValue | number, args: (IRValue | number)[], { src, bp }: { src?: ast.Meta, bp?: boolean } = {}): number {
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
}

function GlobalScope(mod: Tag): Scope {
  return {
    has: (_: string) => false,
    get: (name: string) => new Binding(mod, name),
    set: (name: string, v: ir.Slot) => { throw new Error(`Cannot set ${name} in global scope`) },
    var: (name: string) => new Binding(mod, name),
    mod: () => mod,
    swaps: () => undefined,
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
  }
  return sc
}

// don't continue lowering after return
// e.g. `f(return 1)`
function _push<T, A, M>(code: ir.IR<T, A, M>, x: ir.Expr<T>, { type, src, bp }: { type?: ir.Anno<A>, src?: ast.Meta, bp?: boolean } = {}): number {
  if (!code.block().canbranch()) throw new Error("Pushing into finished block")
  return code.push(ir.stmt(x, { type, src: src && source(src), bp }))
}

function swapreturn(code: LIR, val: IRValue | number, swaps?: Map<number, string>, { src, bp }: { src?: ast.Meta, bp?: boolean } = {}): void {
  if (swaps && swaps.size > 0) {
    const maxArgs = Math.max(...swaps.keys())
    const args: (IRValue | number)[] = [val]
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
  const id = code.push(ir.stmt(ir.expr('ref', x), { type: types.list(int32()) }))
  const obj = code.push(ir.stmt(xcall(tag('common.JSObject'), id)))
  const s = code.push(ir.stmt(xcall(tag('common.String'), obj)))
  return code.push(ir.stmt(xpart(s, Type(1n))))
}

function lowermatch(sc: Scope, code: LIR, val: IRValue | number, pat: ast.Tree): IRValue | number {
  const sig = lowerpattern(pat, sc.mod(), resolve_static)
  const pattern = patternType(sig.pattern)
  const patternTuple = _push(code, xtuple(), { type: pattern })
  const m = rcall(code, tag('common.match'), [val, patternTuple])
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

function lower(sc: Scope, code: LIR, x: ast.Tree | ast.Tree[], value = true): IRValue | number {
  if (Array.isArray(x)) {
    if (x.length === 0) return nil
    x.slice(0, -1).forEach(item => lower(sc, code, item, false))
    return lower(sc, code, x[x.length - 1], value)
  }

  if (x instanceof ast.Token) {
    const val = x.unwrap()
    if (val instanceof Symbol) {
      // TODO only active inside loop
      if (val.toString() === 'break') {
        code.branch(-1)
        return nil
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
      return _push(code, xtuple(), { type: Type(val) })
    }
  }

  if (x instanceof ast.Expr) {
    if (x.head === 'Block') return lower(Scope(sc), code, x.args, value)
    if (x.head === 'Group') return lower(sc, code, x.args, value)
    if (x.head === 'Operator') return lowerOperator(sc, code, x, value)
    if (x.head === 'Call') return lowerCall(sc, code, x)
    if (x.head === 'Index') return lowerIndex(sc, code, x)
    if (x.head === 'Field') return lowerField(sc, code, x)
    if (x.head === 'List') return lowerList(sc, code, x)
    if (x.head === 'Template') return lowerTemplate(sc, code, x)
    if (x.head === 'Syntax') return lowerSyntax(sc, code, x, value)
    throw new Error(`Unimplemented ast.Expr lowering for head: ${x.head}`)
  }

  throw new Error(`Unimplemented lowering for: ${x}`)
}

function lowerOperator(sc: Scope, code: LIR, ex: ast.Expr, value = true): IRValue | number {
  const op = asSymbol(ex.args[0].unwrap()).toString()
  if (op === '=' && ex.args[1] instanceof ast.Token && ex.args[1].unwrap() instanceof Symbol) {
    // Simple assignment: x = value
    const y = lower(sc, code, ex.args[2])
    // Globals are side effects (if they error) so don't let the SSA transform move them around
    const ySlot = y instanceof Binding ? _push(code, ir.expr('global', y)) : y
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

function argtuple(sc: Scope, code: LIR, args: ast.Tree[], src?: ast.Meta): [IRValue | number, Map<string, number>] {
  const swaps = new Map<string, number>()
  const parts: (IRValue | number)[] = []
  let idx = 1
  let splat = false
  const argQueue = [...args]
  while (argQueue.length > 0) {
    if (ast.isExpr(argQueue[0], 'Splat')) {
      const splatArg = argQueue.shift() as ast.Expr
      parts.push(lower(sc, code, splatArg.args[0]))
      splat = true
    } else {
      const as: (IRValue | number)[] = []
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

function lowerCall(sc: Scope, code: LIR, ex: ast.Expr): IRValue | number {
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

function lowerIndex(sc: Scope, code: LIR, ex: ast.Expr): IRValue | number {
  const callExpr = ast.Call(tag('common.get'), ...ex.args)
  return lower(sc, code, callExpr.withmeta(ex.meta))
}

function lowerField(sc: Scope, code: LIR, ex: ast.Expr): IRValue | number {
  const [obj, fieldName] = ex.args
  const callExpr = ast.Call(tag('common.field'), obj, tag(asSymbol(fieldName.unwrap()).toString()))
  return lower(sc, code, callExpr.withmeta(ex.meta))
}

function lowerList(sc: Scope, code: LIR, ex: ast.Expr): IRValue | number {
  // TODO: should use the `list` function, but this puts off the need for special argument inference
  const [args] = argtuple(sc, code, ex.args, ex.meta)
  return args
}

function lowerTemplate(sc: Scope, code: LIR, ex: ast.Expr): IRValue | number {
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
  ['f64', types.float64()]])

function intrinsic(ex: ast.Tree): [WIntrinsic | WImport, ir.Anno<Type>] {
  let T: ir.Anno<Type> = types.nil
  if (ast.isExpr(ex, 'Operator') && isEqual(ex.args[0].unwrap(), ast.symbol(':'))) {
    const type = ex.args[2]
    if (isEqual(type.unwrap(), ast.symbol('unreachable'))) T = ir.unreachable
    else if (ast.isExpr(type, 'Group'))
      T = types.list(...type.args.map(t => some(wtypes.get(ast.asSymbol(t).name))))
    else T = some(wtypes.get(ast.asSymbol(type).name))
    ex = ex.args[1]
  }
  let op = ast.asExpr(ex).args[0].ungroup()
  if (isEqual(op.unwrap(), ast.symbol('call'))) {
    let name = ast.asExpr(ast.asExpr(ex).args[1], 'Field').args.map(t => t.unwrap().toString())
    return [new WImport(name[0], name[1]), T]
  } else if (isEqual(op, parse.expr('global.get'))) {
    throw new Error('not supported')
  } else {
    const namify = (x: ast.Tree): string =>
      ast.isExpr(x, 'Field') ? x.args.map(namify).join('.') : ast.asSymbol(x).name
    return [new WIntrinsic(namify(op)), T]
  }
}

function intrinsic_args(ex: ast.Tree): ast.Tree[] {
  if (ast.isExpr(ex, 'Operator') && isEqual(ex.args[0].unwrap(), ast.symbol(':')))
    return intrinsic_args(ex.args[1])
  const args = ast.asExpr(ex).args.slice(1).filter(x =>
    ast.isExpr(x, 'Operator') && isEqual(x.args[0].unwrap(), ast.symbol(':'))) as ast.Expr[]
  return args.map(x => x.args[1])
}

function lowerSyntax(sc: Scope, code: LIR, ex: ast.Expr, value = true): IRValue | number {
  const syntax = asSymbol(ex.args[0]).toString()
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
  } else if (syntax === 'while') {
    return lowerWhile(sc, code, ex, value)
  } else if (syntax === 'if') {
    return lowerIf(sc, code, parseIf(ex), value)
  } else if (syntax === 'wasm') {
    const wasmExpr = ast.asExpr(ex.args[1], 'Block').args[0]
    const [op, ret] = intrinsic(wasmExpr)
    const args = intrinsic_args(wasmExpr).map(arg => lower(sc, code, arg))
    return _push(code, xcall(op, ...args), { src: ex.meta, type: ret, bp: true })
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

function lowerWhile(sc: Scope, code: LIR, ex: ast.Expr, value = true): IRValue | number {
  const prevBlock = code.block()
  const header = code.newBlock()
  prevBlock.branch(header)
  const cond = lower(sc, code, ex.args[1])
  const condResult = rcall(code, tag('common.condition'), [cond], { src: ex.meta })
  const condBlock = code.block()
  const bodyStart = code.newBlock()
  lower(sc, code, ast.asExpr(ex.args[2], 'Block'), false)
  const bodyEnd = code.block()
  const after = code.newBlock()
  // Rewrite continue/break to the right block number
  for (let i = condBlock.id; i <= bodyEnd.id; i++) {
    const block = code.block(i + 1)
    for (const [v, st] of block) {
      if (st.expr instanceof ir.Branch && st.expr.target === -1) {
        code.setStmt(v, { ...st, expr: new ir.Branch(after.id + 1, st.expr.args, st.expr.when) })
      }
    }
  }
  condBlock.branch(bodyStart, [], { when: condResult, src: ex.meta && source(ex.meta) })
  condBlock.branch(after, [], { src: ex.args[0].meta && source(ex.args[0].meta) })
  if (bodyEnd.canbranch()) bodyEnd.branch(header)
  return nil
}

// TODO support pattern matching
function lowerLet(sc: Scope, code: LIR, ex: ast.Expr, value = true): IRValue | number {
  const assignments = ex.args.slice(1, -1)
  if (!assignments.every(x => ast.isExpr(x, 'Operator') && asSymbol(x.args[0].unwrap()).toString() === '='))
    throw new Error('let statement: all assignments must be of the form (= var val)')
  const vars = assignments.map(x => asSymbol(ast.asExpr(x, 'Operator').args[1]).toString())
  const vals = assignments.map(x => lower(sc, code, ast.asExpr(x, 'Operator').args[2]))
  sc = Scope(sc)
  for (const v of vars) sc.set(v, ir.slot(gensym(v).toString()))
  for (let i = 0; i < vars.length; i++)
    _push(code, ir.expr('set', sc.get(vars[i]), vals[i]))
  return lower(sc, code, ex.args[ex.args.length - 1], value)
}

function lowerIf(sc: Scope, code: LIR, ex: IfStmt, value = true): IRValue | number {
  sc = Scope(sc)
  const ts: ir.Block<MIR>[] = []
  const vs: (IRValue | number)[] = []
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

function lowerfn(mod: Tag, sig: Signature, body: ast.Tree, resolver: (x: Symbol) => Type, meta?: FuncInfo): LIR {
  const sc = Scope(GlobalScope(mod), sig.swap)
  const code = MIR(meta)
  for (const arg of sig.args) {
    const slot = ir.slot(arg)
    sc.set(arg, slot)
    _push(code, ir.expr('set', slot, code.argument(ir.unreachable)))
  }
  const out = withResolve(resolver, () => lower(sc, code, body))
  if (code.block().canbranch()) swapreturn(code, out, sig.swap)
  return renumber(prune(ssa(fuseblocks(code))))
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
  locals.forEach(x => pr.push(ir.stmt(ir.expr('set', ir.slot(x), new Binding(cx.name, x)))))
  for (const [v, st] of pr) {
    // Global loads use the new slot
    let ex = st.expr.map((x: IRValue | number) => x instanceof Binding && (locals.includes(x.name) || globals.has(x.name)) ? ir.slot(x.name) : x)
    // Global stores use the new slot
    if (ex.head === 'set' && ex.body[0] instanceof Binding) {
      globals.add(ex.body[0].name)
      ex = ir.expr('set', ir.slot(ex.body[0].name), ex.body[1])
    }
    pr.set(v, ex)
  }
  [...locals, ...globals].forEach(x => pr.push(ir.stmt(ir.expr('set', new Binding(cx.name, x), ir.slot(x)))))
  return [pr.finish(), globals]
}

function assigned_globals(code: MIR): Map<Binding, Type> {
  const out = new Map<Binding, Type>()
  for (const [_, st] of code)
    if (st.expr.head === 'set' && st.expr.body[0] instanceof Binding)
      out.set(st.expr.body[0], asType(st.type))
  return out
}

function lower_toplevel(mod: Module, ex: ast.Tree, resolve: (x: Symbol) => Type, meta?: FuncInfo): [LIR, Set<string>] {
  const sc = GlobalScope(mod.name)
  const code = MIR(meta)
  withResolve(resolve, () => { lower(sc, code, ex, false) })
  code.return(nil)
  let [code2, defs] = rewriteGlobals(code, mod)
  return [renumber(prune(ssa(fuseblocks(code2)))), defs]
}

// Turn global references into explicit load instructions
function globals(code: LIR): LIR {
  const pr = new ir.Pipe(code)
  const transform = (x: IRValue | number): IRValue | number =>
    x instanceof Binding ? pr.push(ir.stmt(ir.expr('global', x))) : x
  for (const [v, st] of pr) {
    const ex = st.expr
    if (ex.head === 'global') continue
    pr.delete(v)
    const ex2 = ex.head === 'set'
      ? ir.expr(ex.head, ex.body[0], ...ex.body.slice(1).map(transform))
      : ex.map(transform)
    const v2 = pr.push({ ...st, expr: ex2 })
    pr.replace(v, v2)
  }
  return pr.finish()
}
