import { Type, Tag, tag, asTag, atomValue } from "../frontend/types.js"
import { Module, Modules, Binding, Signature, calltarget } from "../frontend/modules.js"
import { Def } from "../dwarf/index.js"
import { Anno, unreachable } from "../utils/ir.js"
import { callpattern, callablepattern, modtag } from "../frontend/patterns.js"
import { lower_toplevel, expand, source, attrs } from "../frontend/lower.js"
import { symbolValues } from "./primitives.js"
import * as ast from "../frontend/ast.js"
import { parse } from "../frontend/parse.js"
import { emit } from "../backend/compiler.js"

export { LoadState, Loader, SourceString, src as source, loadmodule, reload, vload }

function pathtag(p: string): Tag {
  if (!p.endsWith('.rv')) throw new Error(`Invalid path: ${p}`)
  return tag(p.slice(0, -3).split('/').join('.'))
}

type Loader = (path: string) => Promise<[string, string]>

class LoadState {
  constructor(readonly comp: Modules, readonly mod: Module, readonly load: Loader) { }
}

interface SourceString {
  path: string
  source: string
}

function src(path: string, source: string): SourceString {
  return { path, source }
}

function resolve_static(cx: LoadState, x: ast.Symbol): Type {
  let y = cx.comp.resolve_static(new Binding(cx.mod.name, x.toString()))
  if (y === unreachable) throw new Error(`Could not resolve ${x}`)
  return y
}

function simpleconst(cx: LoadState, x: ast.Tree): Anno<Type> | Binding | undefined {
  const unwrapped = x.unwrap()
  if (unwrapped instanceof ast.Symbol) return cx.mod.get(unwrapped.toString()) // TODO error if missing
  if (typeof unwrapped !== 'string' && ast.isAtom(unwrapped))
    return atomValue(unwrapped)
  if (ast.isExpr(x, 'Template') &&
    ast.symbol('tag').isEqual(x.args[0].unwrap()))
    return Type(modtag(cx.mod.name, x.args[1].unwrap() as string))
  return
}

function attrString(name: string, args?: ast.Tree[]): string | undefined {
  if (!args) return
  if (args.length !== 1) throw new Error(`@${name} expects one argument`)
  const value = args[0].unwrap()
  if (typeof value !== 'string') throw new Error(`@${name} expects a string literal`)
  return value.trim()
}

function load_export(cx: LoadState, x: ast.Expr): void {
  const names = ast.asExpr(x.args[1], 'Block').args
  for (const name of names)
    cx.mod.exports.add(ast.asSymbol(name.unwrap()).toString())
}

function load_import(cx: LoadState, x: ast.Expr): void {
  const pathStr = ast.asString(x.args[3])
  const modTag = new Tag('common', pathtag(pathStr))
  const mod = cx.comp.module(modTag)
  const names = ast.asExpr(x.args[1], 'Block').args.map(name => ast.asSymbol(name.unwrap()).toString())
  cx.mod.import(mod, names)
}

function load_clear(cx: LoadState, x: ast.Expr): void {
  for (let i = 1; i < x.args.length; i++) {
    const name = ast.asSymbol(x.args[i].unwrap()).toString()
    if (!cx.mod.has(name)) continue
    const val = cx.mod.get(name)!
    if (val instanceof Binding) throw new Error('unimplemented: load_clear with binding')
    if (val !== '⊥' && !(val instanceof Tag) && symbolValues(val).size > 0)
      throw new Error('val must be a tag or have no symbolic values')
    if (val instanceof Tag) cx.mod.methods.delete(val)
    cx.mod.delete(name)
  }
}

async function load_include(cx: LoadState, x: ast.Expr): Promise<void> {
  const filename = ast.asString(x.args[1])
  await loadfile(cx, filename)
}

function load_expr(cx: LoadState, x: ast.Tree): void {
  x = replaceInnerFns(cx, x, { owner: new Tag(cx.mod.name, ast.gensym('global')), count: 0 })
  const meta = Def('(global)', x.meta && source(x.meta))
  const [ir, defs] = lower_toplevel(cx.mod, x, meta)
  for (const def of defs) if (!cx.mod.has(def)) cx.mod.set(def, unreachable)
  const method = cx.mod.method(tag('common.core.main'), callpattern(tag('common.core.main'), ast.List()),
    { kind: 'ir', body: ir })
  emit(method)
}

function receiverTag(cx: LoadState, ex: ast.Tree): Tag {
  ex = ex.ungroup()
  if (!(ast.isExpr(ex, 'Operator') && ast.symbol(':').isEqual(ex.args[0].unwrap())))
    throw new Error('Call overloads need a typed receiver, eg fn (f: T)(args...)')
  const trait = ex.args[2].ungroup().unwrap()
  return calltarget(resolve_static(cx, ast.asSymbol(trait)))
}

interface LiftState {
  owner: Tag
  count: number
}

function lambdaParts(ex: ast.Expr): [ast.Tree[], ast.Tree] | undefined {
  if (!ast.isSyntax(ex, 'fn')) return
  if (ex.args.length === 2 && ast.isExpr(ex.args[1], 'Block')) return [[], ex.args[1]]
  if (ex.args.length === 3 && ast.isExpr(ex.args[2], 'Block')) {
    if (!ast.isExpr(ex.args[1], 'Group')) throw new Error(`Expected anonymous function argument list, got ${ast.repr(ex.args[1])}`)
    return [ex.args[1].args, ex.args[2]]
  }
  return
}

function registerLambda(cx: LoadState, name: Tag, params: ast.Tree[], body: ast.Tree, meta?: ast.Meta): void {
  const resolve = (x: ast.Symbol) => resolve_static(cx, x)
  const lambdaType = ast.Call(tag('common.core.pack'), name)
  const self = ast.Operator(ast.symbol(':'), ast.symbol('_'), lambdaType)
  const sig = callablepattern(ast.List(self, ...params), cx.mod.name, resolve)
  cx.mod.method(name, sig, { kind: 'fn', body, meta: Def(name.path, meta && source(meta)) })
}

function replaceInnerFns(cx: LoadState, x: ast.Tree, st: LiftState): ast.Tree {
  if (x instanceof ast.Token) return x
  const lambda = lambdaParts(x)
  if (!lambda) return x.map(arg => replaceInnerFns(cx, arg, st))
  const [params, body] = lambda
  const lambdaTag = new Tag(st.owner, `λ/${++st.count}`)
  const liftedBody = replaceInnerFns(cx, body, { owner: lambdaTag, count: 0 })
  registerLambda(cx, lambdaTag, params, liftedBody, x.meta)
  return ast.Call(tag('common.core.pack'), lambdaTag).withmeta(x.meta)
}

function load_fn(cx: LoadState, ex: ast.Tree): void {
  let [x, as] = attrs(ex)
  x = ast.asExpr(x)
  const extend = as.has('extend')
  const ts = attrString('ts', as.get('ts'))
  const [sig, body] = x.args.slice(1)
  let signature = sig.ungroup()
  if (ast.isExpr(signature, 'Index')) {
    const [x, ...idxs] = signature.args
    signature = ast.Call(tag('common.get'), x, ast.List(...idxs))
  }
  if (!ast.isExpr(signature, 'Call') && !ast.isExpr(signature, 'Operator'))
    throw new Error(`Expected function signature, got ${ast.repr(signature)}`)
  const resolve = (x: ast.Symbol) => resolve_static(cx, x)
  const variable = signature.args[0].unwrap()
  const callable = ast.isExpr(signature, 'Call') &&
    !(variable instanceof Tag) &&
    !(variable instanceof ast.Symbol)
  let fnTag: Tag
  let sigPattern: Signature
  if (callable) {
    fnTag = receiverTag(cx, signature.args[0])
    sigPattern = callablepattern(ast.List(...signature.args), cx.mod.name, resolve)
  } else {
    fnTag =
      variable instanceof Tag ? variable :
        extend ? asTag(resolve_static(cx, ast.asSymbol(variable))) :
          new Tag(cx.mod.name, ast.asSymbol(variable).toString())
    if (!extend && variable instanceof ast.Symbol)
      cx.mod.set(variable.toString(), fnTag)
    sigPattern = callpattern(fnTag, ast.List(...signature.args.slice(1)), cx.mod.name, resolve)
  }
  const meta = Def(fnTag.path, x.meta && source(x.meta))
  const liftedBody = replaceInnerFns(cx, body, { owner: fnTag, count: 0 })
  cx.mod.method(fnTag, sigPattern, { kind: 'fn', body: liftedBody, meta }, ts)
}

async function vload(cx: LoadState, x: ast.Tree, extend = false): Promise<void> {
  x = expand(x)
  let [ex] = attrs(x)
  if (ast.isExpr(ex, 'Syntax')) {
    x = x as ast.Expr
    const first = ex.args[0].unwrap()
    if (ast.symbol('include').isEqual(first)) return load_include(cx, x)
    if (ast.symbol('export').isEqual(first)) return load_export(cx, x)
    if (ast.symbol('import').isEqual(first)) return load_import(cx, x)
    if (ast.symbol('clear').isEqual(first)) return load_clear(cx, x)
    if (ast.symbol('fn').isEqual(first)) return load_fn(cx, x)
    return load_expr(cx, x)
  }
  if (ast.isExpr(x, 'File') || ast.isExpr(x, 'Group')) {
    for (const item of x.args) await vload(cx, item)
    return
  }
  if (ast.isExpr(x, 'Operator')) {
    if (x.args.length >= 3 && ast.symbol('=').isEqual(x.args[0].unwrap()) &&
      x.args[1].unwrap() instanceof ast.Symbol) {
      const c = simpleconst(cx, x.args[2])
      if (c !== undefined) {
        cx.mod.set(x.args[1].unwrap().toString(), c)
        return
      }
    }
  }
  load_expr(cx, x)
}

async function loadfile(cx: LoadState, path: SourceString | string, content?: string): Promise<void> {
  if (typeof path !== 'string')
    [path, content] = [path.path, path.source]
  if (content === undefined) [path, content] = await cx.load(path)
  await vload(cx, parse(path, content))
}

async function loadmodule(comp: Modules, mod: Module | Tag, src: SourceString | string, load: Loader): Promise<Module> {
  if (mod instanceof Tag) mod = comp.module(mod)
  const cx = new LoadState(comp, mod, load)
  await loadfile(cx, src)
  return mod
}

async function reload(comp: Modules, src: SourceString | string, load: Loader): Promise<Modules> {
  const main = comp.module(tag(""))
  main.clear()
  const common = comp.module(tag("common"))
  main.import(common, [...common.exports])
  await loadmodule(comp, main, src, load)
  return comp
}
