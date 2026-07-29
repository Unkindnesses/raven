import { Type, Tag, tag, asTag, atomValue } from "../frontend/types.js"
import { Module, Modules, MethodKey, Binding, calltarget } from "../frontend/modules.js"
import { Def } from "../dwarf/index.js"
import { Anno, unreachable } from "../utils/ir.js"
import { lower_toplevel, expand, source, attrs, modtag } from "../frontend/lower.js"
import * as patterns from "../frontend/patterns.js"
import { symbolValues } from "./primitives.js"
import * as ast from "../frontend/ast.js"
import { parse } from "../frontend/parse.js"
import { emit } from "../backend/compiler.js"
import { Loader } from "../frontend/packages.js"

export { LoadState, SourceString, src as source, loadmodule, reload, vload, wrapPrint, resolve_static }

const declarations = ['fn', 'bundle', 'show', 'showPack', 'clear', 'import', 'export']

function wrapPrint(ex: ast.Tree): ast.Tree {
  if (ast.isExpr(ex, 'Syntax')) {
    const head = ex.args[0].unwrap()
    if (head instanceof ast.Symbol && declarations.includes(head.toString())) return ex
  }
  return ast.Call(ast.Template(ast.symbol('tag'), 'common.replshow'), ex)
}

function resolve_static(sources: Modules, mod: Tag, x: ast.Symbol): Type {
  let y = sources.resolve_static(new Binding(mod, x.toString()))
  if (y === unreachable) throw new Error(`Could not resolve ${x}`)
  return y
}

class LoadState {
  constructor(
    readonly comp: Modules,
    readonly mod: Module,
    readonly loader: Loader,
    readonly path: string = '',
    readonly importing: readonly string[] = [],
  ) { }

  at(path: string) { return new LoadState(this.comp, this.mod, this.loader, path, this.importing) }

  resolve_static(x: ast.Symbol): Type {
    return resolve_static(this.comp, this.mod.name, x)
  }
}

interface SourceString {
  path: string
  source: string
}

function src(path: string, source: string): SourceString {
  return { path, source }
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

function importnames(x: ast.Expr, from?: Module): string[] {
  const items = ast.asExpr(x.args[1], 'Block').args
  if (items.length === 1 && ast.isExpr(items[0], 'Splat') && items[0].args.length === 0) {
    if (from === undefined) throw new Error('`{ ... }` needs a module to take names from')
    return [...from.exports.keys()]
  }
  return items.map(name => ast.asSymbol(name.unwrap()).toString())
}

function frommodule(cx: LoadState, x: ast.Expr): Promise<Module> {
  const path = cx.loader.resolve(cx.path, ast.asString(x.args[3]))
  return loadmodule(cx.comp, cx.loader, path, [...cx.importing, cx.path])
}

async function load_export(cx: LoadState, x: ast.Expr): Promise<void> {
  if (x.args.length === 2) return cx.mod.export(importnames(x))
  const from = await frommodule(cx, x)
  cx.mod.export(importnames(x, from), from)
}

async function load_import(cx: LoadState, x: ast.Expr): Promise<void> {
  const from = await frommodule(cx, x)
  cx.mod.import(from, importnames(x, from))
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
  await loadfile(cx, cx.loader.resolve(cx.path, ast.asString(x.args[1])))
}

function load_expr(cx: LoadState, x: ast.Tree): void {
  const meta = Def('(global)', x.meta && source(x.meta))
  const key = new MethodKey(cx.mod.name, tag('common.core.main'))
  const [methods, defs] = lower_toplevel(cx.mod, key, x, meta)
  for (const def of defs) if (!cx.mod.has(def)) cx.mod.set(def, unreachable)
  const method = cx.mod.methods.method(key, { args: [], swap: new Map() }, methods)
  emit(method)
}

function receiverTag(cx: LoadState, ex: ast.Tree): Tag {
  ex = ex.ungroup()
  if (!(ast.isExpr(ex, 'Operator') && ast.symbol(':').isEqual(ex.args[1].unwrap())))
    throw new Error('Call overloads need a typed receiver, eg fn (f: T)(args...)')
  const trait = ex.args[2].ungroup().unwrap()
  return calltarget(cx.resolve_static(ast.asSymbol(trait)))
}

function load_fn(cx: LoadState, ex: ast.Tree): void {
  let [x, as] = attrs(ex)
  x = ast.asExpr(x)
  const extend = as.has('extend')
  const ts = attrString('ts', as.get('ts'))
  let [sig, body] = x.args.slice(1)
  sig = sig.ungroup()
  if (ast.isExpr(sig, 'Index')) {
    const [x, ...idxs] = sig.args
    sig = ast.Call(tag('common.get'), x, ast.List(...idxs))
  }
  if (!ast.isExpr(sig, 'Call') && !ast.isExpr(sig, 'Operator'))
    throw new Error(`Expected function signature, got ${ast.repr(sig)}`)
  const [callee, ...params] = ast.callargs(sig)
  const variable = callee.unwrap()
  const callable = ast.isExpr(sig, 'Call') &&
    !(variable instanceof Tag) &&
    !(variable instanceof ast.Symbol)
  let fnTag: Tag
  if (callable) {
    fnTag = receiverTag(cx, sig.args[0])
    sig = ast.List(...sig.args)
  } else {
    fnTag =
      variable instanceof Tag ? variable :
        extend ? asTag(cx.resolve_static(ast.asSymbol(variable))) :
          new Tag(cx.mod.name, ast.asSymbol(variable).toString())
    if (!extend && variable instanceof ast.Symbol)
      cx.mod.set(variable.toString(), fnTag)
    sig = ast.List(fnTag, ...params)
  }
  const meta = Def(fnTag.path, x.meta && source(x.meta))
  cx.mod.method(fnTag, patterns.signature(sig), { body, sig, meta }, { ts })
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
    if (x.args.length >= 3 && ast.symbol('=').isEqual(x.args[1].unwrap()) &&
      x.args[0].unwrap() instanceof ast.Symbol) {
      const c = simpleconst(cx, x.args[2])
      if (c !== undefined) {
        cx.mod.set(x.args[0].unwrap().toString(), c)
        return
      }
    }
  }
  load_expr(cx, x)
}

async function loadfile(cx: LoadState, src: SourceString | string): Promise<void> {
  if (typeof src === 'string') src = { path: src, source: await cx.loader.read(src) }
  await vload(cx.at(src.path), parse(src.path, src.source))
}

function prelude(comp: Modules, mod: Module): void {
  if (mod.name.parts[0] === 'common') return
  const common = comp.module(tag("common"))
  mod.import(common, [...common.exports.keys()])
}

async function loadmodule(comp: Modules, loader: Loader, path: string, importing: readonly string[] = []): Promise<Module> {
  if (importing.includes(path)) {
    const cycle = [...importing.slice(importing.indexOf(path)), path]
    throw new Error(`Circular import: ${cycle.join(' -> ')}`)
  }
  const mod = comp.module(loader.modtag(path))
  if (mod.path !== undefined) {
    if (mod.path !== path) throw new Error(`Module ${mod.name} is already loaded from ${mod.path}`)
    return mod
  }
  mod.path = path
  prelude(comp, mod)
  // TODO methods are looked up from main, so its imports have to cover every
  // module, even those it doesn't import directly.
  comp.module(tag("")).import(mod)
  await loadfile(new LoadState(comp, mod, loader, '', importing), path)
  return mod
}

async function reload(comp: Modules, src: SourceString | string, loader: Loader): Promise<Modules> {
  const main = comp.module(tag(""))
  main.clear()
  loader.package('', typeof src === 'string' ? src : src.path)
  prelude(comp, main)
  await loadfile(new LoadState(comp, main, loader), src)
  return comp
}
