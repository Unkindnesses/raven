import { Type, Tag, tag, asTag } from "../frontend/types"
import { Module, Modules, Binding } from "../frontend/modules"
import { Def } from "../dwarf"
import { Anno, unreachable } from "../utils/ir"
import { modtag } from "../frontend/patterns"
import { lower_toplevel, bundlemacro, lowerfn, source, unwrapAnno } from "../frontend/lower"
import { lowerpattern } from "../frontend/patterns"
import { symbolValues, core } from "./primitives"
import * as ast from "../frontend/ast"
import * as path from "path"
import * as fs from "fs"
import isEqual from "lodash/isEqual"
import { parse } from "../frontend/parse"
import { emit } from "../backend/compiler"
import { dirname } from "../dirname"

export { LoadState, SourceString, src as source, loadmodule, reload, common, vload }

function pathtag(p: string): Tag {
  if (!p.endsWith('.rv')) throw new Error(`Invalid path: ${p}`)
  return tag(p.slice(0, -3).split('/').join('.'))
}

class LoadState {
  constructor(readonly comp: Modules, readonly mod: Module) { }
}

const common = path.resolve(dirname, "../common")

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
  if (typeof unwrapped === 'number' || typeof unwrapped === 'bigint' || unwrapped instanceof Tag) return Type(unwrapped)
  if (ast.isExpr(x, 'Template') &&
    isEqual(x.args[0].unwrap(), ast.symbol('tag')))
    return Type(modtag(cx.mod.name, x.args[1].unwrap() as string))
  return
}

function load_export(cx: LoadState, x: ast.Expr): void {
  const names = ast.asExpr(x.args[1], 'Block').args
  for (const name of names)
    cx.mod.exports.add(ast.asSymbol(name.unwrap()).toString())
}

function load_import(cx: LoadState, x: ast.Expr): void {
  const pathStr = ast.asString(x.args[3])
  const modTag = new Tag('common', ...pathtag(pathStr).parts)
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

function load_include(cx: LoadState, x: ast.Expr): void {
  const filename = ast.asString(x.args[1])
  const filepath = path.join(common, filename)
  loadfile(cx, filepath)
}

function load_expr(cx: LoadState, x: ast.Tree): void {
  const meta = Def('common.core.main', x.meta && source(x.meta))
  const [ir, defs] = lower_toplevel(cx.mod, x, (x: ast.Symbol) => resolve_static(cx, x), meta)
  for (const def of defs) if (!cx.mod.has(def)) cx.mod.set(def, unreachable)
  emit(cx.mod.method(tag('common.core.main'), lowerpattern(ast.List()), ir))
}

function load_fn(cx: LoadState, x: ast.Expr): void {
  let extend = ast.isExpr(x, 'Annotation') && isEqual(x.args[0].unwrap(), ast.symbol('extend'))
  x = unwrapAnno(x) as ast.Expr
  const [sig, body] = x.args.slice(1)
  let signature = sig.ungroup()
  if (ast.isExpr(signature, 'Index')) signature = ast.Call(tag('common.get'), ...signature.args)
  if (!ast.isExpr(signature, 'Call') && !ast.isExpr(signature, 'Operator'))
    throw new Error(`Expected function signature, got ${ast.repr(signature)}`)
  const variable = signature.args[0].unwrap()
  let fnTag =
    variable instanceof Tag ? variable :
      extend ? asTag(resolve_static(cx, ast.asSymbol(variable))) :
        new Tag(cx.mod.name, ast.asSymbol(variable).toString())
  if (!extend && variable instanceof ast.Symbol)
    cx.mod.set(variable.toString(), fnTag)
  const resolve = (x: ast.Symbol) => resolve_static(cx, x)
  const sigPattern = lowerpattern(ast.List(...signature.args.slice(1)), cx.mod.name, resolve)
  const meta = Def(fnTag.path, x.meta && source(x.meta))
  const ir = lowerfn(cx.mod.name, sigPattern, body, resolve, meta)
  cx.mod.method(fnTag, sigPattern, ir)
}

function vload(cx: LoadState, x: ast.Tree, extend = false): void {
  let ex = unwrapAnno(x)
  if (ast.isExpr(ex, 'Syntax')) {
    x = x as ast.Expr
    const first = ex.args[0].unwrap()
    if (isEqual(first, ast.symbol('include'))) return load_include(cx, x)
    if (isEqual(first, ast.symbol('export'))) return load_export(cx, x)
    if (isEqual(first, ast.symbol('import'))) return load_import(cx, x)
    if (isEqual(first, ast.symbol('clear'))) return load_clear(cx, x)
    if (isEqual(first, ast.symbol('bundle'))) return vload(cx, bundlemacro(x))
    if (isEqual(first, ast.symbol('fn'))) return load_fn(cx, x)
    return load_expr(cx, x)
  }
  if (ast.isExpr(x, 'Group')) {
    for (const item of x.args) vload(cx, item)
    return
  }
  if (ast.isExpr(x, 'Operator')) {
    if (x.args.length >= 3 && isEqual(x.args[0].unwrap(), ast.symbol('=')) &&
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

function loadfile(cx: LoadState, path: SourceString | string, content?: string): void {
  if (typeof path !== 'string')
    [path, content] = [path.path, path.source]
  if (content === undefined) content = fs.readFileSync(path, 'utf8')
  const exprs = parse(path, content)
  for (const expr of exprs) vload(cx, expr)
}

function loadmodule(comp: Modules, mod: Module | Tag, src: SourceString | string): Module {
  if (mod instanceof Tag) mod = comp.module(mod)
  const cx = new LoadState(comp, mod)
  loadfile(cx, src)
  return mod
}

function reload(comp: Modules, src: SourceString | string): Modules {
  const main = comp.module(tag(""))
  main.clear()
  const common = comp.module(tag("common"))
  main.import(common, [...common.exports])
  loadmodule(comp, main, src)
  return comp
}

function load(src: SourceString | string): Modules {
  const comp = new Modules()
  comp.module(core())
  loadmodule(comp, tag("common.core"), path.join(common, "core.rv"))
  loadmodule(comp, tag("common"), path.join(common, "common.rv"))
  reload(comp, src)
  return comp
}
