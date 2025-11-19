import { Pipeline, withEmit } from './backend/compiler.js'
import { Loader, LoadState, reload, source, vload } from './middle/load.js'
import bundledStdlibJson from './common.json' with { type: 'json' }
import * as wasm from './backend/wasm.js'
import { tag } from './frontend/types.js'
import { reset } from './utils/cache.js'
import { binary } from './wasm/binary.js'
import { parse } from './frontend/parse.js'
import * as ast from './frontend/ast.js'

export { compiler }

type BundledStdlibTree = { [segment: string]: string | BundledStdlibTree }

const bundledStdlib = bundledStdlibJson as BundledStdlibTree

const lookupBundled = (key: string): string | undefined => {
  const segments = key.split('/').filter(Boolean)
  let node: string | BundledStdlibTree = bundledStdlib
  for (const segment of segments) {
    if (typeof node === 'string') return undefined
    node = node[segment]
    if (node === undefined) return undefined
  }
  return typeof node === 'string' ? node : undefined
}

const hasBundled = (key: string): boolean => lookupBundled(key) !== undefined

const canonical = (path: string): string => {
  const normalized = path.replace(/\\/g, '/').replace(/^\//, '')
  if (hasBundled(normalized)) return normalized
  const marker = '/common/'
  const idx = normalized.lastIndexOf(marker)
  if (idx !== -1) return normalized.slice(idx + marker.length)
  if (normalized.startsWith('common/')) return normalized.slice('common/'.length)
  return normalized
}

const load: Loader = path => {
  const key = canonical(path)
  const contents = lookupBundled(key)
  if (contents !== undefined) return [`common/${key}`, contents]
  throw new Error(`Unable to load ${path}; filesystem access is not available in this environment`)
}

function wrapPrint(ex: ast.Tree) {
  if (ast.isExpr(ex, 'Syntax')) {
    const head = ex.args[0].unwrap()
    if (head instanceof ast.Symbol && ['fn', 'bundle', 'show', 'showPack'].includes(head.toString()))
      return ex
  }
  return ast.Call(ast.Template(ast.symbol('tag'), 'common.replshow'), ex)
}

// TODO combine with repl.ts
class StreamCompiler {
  private readonly pipe: Pipeline
  private readonly emitter: wasm.StreamEmitter
  private ready = false

  constructor(private readonly load: Loader) {
    this.pipe = new Pipeline()
    this.emitter = new wasm.StreamEmitter()
    this.pipe.loadcommon(this.emitter, this.load)
  }

  compile(src: string): Uint8Array[] {
    const modules: Uint8Array[] = []
    const strip = true
    modules.push(...this.init(strip))
    withEmit(m => {
      reset(this.pipe)
      this.pipe.emit(m, this.emitter)
    }, () => {
      const defs = this.pipe.sources
      const module = defs.module(tag(''))
      const cx = new LoadState(defs, module, load)
      const exprs = parse('repl', src)
      if (exprs.length) exprs[exprs.length - 1] = wrapPrint(exprs[exprs.length - 1])
      for (const expr of exprs) vload(cx, expr)
    })
    reset(this.pipe)
    modules.push(...this.flush(strip))
    return modules
  }

  private init(strip: boolean): Uint8Array[] {
    if (this.ready) return []
    withEmit(m => {
      reset(this.pipe)
      this.pipe.emit(m, this.emitter)
    }, () => {
      reload(this.pipe.sources, source('repl', ''), this.load)
    })
    reset(this.pipe)
    this.ready = true
    return this.flush(strip)
  }

  private flush(strip: boolean): Uint8Array[] {
    const modules: Uint8Array[] = []
    while (this.emitter.queue.length)
      modules.push(binary(this.emitter.queue.shift()!, strip))
    return modules
  }
}

export interface Compiler {
  compile(src: string): Uint8Array[]
}

function compiler(): Compiler {
  return new StreamCompiler(load)
}
