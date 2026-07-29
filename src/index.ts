import { Pipeline, withEmit } from './backend/compiler.js'
import { LoadState, reload, source, vload, wrapPrint } from './middle/load.js'
import { Loader } from './frontend/packages.js'
import bundledStdlibJson from './common.json' with { type: 'json' }
import * as wasm from './backend/wasm.js'
import { tag } from './frontend/types.js'
import { reset } from './utils/cache.js'
import { binary } from './wasm/binary.js'
import { parse } from './frontend/parse.js'

export { compiler, Compiler }

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

const load = new Loader(async path => {
  const contents = lookupBundled(path.replace(/^common\//, ''))
  if (contents !== undefined) return contents
  throw new Error(`Unable to load ${path}; filesystem access is not available in this environment`)
}, { common: 'common/common.rv' })

// TODO combine with repl.ts
class StreamCompiler {
  private readonly pipe: Pipeline
  private readonly emitter: wasm.StreamEmitter
  private ready = false

  private constructor(private readonly load: Loader) {
    this.pipe = new Pipeline()
    this.emitter = new wasm.StreamEmitter(this.pipe.wasm.tables)
  }

  static async create(load: Loader): Promise<StreamCompiler> {
    const compiler = new StreamCompiler(load)
    await compiler.pipe.loadcommon(compiler.emitter, load)
    return compiler
  }

  async compile(src: string): Promise<Uint8Array[]> {
    const modules: Uint8Array[] = []
    const strip = true
    modules.push(...await this.init(strip))
    await withEmit(m => {
      reset(this.pipe)
      this.pipe.emit(this.emitter, m)
    }, async () => {
      const defs = this.pipe.sources
      const module = defs.module(tag(''))
      const cx = new LoadState(defs, module, load)
      const exprs = [...parse('repl', src).args]
      if (exprs.length) exprs[exprs.length - 1] = wrapPrint(exprs[exprs.length - 1])
      for (const expr of exprs) await vload(cx, expr)
    })
    reset(this.pipe)
    modules.push(...this.flush(strip))
    return modules
  }

  private async init(strip: boolean): Promise<Uint8Array[]> {
    if (this.ready) return []
    await withEmit(m => {
      reset(this.pipe)
      this.pipe.emit(this.emitter, m)
    }, async () => {
      await reload(this.pipe.sources, source('repl', ''), this.load)
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

interface Compiler {
  compile(src: string): Promise<Uint8Array[]>
}

async function compiler(): Promise<Compiler> {
  return StreamCompiler.create(load)
}
