import { Worker } from 'node:worker_threads'
import * as path from 'node:path'
import { Writable } from 'node:stream'
import { dirname } from './dirname.js'
import { binary as wasmBinary } from '../wasm/binary.js'
import * as wasm from '../backend/wasm.js'
import { Pipeline, withEmit } from '../backend/compiler.js'
import { load } from './compile.js'
import { reset } from '../utils/cache.js'
import { LoadState, vload, reload, source, wrapPrint } from '../middle/load.js'
import * as types from '../frontend/types.js'
import { tag } from '../frontend/types.js'
import { parse } from '../frontend/parse.js'
import * as ast from '../frontend/ast.js'
import { WorkerCommand, WorkerRequest, WorkerResponse } from './worker.js'
import { Options, withOptions } from '../utils/options.js'
import { invoke_method } from '../middle/primitives.js'

export { REPL }

interface ReplOptions {
  stdout?: Writable
  stderr?: Writable
  options?: Partial<Options>
}

interface Pending {
  resolve(): void
  reject(err: Error): void
}

class REPL {
  private readonly worker: Worker
  private pipe: Pipeline
  private readonly emitter: wasm.StreamEmitter
  private readonly history: { pipe: Pipeline, input: string }[] = []
  private readonly pending = new Map<number, Pending>()
  private readonly stdout: Writable
  private readonly stderr: Writable
  private readonly options: Partial<Options>
  private nextId = 0
  private output = ''
  private closed = false

  constructor(opts: ReplOptions = {}) {
    this.stdout = opts.stdout ?? process.stdout
    this.stderr = opts.stderr ?? process.stderr
    this.options = opts.options ?? {}
    this.pipe = new Pipeline()
    this.emitter = new wasm.StreamEmitter(this.pipe.wasm.tables)
    this.worker = new Worker(path.join(dirname, '../../dist/cli/worker.js'), { name: 'raven-repl' })
    this.attachIO()
  }

  static async create(opts: ReplOptions = {}) {
    const repl = new REPL(opts)
    await repl.init()
    return repl
  }

  async close() {
    if (this.closed) return
    this.closed = true
    for (const [, pending] of this.pending) pending.reject(new Error('REPL closed'))
    this.pending.clear()
    await this.worker.terminate()
  }

  async init() {
    await withOptions(this.options, async () => {
      await this.pipe.loadcommon(this.emitter, load)
      this.pipe.export(this.emitter, [invoke_method, types.Func, types.list(), types.list(), types.list()], '__raven_async_task')
      await withEmit(m => {
        reset(this.pipe)
        this.pipe.emit(this.emitter, m)
      }, async () => {
        await reload(this.pipe.sources, source('repl', ''), load)
      })
      reset(this.pipe)
      await this.flush()
    })
  }

  async eval(src: string) {
    this.output = ''
    await withOptions(this.options, async () => {
      const exprs = [...parse('repl', src).args]
      const undoCount = getUndoCount(exprs)
      if (undoCount !== null) return this.undo(undoCount)
      if (!exprs.length) return
      const prev = this.pipe
      const pipe = this.pipe.fork()
      exprs[exprs.length - 1] = wrapPrint(exprs[exprs.length - 1])
      await withEmit(m => {
        reset(pipe)
        pipe.emit(this.emitter, m)
      }, async () => {
        const defs = pipe.sources
        const module = defs.module(tag(''))
        const cx = new LoadState(defs, module, load)
        for (const expr of exprs) await vload(cx, expr)
      })
      reset(pipe)
      await this.flush()
      this.history.push({ pipe: prev, input: src })
      this.pipe = pipe
    })
    return this.output
  }

  private async flush() {
    while (this.emitter.queue.length) {
      const module = this.emitter.queue.shift()!
      const binary = wasmBinary(module)
      const buffer = binary.buffer
      if (!(buffer instanceof ArrayBuffer)) throw new Error('expected ArrayBuffer')
      await this.command({ type: 'wasm', module: binary }, [buffer])
    }
  }

  private attachIO() {
    this.worker.on('message', msg => this.handleMessage(msg as WorkerResponse))
    this.worker.on('error', err => this.fail(err as Error))
    this.worker.on('exit', code => {
      if (!this.closed && code !== 0)
        this.fail(new Error(`REPL worker exited with code ${code}`))
    })
  }

  private handleMessage(msg: WorkerResponse) {
    if (msg.type === 'stdout') {
      this.output += msg.data
      this.stdout?.write(msg.data)
      return
    }
    if (msg.type === 'stderr') {
      this.stderr?.write(msg.data)
      return
    }
    const pending = this.pending.get(msg.id)
    if (!pending) return
    this.pending.delete(msg.id)
    if (msg.type === 'ok') pending.resolve()
    else pending.reject(msg.error)
  }

  private fail(err: Error) {
    for (const [, pending] of this.pending) pending.reject(err)
    this.pending.clear()
    if (!this.closed) this.closed = true
  }

  private async command(command: WorkerCommand, transfer: ArrayBuffer[] = []) {
    if (this.closed) throw new Error('REPL closed')
    return await new Promise<void>((resolve, reject) => {
      const id = this.nextId++
      this.pending.set(id, { resolve, reject })
      const payload: WorkerRequest = { ...command, id }
      if (transfer.length) this.worker.postMessage(payload, transfer)
      else this.worker.postMessage(payload)
    })
  }

  private undo(count: number) {
    for (let i = 0; i < count; i++) {
      const entry = this.history.pop()
      if (!entry) return
      this.pipe = entry.pipe
      const line = `# undo ${entry.input}\n`
      this.output += line
      this.stdout?.write(line)
    }
  }
}

function getUndoCount(exprs: readonly ast.Tree[]): number | null {
  if (exprs.length !== 1) return null
  const node = exprs[0].ungroup()
  if (node instanceof ast.Token)
    return ast.symbol('undo').isEqual(node.unwrap()) ? 1 : null
  if (!ast.isExpr(node, 'Syntax')) return null
  const [head, ...args] = node.args
  if (!ast.symbol('undo').isEqual(head.unwrap())) return null
  if (args.length !== 1) return null
  const count = args[0].ungroup().unwrap()
  if (typeof count === 'bigint') return Number(count)
  return null
}
