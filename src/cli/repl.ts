import { Worker } from 'node:worker_threads'
import * as path from 'node:path'
import { dirname } from '../dirname'
import type { Writable } from 'node:stream'
import { binary as wasmBinary } from '../wasm/binary'
import { StreamEmitter } from '../backend/wasm'
import { Pipeline, withEmit } from '../backend/compiler'
import { reset } from '../utils/cache'
import { LoadState, vload, reload, source } from '../middle/load'
import { tag } from '../frontend/types'
import { parse } from '../frontend/parse'
import * as ast from '../frontend/ast'
import { WorkerCommand, WorkerRequest, WorkerResponse } from './worker'

export { REPL }

interface ReplOptions {
  stdout?: Writable
  stderr?: Writable
}

interface Pending {
  resolve(): void
  reject(err: Error): void
}

class REPL {
  private readonly worker: Worker
  private readonly pipe: Pipeline
  private readonly emitter: StreamEmitter
  private readonly pending = new Map<number, Pending>()
  private readonly stdout: Writable
  private readonly stderr: Writable
  private nextId = 0
  private output = ''
  private closed = false

  private constructor(opts: ReplOptions = {}) {
    this.stdout = opts.stdout ?? process.stdout
    this.stderr = opts.stderr ?? process.stderr
    this.pipe = new Pipeline()
    this.emitter = new StreamEmitter()
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

  private async init() {
    this.pipe.loadcommon(this.emitter)
    await withEmit(m => {
      reset(this.pipe)
      this.pipe.emit(m, this.emitter)
    }, () => {
      reload(this.pipe.sources, source('repl', ''))
    })
    reset(this.pipe)
    await this.flush()
  }

  async eval(src: string) {
    this.output = ''
    await withEmit(m => {
      reset(this.pipe)
      this.pipe.emit(m, this.emitter)
    }, () => {
      const defs = this.pipe.sources
      const module = defs.module(tag(''))
      const cx = new LoadState(defs, module)
      const exprs = parse('repl', src)
      for (const expr of exprs) vload(cx, wrapPrint(expr))
    })
    reset(this.pipe)
    await this.flush()
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
    this.worker.on('error', err => this.fail(err))
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
}

function wrapPrint(ex: ast.Tree) {
  if (ast.isExpr(ex, 'Syntax')) {
    const head = ex.args[0].unwrap()
    if (head instanceof ast.Symbol && ['fn', 'bundle', 'show', 'showPack'].includes(head.toString()))
      return ex
  }
  return ast.Call(ast.Template(ast.symbol('tag'), 'common.replshow'), ex)
}
