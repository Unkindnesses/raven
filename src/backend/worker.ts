import { parentPort, MessagePort } from 'node:worker_threads'
import { Console } from 'node:console'
import { Writable } from 'node:stream'
import { loadWasm, support } from './support'

export { WorkerCommand, WorkerRequest, WorkerResponse }

type WorkerCommand =
  | { type: 'strings', strings: string[] }
  | { type: 'wasm', module: Uint8Array }

type WorkerRequest = WorkerCommand & { id: number }

type WorkerResponse =
  | { id: number, type: 'ok' }
  | { id: number, type: 'error', error: Error }
  | { type: 'stdout', data: string }
  | { type: 'stderr', data: string }

const port = parentPort
if (!port) throw new Error('REPL worker requires a parent port')

const imports = { support, wasm: {} }
let current = Promise.resolve()

port.on('message', (msg: WorkerRequest) => {
  const type = msg.type
  current = current.then(async () => {
    try {
      if (msg.type === 'strings') support.strings = msg.strings
      else if (msg.type === 'wasm') await runWasm(msg.module)
      else throw new Error('unknown command ' + String(type))
      port.postMessage({ id: msg.id, type: 'ok' })
    } catch (error) {
      port.postMessage({ id: msg.id, type: 'error', error })
    }
  })
})

class PortStream extends Writable {
  constructor(readonly port: MessagePort, readonly type: string) {
    super({ decodeStrings: false })
  }
  _write(data: any, encoding: BufferEncoding, callback: (error?: Error | null) => void) {
    this.port.postMessage({ type: this.type, data })
    callback()
  }
}

function patchIo(port: MessagePort) {
  const stdout = new PortStream(port, 'stdout')
  const stderr = new PortStream(port, 'stderr')
  process.stdout.write = stdout.write.bind(stdout)
  process.stderr.write = stderr.write.bind(stderr);
  (globalThis as any).console = new Console({ stdout, stderr })
}

patchIo(port)

async function runWasm(module: Uint8Array) {
  const exports = await loadWasm(module, imports)
  Object.assign(imports.wasm, exports)
  const _start = (WebAssembly as any).promising(exports._start)
  await _start()
}
