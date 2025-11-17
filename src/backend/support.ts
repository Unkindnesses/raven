import * as fs from 'fs/promises'
import { DebugModule, locate, Source } from '../dwarf/parse'
import { Def } from '../dwarf'

export { loadWasm, support, table }

let idcounter = 0
const table: any = {}

function createRef(obj: unknown) {
  if (idcounter >= 4294967295) throw new Error("too many JSRefs")
  const ref = idcounter++
  table[ref] = obj
  return ref
}

function fromRef(ref: number) {
  return table[ref]
}

function release(ref: number) {
  delete table[ref]
}

function global() {
  return globalThis
}

function call(obj: any, meth: any, ...args: any[]) {
  const func = obj[meth]
  if (func === undefined) {
    throw new Error(`No such method ${meth}`)
  }
  return func.call(obj, ...args)
}

function apply(obj: any, meth: any, args: any) {
  return call(obj, meth, ...args)
}

async function errcall(obj: any, meth: any, ...args: any[]) {
  const func = obj[meth]
  if (func === undefined) {
    throw new Error(`No such method ${meth}`)
  }
  try {
    let result = await func.call(obj, ...args)
    return [0, result]
  } catch (e) {
    return [1, e]
  }
}

function equal(a: any, b: any) {
  return a === b
}

function abort(obj: any, cause: any) {
  throw new Error(obj, cause ? { cause: cause } : {})
}

(globalThis as any).require = require;

(globalThis as any).sleep = function (n: number) {
  return new Promise<void>(resolve => {
    setTimeout(() => { resolve() }, n * 1000)
  })
};

(globalThis as any).readline = function () {
  process.stdin.resume()
  return new Promise(resolve => {
    process.stdin.once('data', data => {
      process.stdin.pause()
      resolve(data.toString().trim())
    })
  })
};

// TODO: used for testing, remove
(globalThis as any).dummyPromise = function (n: any) {
  return new Promise(resolve => resolve(n))
};

(globalThis as any).dummyErr = function () {
  throw new Error('dummy error')
}

function support() {
  return {
    global, call, apply,
    createRef, fromRef, abort,
    equal, release,
    get: (x: any, p: any) => x[p],
    set: (x: any, p: any, v: any) => { x[p] = v },
    identity: (x: any) => x,
    await: new (WebAssembly as any).Suspending((x: any) => x),
    errcall: new (WebAssembly as any).Suspending(errcall),
    debugger: () => { debugger }
  }
}

const debugModules = new Map<WebAssembly.Instance, DebugModule>()

function formatFrame(def: Def, loc: Source): string {
  const src = `${loc.file}:${loc.line}:${loc.col}`
  if (def.name === '(global)') return src
  return `${def.name} (${src})`
}

function formatStack(err: Error, frames: NodeJS.CallSite[]): string {
  let lines: string[] = []
  for (const frame of frames) {
    const script = frame.getScriptNameOrSourceURL()
    if (typeof script === 'string' && script.startsWith('wasm://')) {
      let debug = debugModules.get(frame.getThis() as WebAssembly.Instance)
      if (debug) {
        const located = locate(frame.getPosition(), debug)
        if (located) {
          for (const [def, src] of located.reverse())
            if (src) lines.push(formatFrame(def, src))
          continue
        }
      }
    }
    lines.push(frame.toString())
  }
  lines = lines.filter(l => !/cli\/(exec|worker)/.test(l))
  while (lines.length && /common\.abort.*wasm\/js\.rv/.test(lines[0])) lines.shift()
  const header = err.toString()
  return [header, ...lines.map(x => `    at ${x}`)].join('\n')
}

Error.prepareStackTrace = (err, frames) => formatStack(err, frames)
Error.stackTraceLimit = 100

async function loadWasm(buf: string | Uint8Array, imports: any = {}) {
  if (typeof buf === 'string')
    buf = await fs.readFile(buf)
  imports = { ...imports, support: support() }
  const res = await (WebAssembly.instantiate as any)(new Uint8Array(buf), imports, { builtins: ['js-string'], importedStringConstants: "strings" })
  const debug = DebugModule(buf)
  if (debug) debugModules.set(res.instance, debug)
  return res.instance.exports
}
