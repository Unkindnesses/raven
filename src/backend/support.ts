import * as fs from 'fs/promises'
import { DebugModule, locate, sections, Source } from '../dwarf/parse'
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

async function awate(ref: number) {
  return createRef(await fromRef(ref))
}

let strings: string[] = []

function string(i: number) {
  return createRef(strings[i])
}

function release(ref: number) {
  delete table[ref]
}

function global() {
  return createRef(globalThis)
}

function property(obj: number, prop: number) {
  const r = fromRef(obj)[fromRef(prop)]
  return createRef(r)
}

function _call(obj: any, meth: any, ...args: any[]) {
  const func = obj[meth]
  if (func === undefined) {
    throw new Error(`No such method ${meth}`)
  }
  return createRef(func.call(obj, ...args))
}

function call(obj: number, meth: number, ...args: number[]) {
  return _call(fromRef(obj), fromRef(meth), ...args.map(fromRef))
}

function apply(obj: number, meth: number, args: number) {
  return _call(fromRef(obj), fromRef(meth), ...fromRef(args))
}

async function errcall(_obj: number, _meth: number, ..._args: number[]) {
  let [obj, meth, ...args] = [_obj, _meth, ..._args].map(fromRef)
  const func = obj[meth]
  if (func === undefined) {
    throw new Error(`No such method ${meth}`)
  }
  try {
    let result = await func.call(obj, ...args)
    return [0, createRef(result)]
  } catch (e) {
    return [1, createRef(e)]
  }
}

function equal(a: number, b: number) {
  return fromRef(a) === fromRef(b)
}

function abort(obj: number, cause: number) {
  throw new Error(fromRef(obj), cause ? { cause: fromRef(cause) } : {})
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

const support = {
  global, property, call, apply, string,
  createRef, fromRef, abort,
  equal, release,
  await: new (WebAssembly as any).Suspending(awate),
  errcall: new (WebAssembly as any).Suspending(errcall),
  debugger: () => { debugger }
}

interface Metadata {
  strings: string[]
  jsalloc: boolean
}

function meta(bytes: Uint8Array): Metadata | undefined {
  const bs = sections(bytes)[1].get('raven.meta')
  if (!bs) return
  return JSON.parse(new TextDecoder().decode(bs)) as Metadata
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
    const script = frame.getScriptNameOrSourceURL?.()
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
  lines = lines.filter(l => !/backend\/(exec|worker)/.test(l))
  while (lines.length && /common\.abort.*wasm\/js\.rv/.test(lines[0])) lines.shift()
  const header = err.toString()
  return [header, ...lines.map(x => `    at ${x}`)].join('\n')
}

Error.prepareStackTrace = (err, frames) => formatStack(err, frames)

async function loadWasm(buf: string | Uint8Array, imports: any = {}) {
  if (typeof buf === 'string')
    buf = await fs.readFile(buf)
  const m = meta(buf)
  if (m === undefined) throw new Error('Not a Raven wasm module.')
  imports = { ...imports, support: { ...support, string: (i: number) => createRef(m.strings[i]) } }
  const res = await WebAssembly.instantiate(new Uint8Array(buf), imports)
  const debug = DebugModule(buf)
  if (debug) debugModules.set(res.instance, debug)
  return [res.instance.exports, m] as const
}
