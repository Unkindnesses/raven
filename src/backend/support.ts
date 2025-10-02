import * as fs from 'fs/promises'

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

function string(i: number) {
  return createRef(support.strings[i])
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
  global, property, call, apply,
  string, strings: [] as string[],
  createRef, fromRef, abort,
  equal, release,
  await: new (WebAssembly as any).Suspending(awate),
  errcall: new (WebAssembly as any).Suspending(errcall),
  debugger: () => { debugger }
}

function readVarUint(bytes: Uint8Array, offset: number): [number, number] | null {
  let result = 0
  let shift = 0
  let pos = offset
  while (pos < bytes.length) {
    const byte = bytes[pos]
    pos += 1
    result |= (byte & 0x7f) << shift
    if ((byte & 0x80) === 0) return [result >>> 0, pos]
    shift += 7
    if (shift > 35) break
  }
  return null
}

function extractStrings(bytes: Uint8Array): string[] | null {
  if (bytes.length < 8) return null
  const target = 'raven.strings'
  const decoder = new TextDecoder()
  let offset = 8 // magic + version
  while (offset < bytes.length) {
    const id = bytes[offset]
    offset += 1
    const sizeRead = readVarUint(bytes, offset)
    if (!sizeRead) return null
    const [size, sizeEnd] = sizeRead
    offset = sizeEnd
    const sectionEnd = offset + size
    if (sectionEnd > bytes.length) return null
    if (id === 0) {
      const nameRead = readVarUint(bytes, offset)
      if (!nameRead) return null
      const [nameLen, nameEnd] = nameRead
      const nameStop = nameEnd + nameLen
      if (nameStop > sectionEnd) return null
      const name = decoder.decode(bytes.slice(nameEnd, nameStop))
      if (name === target) {
        const payload = bytes.slice(nameStop, sectionEnd)
        if (payload.length === 0) return []
        const json = decoder.decode(payload)
        return JSON.parse(json)
      }
    }
    offset = sectionEnd
  }
  return null
}

async function loadWasm(buf: string | Uint8Array, imports = { support }) {
  if (typeof buf === 'string')
    buf = await fs.readFile(buf)
  support.strings = extractStrings(buf) ?? []
  let res: any = await WebAssembly.instantiate(buf, imports as any)
  return res.instance.exports
}
