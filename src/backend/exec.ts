#!/usr/bin/env -S node --experimental-wasm-stack-switching
import { loadWasm, table, support } from './support'

let binary: string | undefined

async function main() {
  let wasm: string | Uint8Array = process.argv[2]
  if (binary) wasm = Buffer.from(binary, 'base64')
  let [{ _start }, { jsalloc }] = await loadWasm(wasm)
  _start = (WebAssembly as any).promising(_start)
  try {
    await (_start as any)()
  } catch (e) {
    console.error(e)
    process.exit(1)
  }
  if (jsalloc && Object.keys(table).length !== 0)
    throw new Error("Memory management fault: JSObject")
}

setImmediate(() => main())
