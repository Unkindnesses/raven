#!/usr/bin/env node --experimental-wasm-stack-switching
import { loadWasm, table } from './support'

let binary: string | undefined

async function main({ memcheck = true } = {}) {
  let wasm: string | Uint8Array = process.argv[2]
  if (binary) wasm = Buffer.from(binary, 'base64')
  let { _start } = await loadWasm(wasm)
  _start = (WebAssembly as any).promising(_start)
  try {
    await (_start as any)()
  } catch (e) {
    console.error(e)
    process.exit(1)
  }
  if (memcheck && Object.keys(table).length !== 0)
    throw new Error("Memory management fault: JSObject")
}

setImmediate(() => main())
