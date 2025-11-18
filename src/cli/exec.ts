#!/usr/bin/env -S node --enable-source-maps --experimental-wasm-jspi
import * as fs from 'fs/promises'
import { loadWasm, table } from '../backend/support.js'

let binary: string | undefined

async function main() {
  const wasm = binary ? Buffer.from(binary, 'base64') : await fs.readFile(process.argv[2])
  let { _start } = await loadWasm(wasm)
  _start = (WebAssembly as any).promising(_start)
  try {
    await (_start as any)()
  } catch (e) {
    console.error(e)
    process.exit(1)
  }
  if (Object.keys(table).length !== 0)
    throw new Error("Memory management fault: JSObject")
}

setImmediate(() => main())
