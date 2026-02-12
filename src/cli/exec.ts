#!/usr/bin/env -S node --enable-source-maps --experimental-wasm-jspi
import * as path from 'node:path'
import { pathToFileURL } from 'node:url'
import { loadWasm } from '../backend/support.js'

const wasm = pathToFileURL(path.resolve(process.argv[2]))
let { _start, jsrefs, allocs, frees }: any = await loadWasm(wasm)
_start = (WebAssembly as any).promising(_start)

try {
  await (_start as any)()
} catch (e) {
  console.error(e)
  process.exit(1)
}

if (allocs.value !== frees.value)
  console.warn(`Memory management fault: ${allocs.value} allocs != ${frees.value} frees`)

for (let i = 0; i < jsrefs.length; i++)
  if (jsrefs.get(i) !== null)
    console.warn("Memory management fault: JSObject")
