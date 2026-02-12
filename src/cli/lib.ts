#!/usr/bin/env -S node --enable-source-maps --experimental-wasm-jspi
import { loadWasm } from '../backend/support.js'

export { __ravenInline }

async function __ravenInline(base64: string, check = false) {
  return await __ravenLoad(Buffer.from(base64, 'base64'), check)
}

function checkMemory({ allocs, frees, jsrefs }: any) {
  if (allocs.value !== frees.value)
    console.warn(`Memory management fault: ${allocs.value} allocs != ${frees.value} frees`)
  for (let i = 0; i < jsrefs.length; i++)
    if (jsrefs.get(i) !== null)
      console.warn("Memory management fault: JSObject")
}

async function __ravenLoad(wasm: Uint8Array | URL, check: boolean) {
  const exports = await loadWasm(wasm)
  const _start = (WebAssembly as any).promising(exports._start)
  await _start()
  if (check) checkMemory(exports)
  return (name: string) => {
    const fn = (exports as any)[name]
    if (typeof fn !== 'function') throw new Error(`Missing Raven export: ${name}`)
    return (WebAssembly as any).promising(fn)
  }
}
