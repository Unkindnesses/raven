import { loadWasm, table } from './support'

export { main }

async function main(wasm: string, { memcheck = true } = {}) {
  let { _start } = await loadWasm(__dirname + '/' + wasm)
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
