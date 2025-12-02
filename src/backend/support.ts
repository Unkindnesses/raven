import { DebugModule, locate, Source, sections } from '../dwarf/parse.js'
import { Def } from '../dwarf/index.js'

export { loadWasm, support }

function call(obj: any, meth: any, ...args: any[]) {
  return obj[meth].call(obj, ...args)
}

function apply(obj: any, meth: any, args: any) {
  return call(obj, meth, ...args)
}

async function errcall(obj: any, meth: any, ...args: any[]) {
  try {
    let result = await obj[meth].call(obj, ...args)
    return [0, result]
  } catch (e) {
    return [1, e]
  }
}

function abort(obj: any, cause: any) {
  throw new Error(obj, cause ? { cause: cause } : {})
}

const process = (globalThis as any).process
const isNode =
  typeof process !== 'undefined' &&
  process.versions != null &&
  process.versions.node != null

interface JSEntry {
  code: string
  params: string[]
}

function support() {
  return {
    call, apply, abort,
    equal: (x: any, y: any) => x === y,
    identity: (x: any) => x,
    await: new (WebAssembly as any).Suspending((x: any) => x),
    errcall: new (WebAssembly as any).Suspending(errcall),
    debugger: () => { debugger }
  }
}

function inline(js: JSEntry[]) {
  const inline: Record<string, Function> = {}
  for (let i = 0; i < js.length; i++)
    inline[`js_${i}`] = new Function(...js[i].params, js[i].code)
  return inline
}

interface RavenMeta {
  js: JSEntry[]
}

function parseRavenMeta(buf: Uint8Array): RavenMeta | undefined {
  const [, table] = sections(buf)
  const data = table.get('raven.meta')
  if (!data) return
  return JSON.parse(new TextDecoder().decode(data))
}

const debugModules = new Map<WebAssembly.Instance, DebugModule>()

function formatFrame(def: Def, loc: Source): string {
  const src = `${loc.file}:${loc.line}:${loc.col}`
  if (def.name === '(global)') return src
  return `${def.name} (${src})`
}

function formatStack(err: Error, frames: any[]): string {
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

if (isNode) {
  (Error as any).prepareStackTrace = formatStack;
  (Error as any).stackTraceLimit = 100
}

async function loadWasm(buf: Uint8Array, imports: any = {}) {
  const meta = parseRavenMeta(buf)
  imports = { ...imports, support: support(), inline: inline(meta?.js ?? []) }
  const res = await (WebAssembly.instantiate as any)(new Uint8Array(buf), imports, { builtins: ['js-string'], importedStringConstants: "strings" })
  const debug = DebugModule(buf)
  if (debug) debugModules.set(res.instance, debug)
  return res.instance.exports
}
