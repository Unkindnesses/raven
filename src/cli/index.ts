#!/usr/bin/env -S node --enable-source-maps --experimental-wasm-jspi
import * as commander from 'commander'
import * as fs from 'fs/promises'
import { randomUUID } from 'crypto'
import * as nodeRepl from 'node:repl'
import * as os from 'os'
import * as path from 'path'
import { Compiler, compile, compileJS, exec } from './compile.js'
import { REPL } from './repl.js'
import { Caching, time } from '../utils/cache.js'
import pkg from '../../package.json' with { type: 'json' }

function formatTime(ns: bigint): string {
  const ms = Number(ns) / 1_000_000
  if (ms < 1) return `${(ms * 1000).toFixed(2)}μs`
  if (ms < 1000) return `${ms.toFixed(2)}ms`
  return `${(ms / 1000).toFixed(2)}s`
}

function printTiming(c: Compiler) {
  const phases: any = [
    ['Definitions', [c.pipe.defs, c.pipe.methods]],
    ['Interpret  ', [c.pipe.interp.results]],
    ['Inference  ', [c.pipe.inferred]],
    ['Expansion  ', [c.pipe.expanded]],
    ['Inlining   ', [c.pipe.inlined]],
    ['Refcounts  ', [c.pipe.counted]],
    ['WASM       ', [c.pipe.wasm]]
  ]
  console.log(`Load        ${formatTime(c.time).padStart(10)}`)
  for (const [name, caches] of phases) {
    const t = caches.map((x: Caching) => time(x)).reduce((a: number, b: number) => a + b, 0n)
    console.log(`${name} ${formatTime(t).padStart(10)}`)
  }
  const total = time(c.pipe) + c.time
  console.log(`Total       ${formatTime(total).padStart(10)} `)
}

const installDir = path.join(os.homedir(), '.raven')
const installFile = path.join(installDir, 'id')

const ciEnvVars = [
  'CI', 'CONTINUOUS_INTEGRATION', 'BUILD_NUMBER', 'RUN_ID', 'GITHUB_ACTIONS',
  'GITHUB_RUN_ID', 'GITHUB_WORKFLOW', 'GITLAB_CI', 'TEAMCITY_VERSION',
  'BUILDKITE', 'TF_BUILD'
]

async function installID(): Promise<string> {
  try {
    const existing = (await fs.readFile(installFile, 'utf8')).trim()
    if (existing) return existing
  } catch (err) {
    if ((err as any).code !== 'ENOENT') throw err
  }
  const id = randomUUID()
  await fs.mkdir(installDir, { recursive: true })
  await fs.writeFile(installFile, `${id}\n`)
  return id
}

async function checkUpdate() {
  if (ciEnvVars.some(x => process.env[x])) return
  try {
    const id = await installID()
    const res = await fetch('https://mikeinnes.io/api/update', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ id, version: pkg.version })
    })
    if (!res.ok) throw new Error(`Update request failed with status ${res.status}`)
    await res.json()
  } catch (err) {
    console.error('Unable to update Raven installation', err)
  }
}

async function startRepl() {
  const raven = await REPL.create()
  try {
    const server = nodeRepl.start({
      prompt: '> ',
      ignoreUndefined: true,
      eval(cmd, _context, _filename, callback) {
        const source = cmd.replace(/[\r\n]+$/, '')
        if (!source.trim()) return callback(null, undefined)
        raven.eval(source).then(
          () => callback(null, undefined),
          error => {
            const err = error instanceof Error ? error : new Error(String(error))
            callback(err, undefined)
          }
        )
      }
    })
    const historyPath = path.join(os.homedir(), '.raven_history')
    await new Promise(resolve => server.setupHistory(historyPath, err => {
      if (err) console.warn('Unable to load REPL history', err)
      resolve(undefined)
    }))
    await new Promise(resolve => server.on('exit', resolve))
  } finally {
    await raven.close()
  }
}

async function main() {
  const program = new commander.Command()
  program
    .name('raven')
    .description('The Raven Programming Language')

  program
    .command('version')
    .description('Print the Raven version')
    .action(() => { console.log(`${pkg.version}`) })

  program
    .command('build')
    .description('Compile a Raven source file')
    .argument('<source>', 'Source file to compile')
    .option('--js', 'Emit JS')
    .option('-o, --output <file>', 'Rename output file')
    .option('--time', 'Print compiler phase timing information')
    .action(async (source, { output, js, time }) => {
      let { inline, memcheck, strip } = program.optsWithGlobals()
      source = path.resolve(process.cwd(), source)
      let [compiler] = await (js ? compileJS : compile)(source, { options: { inline, memcheck }, output, strip })
      if (time) printTiming(compiler)
    })

  program
    .argument('[source] [args...]', 'Source file to execute')
    .option('--no-inline', 'Disable function inlining')
    .option('--no-memcheck', 'Disable allocation checks')
    .option('--strip', 'Remove debug metadata from the binary')
    .action(async (xs) => {
      let { inline, memcheck, strip } = program.optsWithGlobals()
      let [source, ...args] = xs
      if (!source) return await startRepl()
      source = path.resolve(process.cwd(), source)
      await exec(source, args, { options: { inline, memcheck }, strip })
    })

  await Promise.all([checkUpdate(), program.parseAsync(process.argv)])
}

await main()
