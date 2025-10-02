import { test as _test } from 'uvu'
import * as assert from 'assert'
import { compile } from '../src/backend/compiler'
import { spawnSync } from 'node:child_process'
import * as path from 'node:path'
import { only, some } from '../src/utils/map'

type LineInfo = {
  file: string
  line: number
  column: number
  bp: boolean
}

const test = process.platform === 'win32' ? _test.skip : _test

function runTool(cmd: string, args: string[], err = false): string {
  const res = spawnSync(cmd, args, { encoding: 'utf-8' })
  if (res.error) throw res.error
  const out = res.stdout + (err ? res.stderr : '')
  if (res.status !== 0) throw new Error(out || `${cmd} ${args.join(' ')} failed`)
  return out
}

const dwarfdump = (f: string): string => runTool('llvm-dwarfdump', [f], true)
const dwarf_verify = (f: string): string => runTool('llvm-dwarfdump', ['--verify', f], true)
const dwarf_verify_lines = (f: string): string => runTool('llvm-dwarfdump', ['--debug-line', '--verify', f], true)
const headers = (f: string): string => runTool('wasm-objdump', ['-h', f])
const disassembly = (f: string): string => runTool('wasm-objdump', ['-d', f])

const code_offset = (f: string): number => {
  const match = headers(f).match(/Code start=0x([0-9a-f]+)/i)
  if (!match) throw new Error('Code start not found')
  return parseInt(match[1], 16)
}

const callsites = (wasm: string, func: string): number[] => {
  const dis = disassembly(wasm)
  const matches = dis.matchAll(new RegExp(`([\\da-f]+):[^\n]*\\| call \\d+ <${func}>`, 'gi'))
  const sites: number[] = []
  for (const m of matches)
    sites.push(parseInt(m[1], 16))
  return sites
}

const linetable = (f: string): Array<[number, LineInfo | null]> => {
  const dump = runTool('llvm-dwarfdump', ['--debug-line', f])
  const fileMatches = Array.from(dump.matchAll(/file_names\[[\s\d]+\]:\s+name: "(.*)"/g))
  const files = fileMatches.map(m => m[1])
  const lineMatches = dump.matchAll(/^0x([0-9a-f]+)\s+(\d+)\s+(\d+)\s+(\d+)(?:\s+\d+){3}[ \t]*(.*)$/gmi)
  const entries: Array<[number, LineInfo | null]> = []
  for (const m of lineMatches) {
    const flags = m[5] || ''
    const info = flags.includes('end_sequence') ? null : {
      file: some(files[parseInt(m[4]) - 1]),
      line: parseInt(m[2]),
      column: parseInt(m[3]),
      bp: flags.includes('is_stmt'),
    }
    entries.push([parseInt(m[1], 16), info])
  }
  return entries
}

function lineinfo(table: Array<[number, LineInfo | null]>, ip: number): LineInfo | null {
  for (const [addr, info] of table) {
    if (addr === ip) return info
    else if (addr > ip) break
  }
  throw new Error('not found')
}

const srcDir = path.join(__dirname, 'language')
let wasm = path.join(srcDir, 'pow.wasm')

test('build', async () => {
  await compile(path.join(srcDir, 'pow.rv'),
    { options: { inline: false } })
})

test('Basic info', async () => {
  const out = dwarfdump(wasm)
  assert.ok(out.includes('DW_AT_producer\t("raven version 0.0.0")'))
  assert.ok(out.includes('DW_AT_language\t(DW_LANG_C99)'))
})

test('Verify', async () => {
  let out = dwarf_verify(wasm)
  assert.ok(!out.includes('warning'))
  assert.ok(out.includes('No errors.'))
  out = dwarf_verify_lines(wasm)
  assert.ok(!out.includes('warning'))
  assert.ok(out.includes('No errors.'))
})

test('Line info', async () => {
  const ip = only(callsites(wasm, 'debug_pow:1')) - code_offset(wasm)
  const table = linetable(wasm)
  const info = some(lineinfo(table, ip))
  assert.ok(info.file.endsWith('test/language/pow.rv'))
  assert.equal(info.line, 10)
  assert.equal(info.column, 18)
})

_test.run()
