import { Form } from './enums.js'
import * as path from 'node:path'
import { HashMap, some } from '../utils/map.js'
import { Attr, Tag } from './enums.js'
import { DIE, Abbrev, LineTable, abbrev, Value, Source, Def, Function, LineInfo } from './structs.js'
import isEqual from 'lodash/isEqual.js'

export {
  buildInlineTree, functionDie,
  leb128U, leb128S, withSize as withSizeU32, putStringZ,
  writeU8, writeI8, writeU16, writeU32,
  writeValue, emitDIE, debug_abbrev, debug_info, debug_line,
  lineFiles
}

// Collect inline frames

type Inlined = [Function, Source | undefined]

function buildInlineTree(lines: [number, LineInfo][], bodySize: number, base: number, root: Def): Inlined[] {
  const active: Inlined[] = []
  const children: Inlined[] = []
  const closeFrom = (depth: number, pos: number) => {
    for (let i = active.length - 1; i >= depth; i--) {
      active[i][0].range[1] = base + pos
      active.pop()
    }
  }
  for (const [pos, li] of lines) {
    const frames = li.src.slice(1)
    let common = 0
    while (common < active.length && common < frames.length) {
      const node = active[common]
      const frame = frames[common]
      const callSite = li.src[common][1]
      if (isEqual(node[0].def, frame[0]) && isEqual(node[1], callSite)) common++
      else break
    }
    closeFrom(common, pos)
    for (let depth = common; depth < frames.length; depth++) {
      const frame = frames[depth]
      const callSite = li.src[depth][1]
      const fn: Function = { def: frame[0], range: [base + pos, base + pos], inlines: [] }
      const node: Inlined = [fn, callSite]
      const parent = depth === 0 ? children : active[depth - 1][0].inlines
      parent.push(node)
      active[depth] = node
    }
  }
  closeFrom(0, bodySize)
  return children
}

function callAttrs(call: Source | undefined, files: string[]): [Attr, Value][] {
  if (!call || call.line <= 0) return []
  const idx = files.indexOf(call.file)
  if (idx === -1) return []
  const attrs: [Attr, Value][] = [
    [Attr.call_file, [Form.data4, idx + 1]],
    [Attr.call_line, [Form.data4, call.line]]
  ]
  if (call.col > 0) attrs.push([Attr.call_column, [Form.data4, call.col]])
  return attrs
}

function inlineDie(fn: Function, call: Source | undefined, files: string[]): DIE {
  const attrs: [Attr, Value][] = [
    [Attr.name, [Form.string, fn.def.name]],
    [Attr.low_pc, [Form.addr, fn.range[0]]],
    [Attr.high_pc, [Form.addr, fn.range[1]]]
  ]
  attrs.push(...callAttrs(call, files))
  const children = fn.inlines.map(child => inlineDie(...child, files))
  return new DIE(Tag.inlined_subroutine, attrs, children)
}

function functionDie(fn: Function, files: string[]): DIE {
  const attrs: [Attr, Value][] = [
    [Attr.name, [Form.string, fn.def.name]],
    [Attr.low_pc, [Form.addr, fn.range[0]]],
    [Attr.high_pc, [Form.addr, fn.range[1]]]
  ]
  const children = fn.inlines.map(child => inlineDie(...child, files))
  return new DIE(Tag.subprogram, attrs, children)
}

// Values

function leb128U(x: number | bigint, out: number[] = []): number[] {
  let v = BigInt(x)
  while (true) {
    let byte = Number(v & 0x7fn)
    v >>= 7n
    if (v !== 0n) byte |= 0x80
    out.push(byte)
    if (v === 0n) break
  }
  return out
}

function leb128S(x: number | bigint, out: number[] = []): number[] {
  let v = BigInt(x)
  while (true) {
    let byte = Number(v & 0x7fn)
    v >>= 7n
    const sign = (byte & 0x40) === 0
    if ((v === 0n && sign) || (v === -1n && !sign)) {
      out.push(byte)
      break
    } else {
      out.push(byte | 0x80)
    }
  }
  return out
}

function writeU8(out: number[], x: number) { out.push(x & 0xff) }
function writeI8(out: number[], x: number) { out.push((x + 256) & 0xff) }
function writeU16(out: number[], x: number) { out.push(x & 0xff, (x >>> 8) & 0xff) }
function writeU32(out: number[], x: number) { out.push(x & 0xff, (x >>> 8) & 0xff, (x >>> 16) & 0xff, (x >>> 24) & 0xff) }

function withSize(out: number[], f: (buf: number[]) => void) {
  const buf: number[] = []
  f(buf)
  writeU32(out, buf.length)
  out.push(...buf)
}

function putStringZ(out: number[], s: string) {
  const bytes = new TextEncoder().encode(s)
  out.push(...bytes)
  writeU8(out, 0x00)
}

function writeValue(out: number[], fv: Value) {
  const [form, v] = fv
  switch (form) {
    case Form.addr:
    case Form.sec_offset:
    case Form.data4:
      writeU32(out, v)
      break
    case Form.data1:
      writeU8(out, v)
      break
    case Form.data2:
      writeU16(out, v)
      break
    case Form.data8: {
      const n = v as bigint
      for (let i = 0n; i < 8n; i++) writeU8(out, Number((n >> (8n * i)) & 0xffn))
      break
    }
    case Form.string:
      putStringZ(out, v)
      break
    case Form.flag:
      writeU8(out, v ? 1 : 0)
      break
    default:
      throw new Error(`Unsupported DWARF form: ${form}`)
  }
}

function emitDIE(out: number[], die: DIE, codes: HashMap<Abbrev, number>) {
  leb128U(some(codes.get(abbrev(die))), out) // abbreviation code
  for (const [_, v] of die.attrs) writeValue(out, v) // attribute value
  if (die.children.length > 0) {
    for (const child of die.children) emitDIE(out, child, codes)
    leb128U(0, out) // NULL entry terminates children
  }
}

// Abbrev section

function debug_abbrev(out: number[], as: Abbrev[]) {
  for (let i = 0; i < as.length; i++) {
    leb128U(i + 1, out) // abbreviation code
    leb128U(as[i].tag, out)
    writeU8(out, as[i].children ? 1 : 0)
    for (const [k, v] of as[i].attrs) {
      leb128U(k, out)
      leb128U(v, out)
    }
    leb128U(0, out) // end attributes (name)
    leb128U(0, out) // end attributes (form)
  }
  writeU8(out, 0x00) // end abbreviations
}

function debug_info(out: number[], info: DIE, as: Abbrev[]) {
  const codes = new HashMap<Abbrev, number>()
  for (let i = 0; i < as.length; i++) codes.set(as[i], i + 1)
  withSize(out, buf => {
    writeU16(buf, 4) // DWARF version
    writeU32(buf, 0) // debug_abbrev_offset
    writeU8(buf, 0x04) // address_size
    emitDIE(buf, info, codes)
  })
}

// Line table

function ln_end_sequence(out: number[]) {
  writeU8(out, 0x00)
  leb128U(0x01, out)
  writeU8(out, 0x01)
}

function lineFiles(lt: LineTable): string[] {
  const files: string[] = []
  for (const [_, info] of lt.lines)
    for (const [__, src] of info.src)
      if (src && !files.includes(src.file)) files.push(src.file)
  return files
}

function debug_line(out: number[], lt: LineTable) {
  const files = lineFiles(lt)
  withSize(out, buf => {
    writeU16(buf, 4) // version
    withSize(buf, hdr => {
      writeU8(hdr, 0x01) // minimum_instruction_length
      writeU8(hdr, 0x01) // maximum_operations_per_instruction
      writeU8(hdr, 0x00) // default_is_stmt
      writeI8(hdr, -3) // line_base
      writeI8(hdr, 12) // line_range
      writeI8(hdr, 13) // opcode_base
      // standard_opcode_lengths
      for (const i of [0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1]) writeI8(hdr, i)
      // dirs go here
      writeU8(hdr, 0x00) // dirs
      for (const file of files) {
        const abs = path.isAbsolute(file) ? file : path.resolve(process.cwd(), file)
        putStringZ(hdr, abs)
        leb128U(0, hdr) // dir
        leb128U(0, hdr) // last modified
        leb128U(0, hdr) // file size
      }
      writeU8(hdr, 0x00)
    })
    let [offset, line, col, file, bp, open] = [0, 1, 0, files.length > 0 ? files[0] : '', false, false]
    const reset = () => {
      [offset, line, col, file, bp, open] = [0, 1, 0, files.length > 0 ? files[0] : '', false, false]
    }
    reset()
    for (const [o, info] of lt.lines) {
      const src = info.src[info.src.length - 1][1]
      writeU8(buf, 0x02) // advance_pc
      leb128U(o - offset, buf)
      offset = o
      if (!src) {
        ln_end_sequence(buf)
        reset()
        continue
      }
      if (src.line !== line) {
        writeU8(buf, 0x03) // advance_line
        leb128S(src.line - line, buf)
        line = src.line
      }
      if (src.file !== file) {
        writeU8(buf, 0x04) // set_file
        leb128U(files.indexOf(src.file) + 1, buf)
        file = src.file
      }
      if (src.col !== col) {
        writeU8(buf, 0x05) // set_column
        leb128U(src.col, buf)
        col = src.col
      }
      if (info.bp !== bp) {
        writeU8(buf, 0x06) // negate_stmt
        bp = info.bp
      }
      writeU8(buf, 0x01) // copy
      open = true
    }
    if (open) ln_end_sequence(buf)
  })
}
