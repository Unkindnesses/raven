import { Attr, Tag, Form } from '../dwarf/enums'
import { Source, Abbrev, DIE, Value, Def, Function, Stack } from '../dwarf/structs'
import { asBool, asNumber, asString, some } from '../utils/map'

export { DebugModule, Source, sections, locate }

const decoder = new TextDecoder()

class Reader {
  readonly bytes: Uint8Array
  readonly view: DataView
  readonly start: number
  readonly limit: number
  offset: number

  constructor(bytes: Uint8Array, offset = 0, limit = bytes.length, view?: DataView, start?: number) {
    this.bytes = bytes
    this.view = view ?? new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength)
    this.start = start ?? offset
    this.offset = offset
    this.limit = Math.min(limit, bytes.length)
  }

  get done(): boolean {
    return this.offset >= this.limit
  }

  remaining(): number {
    return this.limit - this.offset
  }

  private fits(length: number): boolean {
    return length >= 0 && this.offset + length <= this.limit
  }

  private ensure(length: number): void {
    if (!this.fits(length)) throw new Error('Unexpected EOF in DWARF section')
  }

  fork(length: number): Reader {
    this.ensure(length)
    const start = this.offset
    const end = start + length
    this.offset = end
    return new Reader(this.bytes, start, end, this.view, start)
  }

  skip(length: number): void {
    this.ensure(length)
    this.offset += length
  }

  peek(): number | undefined {
    this.ensure(1)
    return this.bytes[this.offset]
  }

  u8(): number {
    this.ensure(1)
    const value = this.bytes[this.offset]
    this.offset += 1
    return value
  }

  i8(): number {
    this.ensure(1)
    const value = this.view.getInt8(this.offset)
    this.offset += 1
    return value
  }

  u16(): number {
    this.ensure(2)
    const value = this.view.getUint16(this.offset, true)
    this.offset += 2
    return value
  }

  u32(): number {
    this.ensure(4)
    const value = this.view.getUint32(this.offset, true)
    this.offset += 4
    return value
  }

  u64(): bigint {
    this.ensure(8)
    const value = this.view.getBigUint64(this.offset, true)
    this.offset += 8
    return value
  }

  chunk(length: number): Uint8Array {
    this.ensure(length)
    const start = this.offset
    const end = start + length
    this.offset = end
    return this.bytes.subarray(start, end)
  }

  cstring(): string {
    let pos = this.offset
    while (pos < this.limit && this.bytes[pos] !== 0) pos += 1
    if (pos >= this.limit) throw new Error('Truncated DWARF string')
    const value = decoder.decode(this.bytes.subarray(this.offset, pos))
    this.offset = pos + 1
    return value
  }

  uleb(): number {
    let result = 0
    let shift = 0
    let pos = this.offset
    while (pos < this.limit) {
      const byte = this.bytes[pos]
      pos += 1
      result |= (byte & 0x7f) << shift
      if ((byte & 0x80) === 0) {
        this.offset = pos
        return result >>> 0
      }
      shift += 7
      if (shift > 35) throw new Error('ULEB128 too large')
      if (pos >= this.limit) throw new Error('Truncated ULEB128')
    }
    throw new Error('Unexpected EOF while reading ULEB128')
  }

  sleb(): number {
    if (this.done) throw new Error('Unexpected EOF while reading SLEB128')
    let result = 0
    let shift = 0
    let pos = this.offset
    let byte = 0
    while (pos < this.limit) {
      byte = this.bytes[pos]
      pos += 1
      result |= (byte & 0x7f) << shift
      shift += 7
      if ((byte & 0x80) === 0) {
        this.offset = pos
        if (shift < 32 && (byte & 0x40) !== 0) result |= (~0 << shift)
        return result | 0
      }
      if (shift > 35) throw new Error('SLEB128 too large')
    }
    throw new Error('Truncated SLEB128')
  }
}

function asTag(value: number): Tag {
  if (Reflect.get(Tag, value) === undefined) throw new Error(`Invalid DWARF tag ${value}`)
  return value as Tag
}

function asAttr(value: number): Attr {
  if (Reflect.get(Attr, value) === undefined) throw new Error(`Invalid DWARF attribute ${value}`)
  return value as Attr
}

function asForm(value: number): Form {
  if (Reflect.get(Form, value) === undefined) throw new Error(`Unsupported DWARF form ${value}`)
  return value as Form
}

interface LineRow {
  address: number
  source: Source
  isStmt: boolean
}

interface DebugModule {
  base: number
  lines: LineRow[]
  functions: Function[]
}

function parseDebugAbbrev(section: Uint8Array): Map<number, Abbrev> {
  const table = new Map<number, Abbrev>()
  const reader = new Reader(section)
  while (!reader.done) {
    const code = reader.uleb()
    if (code === 0) continue
    const tagValue = reader.uleb()
    const hasChildren = reader.u8()
    const attrs: [Attr, Form][] = []
    while (true) {
      const nameValue = reader.uleb()
      const formValue = reader.uleb()
      if (nameValue === 0 && formValue === 0) break
      attrs.push([asAttr(nameValue), asForm(formValue)])
    }
    table.set(code, new Abbrev(asTag(tagValue), attrs, hasChildren !== 0))
  }
  return table
}

function readValue(reader: Reader, form: Form): Value {
  switch (form) {
    case Form.addr:
      return [form, reader.u32()]
    case Form.sec_offset:
    case Form.data4:
      return [form, reader.u32()]
    case Form.data1:
      return [form, reader.u8()]
    case Form.data2:
      return [form, reader.u16()]
    case Form.data8:
      return [form, reader.u64()]
    case Form.string:
      return [form, reader.cstring()]
    case Form.flag:
      return [form, reader.u8() !== 0]
    default:
      throw new Error(`Unsupported DWARF form ${form}`)
  }
}

function parseDIE(unit: Reader, abbrevs: Map<number, Abbrev>): DIE | undefined {
  const code = unit.uleb()
  if (code === 0) return
  const abbrev = some(abbrevs.get(code))
  const attrs: [Attr, Value][] = []
  for (const [name, form] of abbrev.attrs)
    attrs.push([name, readValue(unit, form)])
  const children: DIE[] = []
  if (abbrev.children) {
    while (true) {
      const child = parseDIE(unit, abbrevs)
      if (!child) break
      children.push(child)
    }
  }
  return new DIE(abbrev.tag, attrs, children)
}

function parseDebugInfo(section: Uint8Array, abbrevs: Map<number, Abbrev>): DIE {
  const reader = new Reader(section)
  const unitLength = reader.u32()
  const unit = reader.fork(unitLength)
  unit.u16() // DWARF version
  unit.skip(4) // debug_abbrev_offset
  unit.u8() // address_size
  return some(parseDIE(unit, abbrevs))
}

function attrValue(attrs: [Attr, Value][], attr: Attr): Value[1] | undefined {
  for (const [name, value] of attrs)
    if (name === attr) return value[1]
}

function callSite(attrs: [Attr, Value][], files: string[]): Source | undefined {
  const fileIdx = attrValue(attrs, Attr.call_file)
  const line = attrValue(attrs, Attr.call_line)
  if (fileIdx === undefined || line === undefined) return
  const file = some(files[asNumber(fileIdx) - 1])
  const column = asNumber(attrValue(attrs, Attr.call_column))
  return { file, line: asNumber(line), col: asNumber(column) }
}

function dieFunction(die: DIE, files: string[]): Function {
  const name = asString(attrValue(die.attrs, Attr.name))
  const low = asNumber(attrValue(die.attrs, Attr.low_pc))
  const high = asNumber(attrValue(die.attrs, Attr.high_pc))
  const trampoline = asBool(attrValue(die.attrs, Attr.trampoline) ?? false)
  const def = Def(name, undefined, trampoline)
  const inlines = die.children
    .filter(child => child.tag === Tag.inlined_subroutine)
    .map(child => [dieFunction(child, files), callSite(child.attrs, files)] as [Function, Source | undefined])
  return { def, range: [low, high], inlines }
}

function functionsFrom(root: DIE, files: string[]): Function[] {
  const fs: Function[] = []
  for (const die of root.children) {
    if (die.tag !== Tag.subprogram) continue
    fs.push(dieFunction(die, files))
  }
  return fs
}

function parseDebugLine(section: Uint8Array): { rows: LineRow[], files: string[] } {
  const rows: LineRow[] = []
  const files: string[] = []
  const reader = new Reader(section)
  while (reader.remaining() >= 10) {
    const unitLength = reader.u32()
    const unit = reader.fork(unitLength)
    unit.u16() // version
    const headerLength = unit.u32()
    const header = unit.fork(headerLength)
    header.u8() // minimum_instruction_length
    header.u8() // maximum_operations_per_instruction
    header.u8() // default_is_stmt
    header.i8() // line_base
    header.u8() // line_range
    const opcodeBase = header.u8()
    header.skip(opcodeBase - 1) // standard_opcode_lengths
    while (!header.done)
      if (header.cstring() === '') break
    files.length = 0
    while (!header.done) {
      const name = header.cstring()
      if (name === '') break
      header.uleb() // dir
      header.uleb() // last_modified
      header.uleb() // file size
      files.push(name)
    }
    let [address, line, column, file, isStmt] = [0, 1, 0, 1, false]
    const reset = () =>
      [address, line, column, file, isStmt] = [0, 1, 0, 1, false]
    while (!unit.done) {
      const opcode = unit.u8()
      if (opcode >= opcodeBase) throw new Error('unsupported opcode')
      switch (opcode) {
        case 0x00:
          if (unit.uleb() !== 1 || unit.u8() !== 1) throw new Error('Unsupported extended opcode')
          reset()
          break
        case 0x01:
          if (file >= 1 && file <= files.length)
            rows.push({ address, source: { file: files[file - 1], line, col: column }, isStmt })
          break
        case 0x02:
          address += unit.uleb()
          break
        case 0x03:
          line += unit.sleb()
          break
        case 0x04:
          file = unit.uleb()
          break
        case 0x05:
          column = unit.uleb()
          break
        case 0x06:
          isStmt = !isStmt
          break
      }
    }
  }
  rows.sort((a, b) => a.address - b.address)
  return { rows, files }
}

function sections(bytes: Uint8Array): [number, Map<string, Uint8Array>] {
  let codeBase = 0
  const table = new Map<string, Uint8Array>()
  if (bytes.length < 8) return [codeBase, table]
  const reader = new Reader(bytes)
  reader.skip(8)
  while (!reader.done) {
    const id = reader.u8()
    const size = reader.uleb()
    const payload = reader.fork(size)
    if (id === 0) {
      const nameLen = payload.uleb()
      const nameBytes = payload.chunk(nameLen)
      const name = decoder.decode(nameBytes)
      const data = payload.chunk(payload.remaining())
      table.set(name, data)
    } else if (id === 0x0a) codeBase = payload.start
  }
  return [codeBase, table]
}

function DebugModule(bytes: Uint8Array): DebugModule | undefined {
  const [base, table] = sections(bytes)
  const lineSection = table.get('.debug_line')
  const infoSection = table.get('.debug_info')
  const abbrevSection = table.get('.debug_abbrev')
  if (!lineSection || !infoSection || !abbrevSection) return
  const abbrevs = parseDebugAbbrev(abbrevSection)
  const { rows: lines, files } = parseDebugLine(lineSection)
  const functions = functionsFrom(parseDebugInfo(infoSection, abbrevs), files)
  return { base, lines, functions }
}

function search<T>(items: T[], cmp: (item: T) => -1 | 0 | 1): T | undefined {
  if (!items.length) return
  let lo = 0
  let hi = items.length - 1
  while (lo <= hi) {
    const mid = Math.floor((lo + hi) / 2)
    const item = items[mid]
    const res = cmp(item)
    if (res === 0) return item
    if (res < 0) hi = mid - 1
    else lo = mid + 1
  }
}

function findByRange<T>(items: T[], pc: number, getRange: (item: T) => [number, number]): T | undefined {
  return search(items, (item) => {
    const [lo, hi] = getRange(item)
    if (pc < lo) return -1
    if (pc >= hi) return 1
    return 0
  })
}

function findLineRow(lines: LineRow[], pc: number): LineRow | undefined {
  return search(lines, row => {
    if (pc === row.address) return 0
    return pc < row.address ? -1 : 1
  })
}

function locate(pc: number, m: DebugModule): Stack | undefined {
  const relative = pc - m.base
  if (relative < 0) return
  const fn = findByRange(m.functions, relative, fn => fn.range)
  if (!fn) return
  const line = findLineRow(m.lines, relative)
  const frames: Stack = [[fn.def, undefined]]
  let current = fn
  while (true) {
    const next = findByRange(current.inlines, relative, inline => inline[0].range)
    if (!next) break
    const [child, call] = next
    frames[frames.length - 1][1] = call
    frames.push([child.def, undefined])
    current = child
  }
  frames[frames.length - 1][1] = line?.source
  return frames
}
