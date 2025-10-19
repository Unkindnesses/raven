import { Attr, Tag, Form } from '../dwarf/enums'
import { Source, Abbrev } from '../dwarf/structs'

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
    if (this.done) return
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

interface FunctionRange {
  name: string
  low: number
  high: number
  trampoline: boolean
}

interface DebugModule {
  base: number
  lines: LineRow[]
  functions: FunctionRange[]
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

function readFormValue(reader: Reader, form: Form, addressSize: number): number | bigint | string | boolean {
  switch (form) {
    case Form.addr:
      return addressSize === 8 ? Number(reader.u64()) : reader.u32()
    case Form.sec_offset:
    case Form.data4:
      return reader.u32()
    case Form.data1:
      return reader.u8()
    case Form.data2:
      return reader.u16()
    case Form.data8:
      return reader.u64()
    case Form.string:
      return reader.cstring()
    case Form.flag:
      return reader.u8() !== 0
    default:
      throw new Error(`Unsupported DWARF form ${form}`)
  }
}

function parseDebugInfo(section: Uint8Array, abbrevs: Map<number, Abbrev>): FunctionRange[] {
  const functions: FunctionRange[] = []
  const reader = new Reader(section)
  while (reader.remaining() >= 11) {
    const unitLength = reader.u32()
    const unit = reader.fork(unitLength)
    unit.skip(2)
    unit.skip(4)
    const addressSizeByte = unit.u8()
    const addressSize = bytesPerAddress(addressSizeByte)
    const stack: Tag[] = []
    while (!unit.done) {
      const code = unit.uleb()
      if (code === 0) {
        if (stack.length === 0) break
        stack.pop()
        continue
      }
      const abbrev = abbrevs.get(code)
      if (!abbrev) throw new Error(`Missing DWARF abbrev ${code}`)
      const values = new Map<Attr, number | bigint | string | boolean>()
      for (const [name, form] of abbrev.attrs)
        values.set(name, readFormValue(unit, form, addressSize))
      if (abbrev.tag === Tag.subprogram) {
        const low = values.get(Attr.low_pc)
        const high = values.get(Attr.high_pc)
        if (typeof low === 'number' && typeof high === 'number') {
          const rawName = values.get(Attr.name)
          const trampoline = values.get(Attr.trampoline) === true
          const name = typeof rawName === 'string' ? rawName : ''
          functions.push({ name, low, high, trampoline })
        }
      }
      if (abbrev.children) stack.push(abbrev.tag)
    }
  }
  functions.sort((a, b) => a.low - b.low)
  return functions
}

function bytesPerAddress(size: number): number {
  if (size === 0) return 4
  return size
}

function parseDebugLine(section: Uint8Array): LineRow[] {
  const rows: LineRow[] = []
  const reader = new Reader(section)
  while (reader.remaining() >= 10) {
    const unitLength = reader.u32()
    const unit = reader.fork(unitLength)
    const version = unit.u16()
    if (version !== 4) {
      unit.offset = unit.limit
      continue
    }
    const headerLength = unit.u32()
    const header = unit.fork(headerLength)
    const minimumInstructionLength = header.u8()
    header.skip(1)
    const defaultIsStmtByte = header.u8()
    const lineBase = header.i8()
    const lineRange = header.u8()
    const opcodeBase = header.u8()
    header.skip(opcodeBase - 1)
    while (!header.done) {
      const next = header.peek()
      if (next === undefined) break
      if (next === 0) {
        header.skip(1)
        break
      }
      header.cstring()
    }
    const files: string[] = []
    while (!header.done) {
      const next = header.peek()
      if (next === undefined) break
      if (next === 0) {
        header.skip(1)
        break
      }
      const name = header.cstring()
      header.uleb()
      header.uleb()
      header.uleb()
      files.push(name)
    }
    let [address, line, column, file, isStmt] = [0, 1, 0, 1, defaultIsStmtByte !== 0]
    const reset = () => {
      [address, line, column, file, isStmt] = [0, 1, 0, 1, defaultIsStmtByte !== 0]
    }
    while (!unit.done) {
      const opcode = unit.u8()
      if (opcode === 0) {
        const length = unit.uleb()
        if (length === 0) continue
        const subopcode = unit.u8()
        const payloadLength = length - 1
        if (payloadLength < 0) throw new Error('Invalid DWARF line payload length')
        const payload = unit.chunk(payloadLength)
        switch (subopcode) {
          case 0x01:
            reset()
            break
          case 0x02:
            if (payloadLength === 4) {
              const payloadView = new DataView(payload.buffer, payload.byteOffset, payload.byteLength)
              address = payloadView.getUint32(0, true)
            }
            break
          default:
            break
        }
        continue
      }
      if (opcode < opcodeBase) {
        switch (opcode) {
          case 0x01:
            if (file >= 1 && file <= files.length)
              rows.push({ address, source: { file: files[file - 1], line, col: column }, isStmt })
            break
          case 0x02:
            address += unit.uleb() * minimumInstructionLength
            continue
          case 0x03:
            line += unit.sleb()
            continue
          case 0x04:
            file = unit.uleb()
            continue
          case 0x05:
            column = unit.uleb()
            continue
          case 0x06:
            isStmt = !isStmt
            continue
          default:
            break
        }
      } else {
        const adjusted = opcode - opcodeBase
        const opAdvance = lineRange === 0 ? 0 : Math.floor(adjusted / lineRange)
        const lineAdvance = lineRange === 0 ? lineBase : lineBase + (adjusted % lineRange)
        address += opAdvance * minimumInstructionLength
        line += lineAdvance
        if (file >= 1 && file <= files.length)
          rows.push({ address, source: { file: files[file - 1], line, col: column }, isStmt })
        continue
      }
    }
  }
  rows.sort((a, b) => a.address - b.address)
  return rows
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
  const functions = parseDebugInfo(infoSection, abbrevs)
  const lines = parseDebugLine(lineSection)
  if (functions.length === 0 && lines.length === 0) return
  return { base, lines, functions }
}

function findFunctionRange(functions: FunctionRange[], pc: number): FunctionRange | undefined {
  let lo = 0
  let hi = functions.length - 1
  let candidate: FunctionRange | undefined
  while (lo <= hi) {
    const mid = Math.floor((lo + hi) / 2)
    const f = functions[mid]
    if (pc < f.low) hi = mid - 1
    else {
      candidate = f
      lo = mid + 1
    }
  }
  if (candidate && pc >= candidate.low && pc < candidate.high) return candidate
  return
}

function findLineRow(lines: LineRow[], pc: number): LineRow | undefined {
  let lo = 0
  let hi = lines.length - 1
  let candidate: LineRow | undefined
  while (lo <= hi) {
    const mid = Math.floor((lo + hi) / 2)
    const row = lines[mid]
    if (pc === row.address) return row
    if (pc < row.address) hi = mid - 1
    else {
      candidate = row
      lo = mid + 1
    }
  }
  return
}

function locate(pc: number, m: DebugModule): { fn: FunctionRange, line?: LineRow } | undefined {
  const relative = pc - m.base
  if (relative < 0) return
  const fn = findFunctionRange(m.functions, relative)
  if (!fn) return
  const line = findLineRow(m.lines, relative)
  return { fn, line }
}
