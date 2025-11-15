import * as wasm from './wasm'
import { HashMap, some } from '../utils/map'
import { opcodes } from './opcodes'
import * as dwarf from '../dwarf'

export { binary }

const { i32, i64, f32, f64 } = wasm.NumType

interface Debug {
  size: number
  lines: dwarf.LineTable
  functions: dwarf.Function[]
}

type NameKind = (wasm.Func | wasm.Global | wasm.Table | wasm.Mem)['kind']

class BinaryContext {
  buffer: number[]
  types: HashMap<wasm.Signature, number>
  names: Map<string, [NameKind, number]>

  constructor(
    buffer: number[],
    types: HashMap<wasm.Signature, number>,
    names: Map<string, [NameKind, number]>,
  ) {
    this.buffer = buffer
    this.types = types
    this.names = names
  }

  static from(buffer: number[], cx: BinaryContext): BinaryContext {
    return new BinaryContext(buffer, cx.types, cx.names)
  }

  static fromModule(buffer: number[], m: wasm.Module): BinaryContext {
    const types = new HashMap<wasm.Signature, number>()
    const names = new Map<string, [NameKind, number]>()
    const add = (name: string, kind: NameKind, id: number) => {
      if (names.has(name)) throw new Error(`Duplicate name: ${name}`)
      names.set(name, [kind, id])
    }
    const sigs = wasm.signatures(m) // assign ids to type signatures
    for (let i = 0; i < sigs.length; i++) types.set(sigs[i], i)
    let id = 0
    for (const f of [...m.imports, ...m.funcs]) {
      if (f.sig.kind !== 'signature') continue
      const name = f.kind === 'import' ? f.sig.name : f.name
      add(name, 'func', id++)
    }
    id = 0
    for (const imp of m.imports.filter(x => x.sig.kind === 'global'))
      add((imp.sig as wasm.Global).name, 'global', id++)
    for (const gl of m.globals) add(gl.name, 'global', id++)
    id = 0
    for (const imp of m.imports.filter(x => x.sig.kind === 'table'))
      add((imp.sig as wasm.Table).name, 'table', id++)
    for (const table of m.tables) add(table.name, 'table', id++)
    id = 0
    for (const imp of m.imports.filter(x => x.sig.kind === 'mem'))
      add((imp.sig as wasm.Mem).name, 'mem', id++)
    for (const mem of m.mems) add(mem.name, 'mem', id++)
    return new BinaryContext(buffer, types, names)
  }

  write(x: number | string | Uint8Array | number[]): void {
    if (typeof x === 'number') this.buffer.push(x)
    else if (typeof x === 'string') this.buffer.push(...new TextEncoder().encode(x))
    else if (x instanceof Uint8Array) this.buffer.push(...x)
    else if (Array.isArray(x)) this.buffer.push(...x)
  }

  leb128(x: number | bigint): void { dwarf.leb128U(x, this.buffer) }
  leb128S(x: number | bigint): void { dwarf.leb128S(x, this.buffer) }

  get position(): number {
    return this.buffer.length
  }
}

function name(cx: BinaryContext, x: string): void {
  const bytes = new TextEncoder().encode(x)
  cx.leb128(bytes.length)
  cx.write(bytes)
}

// Instruction encoding

const numtypes = new Map<wasm.NumType, number>([
  [i32, 0x7f],
  [i64, 0x7e],
  [f32, 0x7d],
  [f64, 0x7c]
])

const absheaptypes = new Map<string, number>([
  ['exn', 0x69],
  ['array', 0x6a],
  ['struct', 0x6b],
  ['i31', 0x6c],
  ['eq', 0x6d],
  ['any', 0x6e],
  ['extern', 0x6f],
  ['func', 0x70]
])

function heaptype(cx: BinaryContext, ht: wasm.HeapType): void {
  if (ht.kind !== 'abstract') throw new Error(`Unsupported heap type: ${ht.kind}`)
  cx.write(some(absheaptypes.get(ht.type)))
}

function valuetype(cx: BinaryContext, vt: wasm.ValueType): void {
  if (typeof vt === 'string') {
    cx.write(some(numtypes.get(vt)))
  } else {
    if (vt.null && vt.type.kind === 'abstract') {
      heaptype(cx, vt.type)
    } else {
      cx.write(vt.null ? 0x63 : 0x64)
      heaptype(cx, vt.type)
    }
  }
}

function instr(cx: BinaryContext, inst: wasm.Instruction, lt: dwarf.LineTable): void {
  switch (inst.kind) {
    case 'unreachable':
      cx.write(0x00)
      break
    case 'block':
      cx.write(0x02)
      cx.write(0x40) // empty type
      for (let i = 0; i < inst.body.length; i++) {
        const src = inst.srcs[i]
        if (src) lt.lines.push([cx.position, src])
        instr(cx, inst.body[i], lt)
      }
      cx.write(0x0b) // end
      break
    case 'loop':
      cx.write(0x03)
      cx.write(0x40) // empty type
      for (let i = 0; i < inst.body.length; i++) {
        const src = inst.srcs[i]
        if (src) lt.lines.push([cx.position, src])
        instr(cx, inst.body[i], lt)
      }
      cx.write(0x0b) // end
      break
    case 'branch':
      cx.write(0x0c + (inst.cond ? 1 : 0))
      cx.leb128(inst.level)
      break
    case 'return':
      cx.write(0x0f)
      break
    case 'call':
      cx.write(0x10)
      cx.leb128(some(cx.names.get(inst.name))[1])
      break
    case 'call_indirect':
      cx.write(0x11)
      cx.leb128(some(cx.types.get(inst.sig)))
      cx.leb128(inst.table)
      break
    case 'drop':
      cx.write(0x1a)
      break
    case 'get_local':
      cx.write(0x20)
      cx.leb128(inst.id)
      break
    case 'set_local':
      cx.write(0x21 + (inst.tee ? 1 : 0))
      cx.leb128(inst.id)
      break
    case 'get_global':
      cx.write(0x23)
      cx.leb128(some(cx.names.get(inst.id))[1])
      break
    case 'set_global':
      cx.write(0x24)
      cx.leb128(some(cx.names.get(inst.id))[1])
      break
    case 'const':
      const typeOffset = [i32, i64, f32, f64].indexOf(inst.type)
      cx.write(0x41 + typeOffset)
      if (inst.type === i32 || inst.type === i64) {
        cx.leb128S(inst.val)
      } else {
        const bytes = new ArrayBuffer(inst.type === f32 ? 4 : 8)
        const view = new DataView(bytes)
        if (inst.type === f32) view.setFloat32(0, inst.val, true)
        else if (inst.type === f64) view.setFloat64(0, inst.val, true)
        cx.write(new Uint8Array(bytes))
      }
      break
    case 'ref_null':
      cx.write(0xd0)
      heaptype(cx, inst.type)
      break
    case 'op':
      cx.write(some(opcodes.get(inst.name)))
      break
    default:
      throw new Error(`Unsupported instruction: ${inst.kind}`)
  }
}

function expr(cx: BinaryContext, x: wasm.Instruction, lt: dwarf.LineTable): void {
  instr(cx, x, lt)
  cx.write(0x0b)
}

// Module sections

function header(cx: BinaryContext): void {
  cx.write('\0asm') // magic
  cx.write([1, 0, 0, 0]) // version number
}

function withsize(cx: BinaryContext, f: (cx: BinaryContext) => void): number {
  const buf = BinaryContext.from([], cx)
  f(buf)
  const size = buf.position
  cx.leb128(size)
  cx.write(buf.buffer)
  return size
}

function typevec(cx: BinaryContext, ts: wasm.ValueType[]): void {
  cx.leb128(ts.length)
  for (const t of ts) valuetype(cx, t)
}

function functype(cx: BinaryContext, s: wasm.Signature): void {
  cx.write(0x60)
  typevec(cx, s.params)
  typevec(cx, s.result)
}

function custom(cx: BinaryContext, nm: string, f: (cx: BinaryContext) => void): void {
  cx.write(0x00) // section id
  withsize(cx, cx => {
    name(cx, nm)
    f(cx)
  })
}

function customSections(cx: BinaryContext, sections: wasm.CustomSection[]): void {
  for (const section of sections) {
    custom(cx, section.name, buf => buf.write(section.data))
  }
}

function types(cx: BinaryContext, m: wasm.Module): void {
  const sigs = wasm.signatures(m)
  if (sigs.length === 0) return
  cx.write(0x01) // section id
  withsize(cx, cx => {
    cx.leb128(sigs.length)
    for (const s of sigs) functype(cx, s)
  })
}

function imports(cx: BinaryContext, imps: wasm.Import[]): void {
  if (imps.length === 0) return
  cx.write(0x02) // section id
  withsize(cx, cx => {
    cx.leb128(imps.length)
    for (const i of imps) {
      name(cx, i.mod)
      name(cx, i.name)
      if (i.sig.kind === 'signature') {
        cx.write(0x00)
        cx.leb128(some(cx.types.get(i.sig)))
      } else if (i.sig.kind === 'table') {
        cx.write(0x01)
        table(cx, i.sig as wasm.Table)
      } else if (i.sig.kind === 'mem') {
        cx.write(0x02)
        mem(cx, i.sig as wasm.Mem)
      } else if (i.sig.kind === 'global') {
        cx.write(0x03)
        globaltype(cx, i.sig as wasm.Global)
      } else {
        throw new Error(`Unsupported sig ${i.sig}`)
      }
    }
  })
}

function functions(cx: BinaryContext, funcs: wasm.Func[]): void {
  if (funcs.length === 0) return
  cx.write(0x03) // section id
  withsize(cx, cx => {
    cx.leb128(funcs.length)
    for (const f of funcs) cx.leb128(some(cx.types.get(f.sig)))
  })
}

function table(cx: BinaryContext, t: wasm.Table): void {
  cx.write(0x70) // funcref
  cx.write(0x00)
  cx.leb128(t.min)
}

function tables(cx: BinaryContext, ts: wasm.Table[]): void {
  if (ts.length === 0) return
  cx.write(0x04) // section id
  withsize(cx, cx => {
    cx.leb128(ts.length)
    for (const t of ts) table(cx, t)
  })
}

function mem(cx: BinaryContext, m: wasm.Mem): void {
  if (m.max === undefined) {
    cx.write(0x00)
    cx.leb128(m.min)
  } else {
    cx.write(0x01)
    cx.leb128(m.min)
    cx.leb128(m.max)
  }
}

function memories(cx: BinaryContext, mems: wasm.Mem[]): void {
  if (mems.length === 0) return
  const m = mems[0]
  cx.write(0x05) // section id
  withsize(cx, cx => {
    cx.leb128(1)
    mem(cx, m)
  })
}

function globaltype(cx: BinaryContext, g: wasm.Global): void {
  valuetype(cx, g.type)
  cx.write(g.mut ? 1 : 0)
}

function globals(cx: BinaryContext, gs: wasm.Global[]): void {
  if (gs.length === 0) return
  cx.write(0x06) // section id
  withsize(cx, cx => {
    cx.leb128(gs.length)
    for (const g of gs) {
      globaltype(cx, g)
      instr(cx, g.init, { lines: [] })
      cx.write(0x0b) // end
    }
  })
}

function exportcode(kind: NameKind): number {
  switch (kind) {
    case 'func': return 0x00
    case 'table': return 0x01
    case 'mem': return 0x02
    case 'global': return 0x03
  }
}

function wexports(cx: BinaryContext, m: wasm.Module): void {
  if (m.exports.length === 0) return
  cx.write(0x07)
  withsize(cx, cx => {
    cx.leb128(m.exports.length)
    for (const ex of m.exports) {
      const [kind, id] = some(cx.names.get(ex.name))
      name(cx, ex.as)
      cx.write(exportcode(kind))
      cx.leb128(id)
    }
  })
}

function elems(cx: BinaryContext, es: wasm.Elem[]): void {
  if (es.length === 0) return
  cx.write(0x09) // section id
  withsize(cx, cx => {
    cx.leb128(es.length)
    for (const e of es) {
      if (e.table !== 0) throw new Error('Only table 0 supported')
      cx.leb128(0)
      expr(cx, wasm.Const(i32, 0), { lines: [] })
      cx.leb128(e.data.length)
      for (const f of e.data) {
        // TODO hacky, but handles invalidated code appearing within tables.
        cx.leb128(cx.names.get(f)?.[1] || 0)
      }
    }
  })
}

function func(cx: BinaryContext, f: wasm.Func): dwarf.LineTable {
  const lt = new dwarf.LineTable([])
  lt.lines.push([cx.position, dwarf.LineInfo([[f.meta, f.meta.source]])])
  cx.leb128(f.locals.length)
  for (const t of f.locals) {
    cx.leb128(1)
    valuetype(cx, t)
  }
  for (let i = 0; i < f.body.body.length; i++) {
    const src = f.body.srcs[i]
    if (src) lt.lines.push([cx.position, src])
    instr(cx, f.body.body[i], lt)
  }
  cx.write(0x0b)
  return lt
}

function code(cx: BinaryContext, funcs: wasm.Func[]): Debug {
  const table = new dwarf.LineTable([])
  const functions: dwarf.Function[] = []
  if (funcs.length === 0) return { size: 0, lines: table, functions }
  cx.write(0x0a) // section id
  const size = withsize(cx, cx => {
    cx.leb128(funcs.length)
    for (const f of funcs) {
      let lt: dwarf.LineTable
      const bodySize = withsize(cx, cx => { lt = func(cx, f) })
      const range: [number, number] = [cx.position - bodySize, cx.position]
      table.lines.push(...dwarf.offset(lt!, -range[0]).lines)
      const inlines = dwarf.buildInlineTree(lt!.lines, bodySize, range[0], f.meta)
      functions.push({ def: f.meta, range, inlines })
    }
  })
  return { size, lines: table, functions }
}

function names(cx: BinaryContext, m: wasm.Module): void {
  const fs = [...m.imports, ...m.funcs].filter(f => f.sig.kind === 'signature')
  const gs = [...m.imports.filter(x => x.sig.kind === 'global').map(x => (x.sig as wasm.Global).name), ...m.globals.map(x => x.name)]
  custom(cx, 'name', cx => {
    cx.write(0x01) // func map
    withsize(cx, cx => {
      cx.leb128(fs.length)
      for (let i = 0; i < fs.length; i++) {
        cx.leb128(i)
        name(cx, fs[i].kind === 'import' ? (fs[i] as wasm.Import).sig.name : fs[i].name)
      }
    })
    if (gs.length === 0) return
    cx.write(0x07) // global map
    withsize(cx, cx => {
      cx.leb128(gs.length)
      for (let i = 0; i < gs.length; i++) {
        cx.leb128(i)
        name(cx, gs[i])
      }
    })
  })
}

function emitDwarf(cx: BinaryContext, info: Debug): void {
  const files = dwarf.lineFiles(info.lines)
  const dies = info.functions.map(fn => dwarf.functionDie(fn, files))
  const root = new dwarf.DIE(dwarf.Tag.compile_unit,
    [
      [dwarf.Attr.producer, [dwarf.Form.string, 'raven version 0.0.0']],
      [dwarf.Attr.language, [dwarf.Form.data2, dwarf.Lang.LANG_C99]],
      [dwarf.Attr.stmt_list, [dwarf.Form.sec_offset, 0]],
      [dwarf.Attr.low_pc, [dwarf.Form.addr, 0]],
      [dwarf.Attr.high_pc, [dwarf.Form.addr, info.size]]
    ],
    dies
  )
  const as = dwarf.abbrevs(root)
  custom(cx, '.debug_info', buf => dwarf.debug_info(buf.buffer, root, as))
  custom(cx, '.debug_abbrev', buf => dwarf.debug_abbrev(buf.buffer, as))
  if (info.lines.lines.length === 0) return
  custom(cx, '.debug_line', buf => dwarf.debug_line(buf.buffer, info.lines))
}

function binary(m: wasm.Module, strip = false): Uint8Array {
  const buf: number[] = []
  const cx = BinaryContext.fromModule(buf, m)
  header(cx)
  types(cx, m)
  imports(cx, m.imports)
  functions(cx, m.funcs)
  tables(cx, m.tables)
  memories(cx, m.mems)
  globals(cx, m.globals)
  wexports(cx, m)
  elems(cx, m.elems)
  customSections(cx, m.customs)
  const dbg = code(cx, m.funcs)
  names(cx, m)
  if (!strip) emitDwarf(cx, dbg)
  return new Uint8Array(buf)
}
