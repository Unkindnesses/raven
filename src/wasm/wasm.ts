import { HashSet } from '../utils/map.js'
import { Def, LineInfo } from '../dwarf/index.js'

export {
  NumType, RefType, ValueType, HeapType, AbsHeapType, sizeof, f64, f32, i64, i32, externref, funcref, asNumType,
  Signature, LineInfo, Instruction,
  Const, RefNull, nop, GetLocal, SetLocal, GetGlobal, SetGlobal, TableOp, Op, Drop, Select, Convert, Branch, Call, CallIndirect, Return, unreachable, Block, Loop, instr,
  Func, Table, Mem, Global, Elem, Data, Import, Export, CustomSection, Module,
  signatures, callees, zero
}

enum NumType {
  f64 = 'f64',
  f32 = 'f32',
  i64 = 'i64',
  i32 = 'i32',
}

enum AbsHeapType {
  exn = 'exn',
  array = 'array',
  struct = 'struct',
  i31 = 'i31',
  eq = 'eq',
  any = 'any',
  extern = 'extern',
  func = 'func'
}

type HeapType =
  | { kind: 'abstract'; type: AbsHeapType }
  | { kind: 'struct', fields: [boolean, ValueType][] }
  | { kind: 'array', field: [boolean, ValueType] }

type RefType = { null: boolean, type: HeapType }

type ValueType = NumType | RefType

const f64 = NumType.f64
const f32 = NumType.f32
const i64 = NumType.i64
const i32 = NumType.i32
const externref: RefType = { null: true, type: { kind: 'abstract', type: AbsHeapType.extern } }
const funcref: RefType = { null: true, type: { kind: 'abstract', type: AbsHeapType.func } }

function asNumType(t: any): NumType {
  if ([f64, f32, i64, i32].includes(t)) return t
  throw new Error(`Expected NumType, got ${typeof t}`)
}

function sizeof(t: ValueType): number {
  switch (t) {
    case NumType.i32:
    case NumType.f32: return 4
    case NumType.i64:
    case NumType.f64: return 8
    default: throw new Error('Reference types have no size')
  }
}

interface Signature {
  kind: 'signature'
  name?: string
  params: ValueType[]
  result: ValueType[]
}

type NamedSignature = Signature & { name: string }

function Signature(params: ValueType[], result: ValueType[]): Signature
function Signature(params: ValueType[], result: ValueType[], name: string): NamedSignature
function Signature(params: ValueType[], result: ValueType[], name?: string): Signature {
  return { kind: 'signature', name, params, result }
}

type Instruction =
  | { kind: 'const'; type: NumType.f32 | NumType.f64; val: number }
  | { kind: 'const'; type: NumType.i32 | NumType.i64; val: bigint }
  | { kind: 'ref_null'; type: HeapType }
  | { kind: 'nop' }
  | { kind: 'get_local'; id: number }
  | { kind: 'set_local'; tee: boolean; id: number }
  | { kind: 'get_global'; id: string }
  | { kind: 'set_global'; id: string }
  | { kind: 'op'; name: string }
  | { kind: 'drop' }
  | { kind: 'select' }
  | { kind: 'convert'; to: NumType; from: NumType; name: string }
  | { kind: 'branch'; cond: boolean; level: number }
  | { kind: 'call'; name: string }
  | { kind: 'call_indirect'; sig: Signature; table: string }
  | { kind: 'table'; op: string; table: string }
  | { kind: 'return' }
  | { kind: 'unreachable' }
  | { kind: 'block'; body: Instruction[]; srcs: LineInfo[] }
  | { kind: 'loop'; body: Instruction[]; srcs: LineInfo[] }

function Const(type: NumType, val: number | bigint): Instruction & { kind: 'const' } {
  if (type === NumType.i32 || type === NumType.i64) {
    return { kind: 'const', type: type, val: BigInt(val) }
  } else {
    return { kind: 'const', type: type, val: Number(val) }
  }
}

function RefNull(type: HeapType): Instruction {
  return { kind: 'ref_null', type }
}

const nop: Instruction = { kind: 'nop' }

function GetLocal(id: number): Instruction {
  return { kind: 'get_local', id }
}

function SetLocal(id: number, tee: boolean = false): Instruction {
  return { kind: 'set_local', tee, id }
}

function GetGlobal(id: string): Instruction {
  return { kind: 'get_global', id }
}

function SetGlobal(id: string): Instruction {
  return { kind: 'set_global', id }
}

function TableOp(op: string, table: string): Instruction {
  return { kind: 'table', op, table }
}

function Op(name: string): Instruction {
  return { kind: 'op', name }
}

function Drop(): Instruction {
  return { kind: 'drop' }
}

function Select(): Instruction {
  return { kind: 'select' }
}

function Convert(to: NumType, from: NumType, name: string): Instruction {
  return { kind: 'convert', to, from, name }
}

function Branch(level: number, cond: boolean = false): Instruction {
  return { kind: 'branch', cond, level }
}

function Call(name: string): Instruction {
  return { kind: 'call', name }
}

function CallIndirect(sig: Signature, table: string): Instruction {
  return { kind: 'call_indirect', sig, table }
}

function Return(): Instruction {
  return { kind: 'return' }
}

const unreachable: Instruction = { kind: 'unreachable' }

type Block = Instruction & { kind: 'block' }

function Block(body: Instruction[] = [], srcs: LineInfo[] = []): Block {
  if (body.length !== srcs.length) throw new Error('Block body and srcs length mismatch')
  return { kind: 'block', body, srcs }
}

type Loop = Instruction & { kind: 'loop' }

function Loop(body: Instruction[] = [], srcs: LineInfo[] = []): Loop {
  if (body.length !== srcs.length) throw new Error('Loop body and srcs length mismatch')
  return { kind: 'loop', body, srcs }
}

function instr(b: Block | Loop, it: Instruction, src: LineInfo) {
  b.body.push(it)
  b.srcs.push(src)
}

interface Func {
  kind: 'func'
  name: string
  sig: Signature
  locals: ValueType[]
  body: Block
  meta: Def
}

function Func(name: string, sig: Signature, locals: ValueType[], body: Block, meta: Def): Func {
  return { kind: 'func', name, sig, locals, body, meta }
}

interface Table {
  kind: 'table'
  name: string
  type: RefType
  min: number
  max?: number
}

function Table(name: string, type: RefType, min: number, options: { max?: number } = {}): Table {
  const { max } = options
  return { kind: 'table', name, type, min, max }
}

interface Mem {
  kind: 'mem'
  name: string
  min: number
  max?: number
}

function Mem(name: string, min: number, max?: number): Mem {
  return { kind: 'mem', name, min, max }
}

interface Global {
  kind: 'global'
  name: string
  type: ValueType
  mut: boolean
  init: Instruction
}

function zero(t: ValueType): Instruction {
  return typeof t === 'string' ? Const(t, 0) : RefNull(t.type)
}

function Global(name: string, type: ValueType, options: { mut?: boolean; init?: Instruction } = {}): Global {
  let { mut = true, init = zero(type) } = options
  return { kind: 'global', name, type, mut, init }
}

interface Elem {
  table: string
  data: string[]
}

function Elem(table: string, data: string[]): Elem {
  return { table, data }
}

interface Data {
  memidx: number
  offset: number
  data: Uint8Array
}

function Data(memidx: number, offset: number, data: Uint8Array): Data {
  return { memidx, offset, data }
}

interface Import {
  kind: 'import'
  mod: string
  name: string
  sig: NamedSignature | Global | Mem | Table
}

function Import(mod: string, name: string, sig: NamedSignature | Global | Mem | Table): Import {
  return { kind: 'import', mod, name, sig }
}

interface Export {
  as: string
  name: string
}

function Export(name: string, as: string = name): Export {
  return { name, as }
}

interface CustomSection {
  name: string
  data: Uint8Array
}

function CustomSection(name: string, data: Uint8Array): CustomSection {
  return { name, data }
}

interface Module {
  funcs: Func[]
  mems: Mem[]
  tables: Table[]
  globals: Global[]
  elems: Elem[]
  data: Data[]
  imports: Import[]
  exports: Export[]
  customs: CustomSection[]
}

function Module(options: Partial<Module> = {}): Module {
  return {
    funcs: options.funcs || [],
    mems: options.mems || [],
    tables: options.tables || [],
    globals: options.globals || [],
    elems: options.elems || [],
    data: options.data || [],
    imports: options.imports || [],
    exports: options.exports || [],
    customs: options.customs || []
  }
}

function signatures(m: Module): Signature[] {
  const sigs: Signature[] = []
  const seen = new HashSet<Signature>()
  const push = (sig: Signature) => {
    if (seen.has(sig)) return
    seen.add(sig)
    sigs.push(sig)
  }
  for (const item of [...m.imports, ...m.funcs])
    if (item.sig.kind === 'signature') push(item.sig)
  const visit = (instr: Instruction) => {
    switch (instr.kind) {
      case 'call_indirect':
        push(instr.sig)
        break
      case 'block':
      case 'loop':
        instr.body.forEach(visit)
        break
    }
  }
  for (const fn of m.funcs) visit(fn.body)
  return sigs
}

// Some AST utils

function callees(x: Func): string[] {
  const cs: string[] = []
  function visit(node: Instruction) {
    switch (node.kind) {
      case 'call':
        cs.push(node.name)
        break
      case 'block':
      case 'loop':
        for (const instr of node.body) visit(instr)
        break
    }
  }
  visit(x.body)
  return cs
}
