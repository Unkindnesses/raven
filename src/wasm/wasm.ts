import { HashSet } from '../utils/map'
import { LineInfo } from '../dwarf'

export {
  Type, sizeof,
  Signature, LineInfo, Instruction,
  Const, RefNull, nop, GetLocal, SetLocal, GetGlobal, SetGlobal, Op, Drop, Select, Convert, Branch, Call, CallIndirect, Return, unreachable, Block, Loop, instr,
  Func, Table, Mem, Global, Elem, Data, Import, Export, Module,
  signatures, callees
}

enum Type {
  i32 = 'i32',
  i64 = 'i64',
  f32 = 'f32',
  f64 = 'f64',
  externref = 'externref'
}

function sizeof(t: Type): number {
  switch (t) {
    case Type.i32:
    case Type.f32: return 4
    case Type.i64:
    case Type.f64: return 8
    default: throw new Error(`No sizeof for Wasm type ${t}`)
  }
}

type Numeric = Type.i32 | Type.i64 | Type.f32 | Type.f64

interface Signature {
  kind: 'signature'
  params: Type[]
  result: Type[]
}

function Signature(params: Type[], result: Type[]): Signature {
  return { kind: 'signature', params, result }
}

type Instruction =
  | { kind: 'const'; type: Type.f32 | Type.f64; val: number }
  | { kind: 'const'; type: Type.i32 | Type.i64; val: bigint }
  | { kind: 'ref_null'; refType: Type }
  | { kind: 'nop' }
  | { kind: 'get_local'; id: number }
  | { kind: 'set_local'; tee: boolean; id: number }
  | { kind: 'get_global'; id: number }
  | { kind: 'set_global'; id: number }
  | { kind: 'op'; name: string }
  | { kind: 'drop' }
  | { kind: 'select' }
  | { kind: 'convert'; to: Type; from: Type; name: string }
  | { kind: 'branch'; cond: boolean; level: number }
  | { kind: 'call'; name: string }
  | { kind: 'call_indirect'; sig: Signature; table: number }
  | { kind: 'return' }
  | { kind: 'unreachable' }
  | { kind: 'block'; body: Instruction[]; srcs: (LineInfo | null)[] }
  | { kind: 'loop'; body: Instruction[]; srcs: (LineInfo | null)[] }

function Const(type: Numeric, val: number | bigint): Instruction & { kind: 'const' } {
  if (type === Type.i32 || type === Type.i64) {
    return { kind: 'const', type: type, val: BigInt(val) }
  } else {
    return { kind: 'const', type: type, val: Number(val) }
  }
}

function RefNull(refType: Type): Instruction {
  return { kind: 'ref_null', refType }
}

const nop: Instruction = { kind: 'nop' }

function GetLocal(id: number): Instruction {
  return { kind: 'get_local', id }
}

function SetLocal(id: number, tee: boolean = false): Instruction {
  return { kind: 'set_local', tee, id }
}

function GetGlobal(id: number): Instruction {
  return { kind: 'get_global', id }
}

function SetGlobal(id: number): Instruction {
  return { kind: 'set_global', id }
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

function Convert(to: Type, from: Type, name: string): Instruction {
  return { kind: 'convert', to, from, name }
}

function Branch(level: number, cond: boolean = false): Instruction {
  return { kind: 'branch', cond, level }
}

function Call(name: string): Instruction {
  return { kind: 'call', name }
}

function CallIndirect(sig: Signature, table: number = 0): Instruction {
  return { kind: 'call_indirect', sig, table }
}

function Return(): Instruction {
  return { kind: 'return' }
}

const unreachable: Instruction = { kind: 'unreachable' }

type Block = Instruction & { kind: 'block' }

function Block(body: Instruction[] = [], srcs?: (LineInfo | null)[]): Block {
  return {
    kind: 'block', body,
    srcs: srcs || body.map(() => null)
  }
}

type Loop = Instruction & { kind: 'loop' }

function Loop(body: Instruction[] = [], srcs?: (LineInfo | null)[]): Loop {
  return {
    kind: 'loop', body,
    srcs: srcs || body.map(() => null)
  }
}

function instr(b: Block | Loop, it: Instruction, src: LineInfo | null = null) {
  b.body.push(it)
  b.srcs.push(src)
}

interface Func {
  kind: 'func'
  name: string
  sig: Signature
  locals: Type[]
  body: Block
  meta?: any
}

function Func(name: string, sig: Signature, locals: Type[], body: Block, meta?: any): Func {
  return { kind: 'func', name, sig, locals, body, meta }
}

interface Table {
  kind: 'table'
  min: number
  name?: string
}

function Table(min: number, name?: string): Table {
  return { kind: 'table', min, name }
}

interface Mem {
  kind: 'mem'
  min: number
  max?: number
  name?: string
}

function Mem(min: number, max?: number, name?: string): Mem {
  return { kind: 'mem', min, max, name }
}

interface Global {
  kind: 'global'
  type: Type
  mut: boolean
  init: Instruction
  name?: string
}

function Global(type: Type, options?: { mut?: boolean; init?: Instruction; name?: string }): Global {
  const { mut = true, init, name } = options || {}
  const finalInit = init || (type === Type.externref ? RefNull(type) : Const(type, 0))
  return { kind: 'global', type, mut, init: finalInit, name }
}

interface Elem {
  table: number
  data: string[]
}

function Elem(table: number, data: string[]): Elem {
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
  as: string
  sig: Signature | Global | Mem | Table
}

function Import(mod: string, name: string, as: string, sig: Signature | Global | Mem | Table): Import {
  return { kind: 'import', mod, name, as, sig }
}

interface Export {
  as: string
  name: string
}

function Export(name: string, as: string = name): Export {
  return { name, as }
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
    exports: options.exports || []
  }
}

function signatures(m: Module): Signature[] {
  const sigs: Signature[] = []
  const seen = new HashSet<Signature>()
  const allItems = [...m.imports, ...m.funcs]
  for (const item of allItems) {
    if (item.sig.kind !== 'signature' || seen.has(item.sig)) continue
    seen.add(item.sig)
    sigs.push(item.sig)
  }
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
