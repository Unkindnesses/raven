import { hash, HashMap, some } from "../utils/map"
import { Anno, unreachable } from "../utils/ir"
import { Type, Tag, tag, repr } from "./types"
import * as types from "./types"
import { NumType, ValueType } from "../wasm/wasm"
import * as cache from "../utils/cache"
import * as ir from "../utils/ir"
import isEqual from 'lodash/isEqual'
import { Pattern } from "./patterns"
import { Def } from "../dwarf"

export {
  Module, Method, Signature, Binding, asBinding, asConst, Modules,
  Definitions, Const, MIR, IRValue, IRType, WIntrinsic, WImport, showIRValue,
  StringRef, xstring, Global, SetGlobal, xglobal, xset, Invoke, Wasm, xwasm, callargs
}

class WIntrinsic {
  constructor(readonly name: string) { }
}

class WImport {
  constructor(readonly mod: string, readonly name: string) { }
}

class Binding {
  constructor(readonly mod: Tag, readonly name: string) { }
  get [hash]() { return `${this.mod.path}.${this.name}` }
}

function asBinding(x: unknown): Binding {
  if (x instanceof Binding) return x
  throw new Error(`Expected Binding, got ${typeof x}`)
}

class Const {
  constructor(
    readonly type: 'i32' | 'i64' | 'f32' | 'f64',
    readonly value: number | bigint) { }
  static i32(v: number | bigint) { return new Const('i32', v) }
  static i64(v: number | bigint) { return new Const('i64', v) }
  static f32(v: number | bigint) { return new Const('f32', v) }
  static f64(v: number | bigint) { return new Const('f64', v) }
  static from(t: NumType, v: number | bigint): Const {
    if (t === NumType.i32) return Const.i32(v)
    if (t === NumType.i64) return Const.i64(v)
    if (t === NumType.f32) return Const.f32(v)
    if (t === NumType.f64) return Const.f64(v)
    throw new Error(`Unsupported Wasm const type ${t}`)
  }
}

function asConst(x: unknown): Const {
  if (x instanceof Const) return x
  throw new Error(`Expected Const, got ${typeof x}`)
}

type IRValue = Type | Const
type IRType = Type | ValueType[]
type MIR = ir.IR<IRValue, IRType>

function irTypeOf(x: IRValue): IRType {
  if (x instanceof Const) {
    if (x.type === 'i32') return types.int32()
    if (x.type === 'i64') return types.int64()
    if (x.type === 'f32') return types.float32()
    if (x.type === 'f64') return types.float64()
    throw new Error('unreachable')
  }
  return x
}

function showIRValue(x: IRValue | IRType): string {
  if (x instanceof Const) return `${x.type}(${x.value.toString()})`
  if (Array.isArray(x)) return `[${x.join(', ')}]`
  return repr(x)
}

function MIR(meta: Def): MIR {
  return new ir.IR<IRValue, IRType>(meta, irTypeOf, showIRValue)
}

class StringRef<T> extends ir.Expr<T> {
  constructor(readonly value: string) { super('string') }
  map(_: (x: T | number) => T | number): StringRef<T> { return new StringRef(this.value) }
  show(_: (x: T) => string): string { return JSON.stringify(this.value) }
}

function xstring<T>(s: string): StringRef<T> { return new StringRef<T>(s) }

class Global<T> extends ir.Expr<T> {
  constructor(readonly binding: Binding) { super('global') }
  map(_: (x: T | number) => T | number): Global<T> { return new Global(this.binding) }
  show(_: (x: T) => string): string { return `global ${this.binding.mod}.${this.binding.name}` }
}

function xglobal<T>(b: Binding): Global<T> { return new Global<T>(b) }

class SetGlobal<T> extends ir.Expr<T> {
  constructor(readonly binding: Binding, readonly value: T | number) { super('setglobal') }
  get body() { return [this.value] }
  map(f: (x: T | number) => T | number): SetGlobal<T> { return new SetGlobal(this.binding, f(this.value)) }
  show(f: (x: T | number) => string): string { return `set ${this.binding.mod}.${this.binding.name}, ${f(this.value)}` }
}

function xset<T>(b: Binding, v: T): SetGlobal<T> { return new SetGlobal<T>(b, v) }

class Invoke<T> extends ir.Expr<T> {
  constructor(readonly method: Method, readonly args: (T | number)[]) { super('invoke', args) }
  map(f: (x: T | number) => T | number): Invoke<T> { return new Invoke(this.method, this.args.map(f)) }
  show(pr: (x: T | number) => string): string {
    return this.args.length > 0
      ? `call ${this.method.toString()}, ${this.args.map(pr).join(', ')}`
      : `call ${this.method.toString()}`
  }
}

class Wasm<T> extends ir.Expr<T> {
  constructor(readonly callee: WIntrinsic | WImport, readonly args: (T | number)[]) { super('wasm', args) }
  map(f: (x: T | number) => T | number): Wasm<T> { return new Wasm(this.callee, this.args.map(f)) }
  show(pr: (x: T | number) => string): string {
    const target = this.callee instanceof WIntrinsic
      ? this.callee.name
      : `\$${this.callee.mod}.${this.callee.name}`
    return this.args.length > 0
      ? `call ${target}, ${this.args.map(pr).join(', ')}`
      : `call ${target}`
  }
}

function xwasm<T>(callee: WIntrinsic | WImport, ...args: (T | number)[]) {
  return new Wasm<T>(callee, args)
}

function callargs(ir: ir.Fragment<MIR>, ex: ir.Expr<IRValue>): [Tag | Method, ir.Val<MIR>[]] {
  return ex instanceof Invoke ?
    [ex.method, ex.body] :
    [types.asTag(ir.type(ex.body[0])), ex.body.slice(1)]
}

interface Signature {
  pattern: Pattern
  args: string[]
  swap: Map<number, string>
}

class Method {
  constructor(
    readonly mod: Tag,
    readonly name: Tag,
    readonly sig: Signature,
    readonly func?: (...args: Type[]) => Anno<Type>,
    readonly params: Type[] = [],
    readonly id = cache.nft()
  ) { }
  get [hash]() { return `${this.id}${this.params.map(x => types.repr(x)).join()}` }
  toString() { return `Method(${this.name})` }
  param(...Ts: Type[]) {
    return new Method(this.mod, this.name, this.sig, this.func, Ts, this.id)
  }
}

class Methods implements cache.Caching {
  private imports = new cache.Ref<Tag[]>([])
  private methods = new cache.Map<Tag, (Method | Tag)[]>()
  private code = new cache.Map<Method, MIR>()

  get subcaches() { return [this.imports, this.methods, this.code] }
  get(k: Tag) { return this.methods.get(k) ?? this.imports.get() }
  ir(m: Method) { return some(this.code.get(m)) }

  method(m: Method, ir?: MIR) {
    const ms = this.methods.get(m.name) ?? this.imports.get()
    this.methods.set(m.name, [...ms, m])
    if (ir) this.code.set(m, ir)
    return m
  }

  import(mod: Tag) {
    if (this.imports.get().some(m => isEqual(m, mod))) return
    this.imports.set([...this.imports.get(), mod])
    for (const k of this.methods.keys())
      this.methods.set(k, [...this.methods.get(k)!, mod])
  }

  clear() {
    this.imports.set([])
    this.methods.clear()
    this.code.clear()
  }

  delete(k: Tag) {
    for (const m of this.get(k))
      if (m instanceof Method) this.code.delete(m)
    return this.methods.delete(k)
  }
}

class Module implements cache.Caching {
  readonly defs = new cache.Map<string, Anno<Type> | Binding>()
  readonly exports = new Set<string>()
  readonly methods = new Methods()
  constructor(readonly name: Tag) { }

  get subcaches() { return [this.defs, this.methods] }
  method(name: Tag, sig: Signature, ir: MIR) { return this.methods.method(new Method(this.name, name, sig), ir) }
  ir(m: Method) { return this.methods.ir(m) }
  get(k: string) { return this.defs.get(k) }
  set(k: string, v: Anno<Type> | Binding) { this.defs.set(k, v) }
  has(k: string) { return this.defs.has(k) }
  delete(k: string) { return this.defs.delete(k) }

  clear() {
    this.defs.clear()
    this.exports.clear()
    this.methods.clear()
  }

  import(from: Module, vars: string[] = []) {
    this.methods.import(from.name)
    for (const v of vars) {
      if (!from.exports.has(v)) throw new Error(`Module ${from.name} does not export ${v} `)
      this.set(v, new Binding(from.name, v))
    }
  }
}

class Modules implements cache.Caching {
  private mods = new HashMap<Tag, Module>()
  get subcaches() { return this.mods.values() }
  module(m: Tag | Module) {
    if (m instanceof Module) {
      this.mods.set(m.name, m)
      return m
    } else {
      const mod = this.mods.get(m) ?? new Module(m)
      this.mods.set(m, mod)
      return mod
    }
  }
  get(b: Binding) { return this.mods.get(b.mod)?.get(b.name) }
  set(b: Binding, v: Anno<Type> | Binding) { this.module(b.mod).set(b.name, v) }
  ir(m: Method) { return some(this.mods.get(m.mod)).ir(m) }
  resolve_static(b: Binding): Anno<Type> {
    const val = this.get(b)
    if (val === undefined) throw new Error(`Binding ${b.name} not found in module ${b.mod} `)
    return val instanceof Binding ? this.resolve_static(val) : val
  }
}

function methods(cx: Modules, name: Tag, mod: Tag = tag(""), ms: Method[] = [], seen = new Set<Tag>()) {
  for (const m of cx.module(mod).methods.get(name)) {
    if (m instanceof Tag) {
      if (seen.has(m)) continue
      seen.add(m)
      methods(cx, name, m, ms, seen)
    } else {
      ms.push(m)
    }
  }
  return ms
}

class Definitions implements cache.Caching {
  private comp: Modules
  globals: cache.Cache<Binding, Anno<Type> | Binding>
  table: cache.EagerCache<Tag, Method[]>
  constructor(comp: Modules) {
    this.comp = comp
    this.globals = new cache.Cache<Binding, Anno<Type> | Binding>(b => comp.module(b.mod).get(b.name) ?? unreachable)
    this.table = new cache.EagerCache<Tag, Method[]>(name => methods(comp, name))
  }
  get subcaches() { return [this.globals, this.table] }
  resolve_static(b: Binding): Anno<Type> {
    return this.comp.resolve_static(b)
  }
  global(b: Binding) { return this.globals.get(b) }
  methods(n: Tag) { return this.table.get(n) }
  ir(m: Method) { return this.comp.ir(m) }
}
