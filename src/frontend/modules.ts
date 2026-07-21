import { hash, HashMap, some } from "../utils/map.js"
import { Anno, unreachable } from "../utils/ir.js"
import { Type, Tag, tag, repr } from "./types.js"
import * as types from "./types.js"
import * as cache from "../utils/cache.js"
import * as ir from "../utils/ir.js"
import * as ast from "./ast.js"
import { Pattern } from "./patterns.js"
import { Def } from "../dwarf/index.js"
import { Instruction, Op, ValueType } from "../wasm/wasm.js"

export {
  Module, Method, Signature, MethodSource, Binding, asBinding, asValue, Modules,
  Definitions, Value, MIR, IRValue, showIRValue,
  StringRef, xstring, JS, xjs, Global, SetGlobal, xglobal, xset, Invoke, Func, xfunc, Wasm, xwasm, calltarget, callargs
}

class Binding {
  constructor(readonly mod: Tag, readonly name: string) { }
  get [hash]() { return `${this.mod.path}.${this.name}` }
  toString() { return `${this.mod.path}.${this.name}` }
}

function asBinding(x: unknown): Binding {
  if (x instanceof Binding) return x
  throw new Error(`Expected Binding, got ${typeof x}`)
}

type Const =
  | Type & { kind: 'bits'; value: bigint }
  | Type & { kind: 'float32'; value: number }
  | Type & { kind: 'float64'; value: number }

class Value {
  constructor(readonly type: Const) { }
  static bits(size: number, value: bigint | number | boolean) {
    return new Value(types.bits(size, value) as Const)
  }
  static f32(value: number) { return new Value(types.float32(value) as Const) }
  static f64(value: number) { return new Value(types.float64(value) as Const) }
  static from(x: Type) { return new Value(asConst(x)) }
  get value() { return this.type.value }
  toString() { return repr(this.type) }
}

function asConst(x: Type): Const {
  if (x.kind === 'bits' && x.value !== undefined) return x as Const
  if (x.kind === 'float32' && x.value !== undefined) return x as Const
  if (x.kind === 'float64' && x.value !== undefined) return x as Const
  throw new Error(`Expected constant bits/float, got ${repr(x)}`)
}

function asValue(x: unknown): Value {
  if (x instanceof Value) return x
  throw new Error(`Expected Value, got ${typeof x}`)
}

type IRValue = Type | Value
type MIR = ir.IR<IRValue, Type>

function irTypeOf(x: IRValue): Type {
  if (x instanceof Value) return types.abstract(x.type)
  return x
}

function showIRValue(x: IRValue | Type): string {
  if (x instanceof Value) return x.toString()
  return repr(x)
}

function MIR(meta: Def): MIR {
  return new ir.IR<IRValue, Type>(meta, irTypeOf, showIRValue)
}

class StringRef<T> extends ir.Expr<T> {
  constructor(readonly value: string) { super('string') }
  map(_: (x: T | number) => T | number): StringRef<T> { return new StringRef(this.value) }
  show(_: (x: T) => string): string { return JSON.stringify(this.value) }
}

function xstring<T>(s: string): StringRef<T> { return new StringRef<T>(s) }

class JS<T> extends ir.Expr<T> {
  constructor(readonly code: string, readonly params: string[] = [], args: (T | number)[] = []) {
    super('js', args)
  }
  map(f: (x: T | number) => T | number): JS<T> {
    return new JS(this.code, this.params, this.body.map(f))
  }
  show(pr: (x: T | number) => string): string {
    const args = this.body.length > 0 ? `, ${this.body.map(pr).join(', ')}` : ''
    return `js\`${this.code}\`${args}`
  }
}

function xjs<T>(code: string, params: string[] = [], args: (T | number)[] = []): JS<T> {
  return new JS<T>(code, params, args)
}

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

class Func<T> extends ir.Expr<T> {
  constructor(readonly method: Method, readonly args: (T | number)[]) { super('func', args) }
  map(f: (x: T | number) => T | number): Func<T> { return new Func(this.method, this.args.map(f)) }
  show(pr: (x: T | number) => string): string {
    return this.args.length > 0
      ? `func ${this.method.toString()}, ${this.args.map(pr).join(', ')}`
      : `func ${this.method.toString()}`
  }
}

function xfunc<T>(method: Method, ...args: (T | number)[]) {
  return new Func<T>(method, args)
}

class Wasm<T> extends ir.Expr<T> {
  readonly callee: Instruction | [string, string]
  constructor(
    callee: Instruction | string | [string, string],
    readonly args: (T | number)[],
    readonly result?: ValueType[]
  ) {
    super('wasm', args)
    this.callee = typeof callee === 'string' ? Op(callee) : callee
  }
  map(f: (x: T | number) => T | number): Wasm<T> { return new Wasm<T>(this.callee, this.args.map(f), this.result) }
  isImport(): this is Wasm<T> & { callee: [string, string] } {
    return Array.isArray(this.callee)
  }
  show(pr: (x: T | number) => string): string {
    let target: string
    if (this.isImport()) {
      target = `\$${this.callee[0]}.${this.callee[1]}`
    } else {
      const instr = this.callee as Instruction
      target = instr.kind === 'op' ? instr.name : `<${instr.kind}>`
    }
    return this.args.length > 0
      ? `call ${target}, ${this.args.map(pr).join(', ')}`
      : `call ${target}`
  }
}

function xwasm<T>(callee: Instruction | string | [string, string], ...args: (T | number)[]) {
  return new Wasm<T>(callee, args)
}

function calltarget(T: Type): Tag {
  if (T instanceof Tag) return T
  return types.asTag(types.tagOf(T))
}

function callargs(code: ir.Fragment<MIR>, ex: ir.Expr<IRValue>): [Tag | Method, ir.Val<MIR>[]] {
  return ex instanceof Invoke ?
    [ex.method, ex.body] :
    [calltarget(ir.asType(code.type(ex.body[0]))), ex.body]
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
    readonly id = cache.nft(),
    readonly ts?: string
  ) { }
  get [hash]() { return `${this.id}${this.params.map(x => types.repr(x)).join()}` }
  toString() { return `Method(${this.name})` }
  isEqual(other: unknown): other is Method { return other instanceof Method && this.id === other.id }
  param(...Ts: Type[]) {
    return new Method(this.mod, this.name, this.sig, this.func, Ts, this.id, this.ts)
  }
}

type MethodSource =
  | { kind: 'fn'; body: ast.Tree; meta: Def }
  | { kind: 'ir'; body: MIR }

class Methods implements cache.Caching {
  private imports = new cache.Ref<Tag[]>([])
  private methods = new cache.Map<Tag, (Method | Tag)[]>()
  private code = new cache.Map<Method, MethodSource>()

  get subcaches() { return [this.imports, this.methods, this.code] }
  get(k: Tag) { return this.methods.get(k) ?? this.imports.get() }
  source(m: Method) { return some(this.code.get(m)) }
  sourceid(m: Method) { return this.code.id(m) }

  method(m: Method, source?: MethodSource, { id }: { id?: bigint } = {}) {
    const ms = this.methods.get(m.name) ?? this.imports.get()
    this.methods.set(m.name, [...ms, m])
    if (source) this.code.set(m, source, { id })
    return m
  }

  import(mod: Tag) {
    if (this.imports.get().some(m => m.isEqual(mod))) return
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

  clone(): Methods {
    const out = new Methods()
    out.imports = this.imports.clone()
    out.methods = this.methods.clone()
    out.code = this.code.clone()
    return out
  }
}

class Module implements cache.Caching {
  readonly defs: cache.Map<string, Anno<Type> | Binding>
  readonly exports: Set<string>
  readonly methods: Methods
  constructor(
    readonly name: Tag,
    defs: cache.Map<string, Anno<Type> | Binding> = new cache.Map(),
    exports: Set<string> = new Set<string>(),
    methods: Methods = new Methods(),
  ) {
    this.defs = defs
    this.exports = exports
    this.methods = methods
  }

  get subcaches() { return [this.defs, this.methods] }
  method(name: Tag, sig: Signature, source: MethodSource, { ts, id }: { ts?: string, id?: bigint } = {}) {
    return this.methods.method(new Method(this.name, name, sig, undefined, [], undefined, ts), source, { id })
  }
  source(m: Method) { return this.methods.source(m) }
  sourceid(m: Method) { return this.methods.sourceid(m) }
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

  clone(): Module {
    return new Module(this.name, this.defs.clone(), new Set(this.exports), this.methods.clone())
  }
}

class Modules implements cache.Caching {
  private mods = new HashMap<Tag, Module>()
  readonly closures = new cache.Cache<Tag, [Method, MIR]>(name => {
    throw new Error(`Closure not found: ${name}`)
  })
  get subcaches() { return [...this.mods.values(), this.closures] }
  reset(deps: Set<bigint>) { cache.reset(cache.pipe(...this.subcaches), deps) }
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
  source(m: Method): MethodSource {
    if (this.closures.iscached(m.name))
      return { kind: 'ir', body: this.closures.get(m.name)[1] }
    return some(this.mods.get(m.mod)).source(m)
  }
  sourceid(m: Method): bigint {
    if (this.closures.iscached(m.name)) return this.closures.id(m.name)
    return some(this.mods.get(m.mod)).sourceid(m)
  }
  resolve_static(b: Binding): Anno<Type> {
    const val = this.get(b)
    if (val === undefined) return unreachable
    return val instanceof Binding ? this.resolve_static(val) : val
  }

  clone(): Modules {
    const out = new Modules()
    for (const [k, mod] of this.mods)
      out.mods.set(k, mod.clone())
    cache.reuse(out.closures, this.closures)
    return out
  }
}

function methods(cx: Modules, name: Tag, mod: Tag = tag(""), ms: Method[] = [], seen = new Set<Tag>()) {
  if (cx.closures.iscached(name)) return [cx.closures.get(name)[0]]
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
}
