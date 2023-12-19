@enum WType i32 i64 f32 f64 externref

WType(::Type{Int32}) = i32
WType(::Type{Int64}) = i64
WType(::Type{Float32}) = f32
WType(::Type{Float64}) = f64

WType(::Type{<:Union{Bool,UInt32}}) = i32
WType(::Type{UInt64}) = i64

const wtypenames = Dict(Symbol(x) => x for x in instances(WType))
WType(s::Symbol) = wtypenames[s]

WType(T::WType) = T

abstract type ExternRef end

jltype(x::WType) = [Int32, Int64, Float32, Float64, ExternRef][Int(x)+1]

struct Signature
  params::Vector{WType}
  result::Vector{WType}
end

abstract type Instruction end

struct Const <: Instruction
  val::Union{Int32,Int64,Float32,Float64}
end

Const(x::UInt32) = Const(reinterpret(Int32, x))
Const(x::UInt64) = Const(reinterpret(Int64, x))

WType(x::Const) = WType(typeof(x.val))

struct RefNull <: Instruction
  type::WType
end

struct Nop <: Instruction end

const nop = Nop()

struct Local <: Instruction
  id::Int
end

struct SetLocal <: Instruction
  tee::Bool
  id::Int
end

struct GetGlobal <: Instruction
  id::Int
end

struct SetGlobal <: Instruction
  id::Int
end

struct Op <: Instruction
  name::Symbol
end

Op(x::WType, op::Symbol) = Op(Symbol(x, ".", op))

Base.getproperty(x::WType, op::Symbol) = Op(x, op)

struct Drop <: Instruction end

struct Select <: Instruction end

struct Convert <: Instruction
  to::WType
  from::WType
  name::Symbol
end

struct Branch <: Instruction
  cond::Bool
  level::Int
end

struct Call <: Instruction
  name::Symbol
end

struct CallIndirect <: Instruction
  sig::Signature
  table::UInt32
end

Branch(l::Integer) = Branch(false, l)

struct Return <: Instruction end

struct Unreachable <: Instruction end

struct Block <: Instruction
  body::Vector{Instruction}
  srcs::Vector{Union{LineInfo,Nothing}}
end

struct Loop <: Instruction
  body::Vector{Instruction}
  srcs::Vector{Union{LineInfo,Nothing}}
end

function instr!(b::Union{Block,Loop}, it::Instruction, src = nothing)
  push!(b.body, it)
  push!(b.srcs, src)
end

Block(is) = Block(is, [nothing for _ in is])
Loop(is) = Loop(is, [nothing for _ in is])

const unreachable = Unreachable()

Base.:(==)(a::Signature, b::Signature) = a.params == b.params && a.result == b.result
Base.hash(s::Signature, h::UInt) = hash((0xa0029abae2de0ab6, s.params, s.result), h)

Signature((p, r)::Pair) = Signature(p, r)
Base.convert(::Type{Signature}, (p,r)::Pair) = Signature(p, r)

struct Func
  name::Symbol
  sig::Signature
  locals::Vector{WType}
  body::Block
  meta
end

struct Table
  min::UInt32
end

struct Mem
  min::UInt32
  max::Union{UInt32,Nothing}
  Mem(min::Integer, max = nothing) = new(min, max)
end

struct Global
  type::WType
  mut::Bool
  init::Instruction
end

Global(val::Number, mut = true) = Global(WType(typeof(val)), mut, Const(val))
Global(T::WType, mut = true) =
  T == externref ?
    Global(T, mut, RefNull(T)) :
    Global(jltype(T)(0), mut)

struct Elem
  table::UInt32
  data::Vector{Symbol}
end

struct Data
  memidx::UInt32
  offset::UInt32
  data::Vector{UInt8}
end

struct Import
  mod::Symbol
  name::Symbol
  as::Symbol
  sig::Union{Signature,Global,Mem,Table}
end

Import(mod::Symbol, name::Symbol, as::Symbol, sig::Pair) =
  Import(mod, name, as, Signature(sig))

Import(mod::Symbol, name::Symbol, sig) = Import(mod, name, name, sig)

struct Export
  as::Symbol
  name::Symbol
end

struct Module
  funcs::Vector{Func}
  mems::Vector{Mem}
  tables::Vector{Table}
  globals::Vector{Global}
  elems::Vector{Elem}
  data::Vector{Data}
  imports::Vector{Import}
  exports::Vector{Export}
end

Module(; funcs = [], mems = [], tables = [], globals = [], elems = [], data = [], imports = [], exports = []) =
  Module(funcs, mems, tables, globals, elems, data, imports, exports)

signatures(m::Module) = unique(f.sig for f in vcat(m.imports, m.funcs) if f.sig isa Signature)

# Some AST utils

callees(x::Func, cs = Symbol[]) = callees(x.body, cs)

function callees(x::Union{Block,Loop}, cs)
  foreach(x -> callees(x, cs), x.body)
  return cs
end

callees(x::Call, cs) = push!(cs, x.name)
callees(x::Instruction, cs) = cs
