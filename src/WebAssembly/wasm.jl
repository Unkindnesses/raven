@enum WType i32 i64 f32 f64

WType(::Type{Int32}) = i32
WType(::Type{Int64}) = i64
WType(::Type{Float32}) = f32
WType(::Type{Float64}) = f64

WType(::Type{<:Union{Bool,UInt32}}) = i32
WType(::Type{UInt64}) = i64

const wtypenames = Dict(Symbol(x) => x for x in instances(WType))
WType(s::Symbol) = wtypenames[s]

WType(T::WType) = T

jltype(x::WType) = [Int32, Int64, Float32, Float64][Int(x)+1]

abstract type Instruction end

struct Const <: Instruction
  val::Union{Int32,Int64,Float32,Float64}
end

Const(x::UInt32) = Const(reinterpret(Int32, x))
Const(x::UInt64) = Const(reinterpret(Int64, x))

WType(x::Const) = WType(typeof(x.val))

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

struct Signature
  params::Vector{WType}
  result::Vector{WType}
end

Base.:(==)(a::Signature, b::Signature) = a.params == b.params && a.result == b.result
Base.hash(s::Signature, h::UInt) = hash((0xa0029abae2de0ab6, s.params, s.result), h)

Base.convert(::Type{Signature}, (p,r)::Pair) = Signature(p, r)

struct Func
  name::Symbol
  sig::Signature
  locals::Vector{WType}
  body::Block
  meta
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
Global(T::WType, mut = true) = Global(jltype(T)(0), mut)

struct Data
  memidx::UInt32
  offset::UInt32
  data::Vector{UInt8}
end

struct Import
  mod::Symbol
  name::Symbol
  as::Symbol
  sig::Signature
end

struct Export
  as::Symbol
  name::Symbol
end

struct Module
  funcs::Vector{Func}
  mems::Vector{Mem}
  globals::Vector{Global}
  data::Vector{Data}
  imports::Vector{Import}
  exports::Vector{Export}
end

Module(; funcs = [], mems = [], globals = [], data = [], imports = [], exports = []) =
  Module(funcs, mems, globals, data, imports, exports)
