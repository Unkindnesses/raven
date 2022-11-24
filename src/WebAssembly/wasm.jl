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

struct Select <: Instruction end

struct Convert <: Instruction
  to::WType
  from::WType
  name::Symbol
end

struct Block <: Instruction
  body::Vector{Instruction}
end

struct If <: Instruction
  t::Vector{Instruction}
  f::Vector{Instruction}
end

struct Loop <: Instruction
  body::Vector{Instruction}
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

const unreachable = Unreachable()

struct Func
  name::Symbol
  params::Vector{WType}
  result::Vector{WType}
  locals::Vector{WType}
  body::Block
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

Global(val, mut = true) = Global(WType(typeof(val)), mut, Const(val))
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
  params::Vector{WType}
  result::Vector{WType}
end

struct Export
  as::Symbol
  name::Symbol
end

# TODO perhaps split this into sections
struct Module
  funcs::Vector{Func}
  mems::Vector{Mem}
  globals::Vector{Global}
  data::Vector{Data}
  imports::Vector{Import}
  exports::Vector{Export}
end

function func(m::Module, name)
  i = findfirst(f -> f.name == name, m.funcs)
  i == nothing || return m.funcs[i]
  i = findfirst(f -> f.as == name, m.imports)
  i == nothing || return m.imports[i]
  error("Function $name not found.")
end

Module(; funcs = [], mems = [], globals = [], data = [], imports = [], exports = []) =
  Module(funcs, mems, globals, data, imports, exports)

# Printing

Base.show(io::IO, i::Nop)         = print(io, "nop")
Base.show(io::IO, i::Const)       = print(io, WType(i), ".const ", i.val)
Base.show(io::IO, i::Local)       = print(io, "local.get ", i.id)
Base.show(io::IO, i::SetLocal)    = print(io, i.tee ? "local.tee " : "local.set ", i.id)
Base.show(io::IO, i::GetGlobal)   = print(io, "global.get ", i.id)
Base.show(io::IO, i::SetGlobal)   = print(io, "global.set ", i.id)
Base.show(io::IO, i::Op)          = print(io, i.name)
Base.show(io::IO, i::Call)        = print(io, "call \$", i.name)
Base.show(io::IO, i::Convert)     = print(io, i.to, ".", i.name, "/", i.from)
Base.show(io::IO, i::Select)      = print(io, "select")
Base.show(io::IO, i::Branch)      = print(io, i.cond ? "br_if " : "br ", i.level)
Base.show(io::IO, i::Return)      = print(io, "return")
Base.show(io::IO, i::Unreachable) = print(io, "unreachable")

printwasm(io, x, level) = show(io, x)

function printwasm_(io, xs, level)
  for x in xs
    print(io, "\n", "  "^(level))
    print(io, "(")
    printwasm(io, x, level)
    print(io, ")")
  end
end

function printwasm(io, x::If, level)
  level += 1
  print(io, "if")
  if !isempty(x.t)
    print(io, "\n", "  "^level, "(then")
    printwasm_(io, x.t, level+1)
  end
  if !isempty(x.f)
    print(io, ")\n", "  "^level, "(else")
    printwasm_(io, x.f, level+1)
  end
  print(io, ")")
end

function printwasm(io, x::Block, level)
  print(io, "block")
  printwasm_(io, x.body, level+1)
end

function printwasm(io, x::Loop, level)
  print(io, "loop")
  printwasm_(io, x.body, level+1)
end

Base.show(io::IO, i::Union{Block,Loop,If}) = printwasm(io, i, 0)

function printwasm(io, x::Mem, level)
  print(io, "\n", "  "^(level))
  print(io, "(memory $(x.min))")    # TODO: add x.max
end

function printwasm(io, x::Data, level)
  print(io, "\n", "  "^(level))
  print(io, """(data (i32.const $(x.offset)) "$(String(x.data))"))""")
end

function printwasm(io, x::Export, level)
  print(io, "\n", "  "^(level))
  print(io, "(export \"$(x.as)\" (func \$$(x.name)))")
end

function printwasm(io, x::Import, level)
  print(io, "\n", "  "^(level))
  print(io, "(import \"$(x.mod)\" \"$(x.name)\" (func \$$(x.as)")
  if length(x.params) > 0
    print(io, " (param")
    foreach(p -> print(io, " $p"), x.params)
    print(io, ")")
  end
  if length(x.result) > 0
    print(io, " (result")
    foreach(p -> print(io, " $p"), x.result)
    print(io, ")")
  end
  print(io, "))")
end

function printwasm(io, x::Global, level)
  print(io, "\n", "  "^(level))
  print(io, "(global ")
  if x.mut
    print(io, "(mut ", x.type, ") ")
  else
    print(io, x.type, " ")
  end
  print(io, "(", x.init, "))")
end

function printvars(io, name, vs)
  if !isempty(vs)
    print(io, " (", name, " ")
    join(io, vs, " ")
    print(io, ")")
  end
end

function printwasm(io::IO, f::Func, level)
  print(io, "\n", "  "^(level))
  print(io, "(func \$$(f.name)")
  printvars(io, "param", f.params)
  printvars(io, "result", f.result)
  !isempty(f.locals) && print(io, "\n", "  "^level, " ")
  printvars(io, "local", f.locals)
  printwasm_(io, f.body.body, level + 1)
  print(io, ")")
end

Base.show(io::IO, f::Func) = printwasm(io, f, 1)

function Base.show(io::IO, m::Module)
  print(io, "(module")
  foreach(p -> printwasm(io, p, 1), m.imports)
  foreach(p -> printwasm(io, p, 1), m.exports)
  foreach(p -> printwasm(io, p, 1), m.globals)
  foreach(p -> printwasm(io, p, 1), m.mems)
  foreach(p -> printwasm(io, p, 1), m.data)
  foreach(p -> printwasm(io, p, 1), m.funcs)
  print(io, ")")
end
