# Types

struct Struct
  data::Vector{Any}
end

vstruct(x...) = Struct(Any[x...])

nparts(x::Struct) = length(x.data)-1
part(x::Struct, i) = x.data[i+1]

tag(x) = part(x, 0)

# Primitive Types
for T in :[Int64, Float64, Symbol, String].args
  @eval part(x::$T, i::Integer) =
          i == 0 ? $(QuoteNode(T)) :
          i == 1 ? x :
          error("Tried to access part $i of 2")
  @eval nparts(x::$T) = 2
end

# Printing

function vprint(io::IO, s::Struct)
  print(io, "struct(")
  join(io, [sprint(vprint, x) for x in s.data], ", ")
  print(io, ")")
end

vprint(io, x::Symbol) = print(io, "`", x, "`")
vprint(io::IO, x::Union{Int64, Float64}) = print(io, x)

vprint(io::IO, x::Expr) = print(io, "`", x, "`")

# Eval

struct VModule
  defs::Dict{Symbol,Any}
  methods::Dict{Symbol,Any}
end

VModule() = VModule(Dict{Symbol,Any}(), Dict{Symbol,IR}())

@forward VModule.defs Base.getindex, Base.setindex!

function eval_expr(m::VModule, x)
  ir = lowerexpr(x)
  interpret(ir)
end

function veval(m::VModule, x::Block)
  x.name == :fn || return eval_expr(m, x)
  f = x.args[1].func
  m[f] = f
  m.methods[f] = lowerfn(x)
  return f
end

function veval(m::VModule, x::Operator)
  x.op == :(=) || return eval_expr(m, x)
  name, ex = x.args
  name isa Symbol || error("Invalid binding $name")
  m[name] = eval_expr(m, ex)
end

veval(m::VModule, x) = eval_expr(m, x)

const main = VModule()

veval(x) = veval(main, x)

function evalfile(io::IO)
  io = LineNumberingReader(io)
  out = nothing
  stmts(io)
  while !eof(io)
    ex = parse(io, 0)
    out = veval(ex)
    stmts(io)
  end
  return out
end

evalfile(f::String) = open(evalfile, f)
evalstring(f::String) = evalfile(IOBuffer(f))

macro vs_str(x)
  :(evalstring($x))
end

# Builtins

for (name, def) in [:> => >, :+ => +, :- => -, :* => *, :/ => /,
             :struct => (t, a...) -> Struct([t, a...]),
             :part => part, :nparts => nparts, :tag => tag]
  main[name] = name
  main.methods[name] = def
end
