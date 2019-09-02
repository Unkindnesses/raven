# Types

struct Struct
  data::Vector{Any}
end

vstruct(x...) = Struct(Any[x...])

nparts(x::Struct) = length(x.data)-1
part(x::Struct, i) = x.data[i+1]

Base.:(==)(a::Struct, b::Struct) = a.data == b.data

tag(x) = part(x, 0)

const vnothing = vstruct(:Nothing)

Primitive = Union{Int,Float64,Symbol,String}

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

struct VMethod
  pattern
  args
  func
end

function select_method(func::Symbol, args...)
  args = vstruct(:Tuple, args...)
  for meth in reverse(main.methods[func])
    bs = match(meth.pattern, args)
    bs == nothing || return (meth, bs)
  end
end

function vinvoke(func::Symbol, args...)
  meth, bs = select_method(func, args...)
  f = meth.func
  args = [bs[a] for a in meth.args]
  f isa Function ? f(args...) : interpret(f, args...)
end

struct VModule
  defs::Dict{Symbol,Any}
  methods::Dict{Symbol,Vector{VMethod}}
end

VModule() = VModule(Dict{Symbol,Any}(), Dict{Symbol,IR}())

function method!(mod::VModule, name::Symbol, m::VMethod)
  mod.defs[name] = name
  push!(get!(mod.methods, name, VMethod[]), m)
  return
end

@forward VModule.defs Base.getindex, Base.setindex!, Base.haskey

function eval_expr(m::VModule, x)
  ir = lowerexpr(x)
  interpret(ir)
end

function veval(m::VModule, x::Block)
  x.name == :fn || return eval_expr(m, x)
  f = x.args[1].func
  args = Tuple(x.args[1].args)
  pat, args = lowerpattern(args)
  method!(main, f, VMethod(pat, args, lowerfn(x, args)))
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
  out = vnothing
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
             :tuple => (a...) -> vstruct(:Tuple, a...),
             :part => part, :nparts => nparts, :tag => tag]
  method!(main, name,
          VMethod(vstruct(:Bind, :args, vstruct(:Hole)), [:args],
                  args -> def(args.data[2:end]...)))
end
