# Eval

struct RMethod
  name::Symbol
  pattern
  args
  func
  partial::Bool
end

RMethod(name, pat, args, func) = RMethod(name, pat, args, func, false)

Base.show(io::IO, meth::RMethod) = print(io, "RMethod($(meth.name))")

function select_method(mod, func::Symbol, args...; partial = true)
  args = data(:Tuple, args...)
  for meth in reverse(mod.methods[func])
    partial || !meth.partial || continue
    bs = match(mod, meth.pattern, args)
    bs == nothing || return (meth, bs)
  end
  error("No method matching $func($(join(args.parts[2:end], ", ")))")
end

function vinvoke(mod, func::Symbol, args...)
  found = select_method(mod, func, args..., partial = false)
  found == nothing && error("No method found for ($func, $(join(args, ", ")))")
  meth, bs = found
  f = meth.func
  args = [bs[a] for a in meth.args]
  f isa Function ? f(args...) : interpret(mod, f, args...)
end

struct RModule
  defs::Dict{Symbol,Any}
  methods::Dict{Symbol,Vector{RMethod}}
end

RModule() = primitives!(RModule(Dict{Symbol,Any}(), Dict{Symbol,IR}()))

function method!(mod::RModule, name::Symbol, m::RMethod)
  mod.defs[name] = name
  push!(get!(mod.methods, name, RMethod[]), m)
  return
end

@forward RModule.defs Base.getindex, Base.setindex!, Base.haskey

part_method = RMethod(:part, lowerpattern(rvx"(data, i)")..., part, true)

function primitives!(mod)
  method!(mod, :data, RMethod(:data, lowerpattern(rvx"args")..., args -> data(args.parts[2:end]...), true))
  method!(mod, :tuple, RMethod(:tuple, lowerpattern(rvx"args")..., identity, true))
  method!(mod, :part, RMethod(:part, lowerpattern(rvx"(data, i)")..., part, true))
  # TODO: this is a hacky fallback
  method!(mod, Symbol("isa?"), RMethod(Symbol("isa?"), lowerpattern(rvx"(x, T)")..., (x, T) -> Int32(tag(x) == T)))

  partial_widen(x::Primitive) = typeof(x)
  partial_widen(x) = x
  method!(mod, :widen, RMethod(:widen, lowerpattern(rvx"(x,)")..., partial_widen, true))

  for T in [Int64, Int32, Float64, Float32]
    mod[Symbol(T)] = Symbol(T)
    method!(mod, Symbol("isa?"), RMethod(Symbol("isa?"), lowerpattern(parse("(x, `$T`)"))..., x -> Int32(isprimitive(x, T))))
  end
  mod[:PrimitiveString] = :PrimitiveString
  method!(mod, Symbol("isa?"), RMethod(Symbol("isa?"), lowerpattern(rvx"(x, `PrimitiveString`)")..., x -> Int32(x isa String)))
  return mod
end

function interpreter_primitives!(mod)
  mod[:__backendWasm] = Int32(0)
  mod[Symbol("false")] = Int32(0)
  mod[Symbol("true")] = Int32(1)
  method!(mod, :data, RMethod(:data, lowerpattern(rvx"args")..., args -> data(args.parts[2:end]...)))
  method!(mod, :tuple, RMethod(:tuple, lowerpattern(rvx"args")..., identity))
  method!(mod, :part, RMethod(:part, lowerpattern(rvx"(data, i)")..., part))
  method!(mod, :nparts, RMethod(:nparts, lowerpattern(rvx"(x,)")..., nparts))
  method!(mod, :widen, RMethod(:widen, lowerpattern(rvx"(x,)")..., identity))
  method!(mod, :_print, RMethod(:_print, lowerpattern(parse("(a: String,)"))..., x -> (print(x); rnothing)))
  method!(mod, :string, RMethod(:string, lowerpattern(parse("(x: Symbol,)"))..., Base.string))
  method!(mod, Symbol("isa?"), RMethod(Symbol("isa?"), lowerpattern(rvx"(x, T)")..., (x, T) -> Int32(tag(x) == T)))
  method!(mod, :(==), RMethod(:(==), lowerpattern(rvx"(x: Symbol, y: Symbol)")..., (x, y) -> Int32(x==y)))
  method!(mod, :panic, RMethod(:panic, lowerpattern(parse("(x: String,)"))..., error))
  for T in [Int64, Int32, Float64, Float32]
    mod[Symbol(T)] = Symbol(T)
    method!(mod, Symbol("isa?"), RMethod(Symbol("isa?"), lowerpattern(parse("(x, `$T`)"))..., x -> Int32(isprimitive(x, T))))
    method!(mod, :string, RMethod(:string, lowerpattern(parse("(x: $T,)"))..., Base.string))
    for op in :[+, -, *, /, &, |].args
      method!(mod, op, RMethod(op, lowerpattern(parse("(a: $T, b: $T)"))...,
                                getfield(Base, op)))
    end
    for op in :[==, >, <, <=].args
      method!(mod, op, RMethod(op, lowerpattern(parse("(a: $T, b: $T)"))...,
                                (args...) -> getfield(Base, op)(args...) |> Int32))
    end
    for S in :[Int32, Int64].args
      method!(mod, S, RMethod(S, lowerpattern(parse("(x: $T,)"))..., x -> getfield(Base, S)(x)))
    end
  end
end

function eval_expr(m::RModule, x)
  ir = lowerexpr(x)
  interpret(m, ir)
end

function veval(m::RModule, x::Syntax)
  x.name == :fn || return eval_expr(m, x)
  sig = x.args[1]
  f = sig isa Operator ? sig.op : sig.func
  args = Tuple(x.args[1].args)
  pat, args = lowerpattern(args)
  method!(m, f, RMethod(f, pat, args, lowerfn(x, args)))
  return f
end

function veval(m::RModule, x::Operator)
  x.op == :(=) || return eval_expr(m, x)
  name, ex = x.args
  name isa Symbol || error("Invalid binding $name")
  m[name] = eval_expr(m, ex)
end

veval(m::RModule, x) = eval_expr(m, x)

function includerv(mod, io::IO)
  io = LineNumberingReader(io)
  out = rnothing
  stmts(io)
  while !eof(io)
    ex = parse(io)
    out = veval(mod, ex)
    stmts(io)
  end
  return out
end

includerv(mod, f::String) = open(io -> includerv(mod, io), f)
evalstring(mod, f::String) = includerv(mod, IOBuffer(f))

macro rv_str(x)
  :(evalstring(main, $x))
end
