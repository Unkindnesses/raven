# Eval

struct RMethod
  pattern
  args
  func
  partial
end

RMethod(pat, args, func) = RMethod(pat, args, func, nothing)

function select_method(func::Symbol, args...)
  args = data(:Tuple, args...)
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

function primitives!(mod)
  for (name, def) in [:data => (t, a...) -> Data([t, a...]),
                      :tuple => (a...) -> data(:Tuple, a...),
                      :part => part, :nparts => nparts, :tag => tag]
    method!(mod, name,
            RMethod(lowerpattern(mod, rvx"args")...,
                    args -> def(args.parts[2:end]...)))
  end
  for T in [Int64, Int32, Float64, Float32]
    mod[Symbol(T)] = Symbol(T)
    method!(mod, :isa, RMethod(lowerpattern(mod, parse("(x, `$T`)"))..., x -> Int32(isprimitive(x, T))))

    for (name, def) in [:+ => +, :- => -, :* => *, :/ => /]
      method!(mod, name,
              RMethod(lowerpattern(mod, parse("(x::$T, y::$T)"))..., def,
                      (a, b) -> PrimitiveHole{T}()))
    end

    for (name, def) in [:> => >]
      method!(mod, name,
              RMethod(lowerpattern(mod, parse("(x::$T, y::$T)"))..., (x...) -> Int32(def(x...)),
                      (a, b) -> PrimitiveHole{Bool}()))
    end

    for S in [Int64, Int32, Float64, Float32]
      method!(mod, Symbol(S),
              RMethod(lowerpattern(mod, parse("(x::$T,)"))..., x -> S(x),
                      _ -> PrimitiveHole{T}()))
    end
  end

  return mod
end

function eval_expr(m::RModule, x)
  ir = lowerexpr(x)
  interpret(ir)
end

function veval(m::RModule, x::Syntax)
  x.name == :fn || return eval_expr(m, x)
  f = x.args[1].func
  args = Tuple(x.args[1].args)
  pat, args = lowerpattern(m, args)
  method!(main, f, RMethod(pat, args, lowerfn(x, args)))
  return f
end

function veval(m::RModule, x::Operator)
  x.op == :(=) || return eval_expr(m, x)
  name, ex = x.args
  name isa Symbol || error("Invalid binding $name")
  m[name] = eval_expr(m, ex)
end

veval(m::RModule, x) = eval_expr(m, x)

const main = RModule()

veval(x) = veval(main, x)

function evalfile(io::IO)
  io = LineNumberingReader(io)
  out = rnothing
  stmts(io)
  while !eof(io)
    ex = parse(io)
    out = veval(ex)
    stmts(io)
  end
  return out
end

evalfile(f::String) = open(evalfile, f)
evalstring(f::String) = evalfile(IOBuffer(f))

macro rv_str(x)
  :(evalstring($x))
end
