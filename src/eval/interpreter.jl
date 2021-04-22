mutable struct Interpreter
  mod::RModule
  ir::IR
  ip::Base.Tuple{Int,Int}
  env::Dict{Variable,Any}
  stmts::Vector{Vector{Variable}}
  function Interpreter(mod::RModule, ir::IR, args...)
    env = Dict(zip(arguments(block(ir, 1)), args))
    new(mod, ir, (1, 1), env, keys.(blocks(ir)))
  end
end

lookup(it, v) = v
lookup(it, v::Quote) = v.expr
lookup(it, v::Variable) = it.env[v]
lookup(it, v::Symbol) = haskey(it.mod, v) ? it.mod[v] : error("$v not defined")

function step!(it::Interpreter)
  b, st = it.ip
  if st > length(it.stmts[b])
    for br in IRTools.branches(block(it.ir, b))
      IRTools.isconditional(br) && Bool(lookup(it, br.condition)) && continue
      IRTools.isreturn(br) && return lookup(it, br.args[1])
      it.ip = br.block, 1
      for (arg, x) in zip(arguments(block(it.ir, br.block)), br.args)
        it.env[arg] = lookup(it, x)
      end
      return
    end
    it.ip = b+1, 1
    return
  else
    v = it.stmts[b][st]
    ex = it.ir[v].expr
    it.env[v] = eval_stmt(it, ex)
    it.ip = b, st+1
    return
  end
end

function eval_stmt(it::Interpreter, ex)
  if isexpr(ex, :call)
    args = map(x -> lookup(it, x), ex.args)
    vinvoke(it.mod, args...)
  elseif !isexpr(ex)
    lookup(it, ex)
  elseif isexpr(ex, :import)
    includerv(it.mod, joinpath(base, "$base/$(ex.args[1]).rv"))
    return rnothing
  else
    error("Unrecognised expression $(ex.head)")
  end
end

function run!(it::Interpreter)
  while (r = step!(it)) == nothing end
  return r
end

interpret(mod, ir::IR, args...) = run!(Interpreter(mod, ir, args...))

function interpret(file::String)
  mod = RModule()
  interpreter_primitives!(mod)
  includerv(mod, joinpath(@__DIR__, "../../base", "base.rv"))
  includerv(mod, file)
end

# Old interp stuff

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
