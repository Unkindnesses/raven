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
  elseif isexpr(ex, :invoke)
    f = lookup(it, ex.args[1])
    args = parts(lookup(it, ex.args[2]))
    vinvoke(it.mod, f, args...)
  elseif !isexpr(ex)
    lookup(it, ex)
  elseif isexpr(ex, :import)
    includerv(it.mod, joinpath(base, "$base/$(ex.args[1]).rv"))
    return rnothing
  else
    error("Unrecognised expression $(ex.head)")
  end
end

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

function run!(it::Interpreter)
  while (r = step!(it)) == nothing end
  return r
end

interpret(mod, ir::IR, args...) = run!(Interpreter(mod, ir, args...))

function interpret(file::String)
  m = RModule()
  interpreter_primitives!(m)
  loadfile(m, file)
  interpret(m, startmethod(m).func)
  return
end

function interpreter_primitives!(mod)
  mod[Symbol("false")] = Int32(0)
  mod[Symbol("true")] = Int32(1)
  method!(mod, :data, RMethod(:data, lowerpattern(rvx"args")..., args -> data(parts(args)...)))
  method!(mod, :part, RMethod(:part, lowerpattern(rvx"(data, i)")..., part))
  method!(mod, :nparts, RMethod(:nparts, lowerpattern(rvx"(x,)")..., nparts))
  method!(mod, :datacat, RMethod(:datacat, lowerpattern(rvx"args")..., args -> datacat(parts(args)...)))
  method!(mod, :widen, RMethod(:widen, lowerpattern(rvx"(x,)")..., identity))
  method!(mod, :_print, RMethod(:_print, lowerpattern(parse("(a: String,)"))..., x -> (print(x); rnothing)))
  method!(mod, :string, RMethod(:string, lowerpattern(parse("(x: Symbol,)"))..., Base.string))
  method!(mod, Symbol("isa?"), RMethod(Symbol("isa?"), lowerpattern(rvx"(x, T)")..., (x, T) -> Int32(tag(x) == T)))
  method!(mod, :(==), RMethod(:(==), lowerpattern(rvx"(x: Symbol, y: Symbol)")..., (x, y) -> Int32(x==y)))
  method!(mod, :panic, RMethod(:panic, lowerpattern(parse("(x: String,)"))..., error))
  method!(mod, :not, RMethod(:not, lowerpattern(parse("(x: Int32,)"))..., x -> Int32(x==0)))
  for T in [Int64, Int32, Float64, Float32]
    mod[Symbol(T)] = Symbol(T)
    method!(mod, Symbol("isa?"), RMethod(Symbol("isa?"), lowerpattern(parse("(x, `$T`)"))..., x -> Int32(x isa T)))
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
