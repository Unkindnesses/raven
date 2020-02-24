struct Source
  mod::RModule
  main::Vector{Any}
end

Source() = Source(RModule(), [])

base = joinpath(@__DIR__, "../../base")

function simpleconst(cx::Source, x)
  x isa Symbol && return cx.mod.defs[x]
  x isa Primitive && return x
  x isa Quote && return x.expr
  return
end

function importpath(x)
  modname(x::Operator) = Base.string(modname(x.args[1]), ".", modname(x.args[2]))
  modname(x::Symbol) = Base.string(x)
  name = x.args[1].args[1]
  name = modname(name)
  path = replace(name, "."=>"/")
end

function load_import(cx, x)
  open(io -> loadfile(cx, io), "$base/$(importpath(x)).rv")
end

function load_expr(cx::Source, x)
  push!(cx.main, x)
end

function static_if(cx::Source, x)
  if simpleconst(cx, x.args[1]) != nothing
    simpleconst(cx, x.args[1]) && vload(cx, x.args[2])
  else
    load_expr(cx, x)
  end
end

function vload(cx::Source, x::Syntax)
  x.name == :import && return load_import(cx, x)
  x.name == :if && return static_if(cx, x)
  x.name == :fn || return load_expr(cx, x)
  sig = x.args[1]
  f = sig isa Operator ? sig.op : sig.func
  args = Tuple(x.args[1].args)
  pat, args = lowerpattern(args)
  method!(cx.mod, f, RMethod(pat, args, lowerfn(x, args)))
  return f
end

vload(cx::Source, x::Block) = foreach(x -> vload(cx, x), x.args)

function vload(cx::Source, x::Operator)
  if x.op == :(=) && x.args[1] isa Symbol && simpleconst(cx, x.args[2]) != nothing
    cx.mod.defs[x.args[1]] = simpleconst(cx, x.args[2])
  else
    push!(cx.main, x)
  end
end

vload(m::Source, x) = load_expr(m, x)

function finish!(cx::Source)
  fn = Syntax(:fn, [Call(:_start, []), Block(cx.main)])
  method!(cx.mod, :_start, RMethod(lowerpattern(Tuple([])), [], lowerfn(fn, [])))
end

function loadfile(cx::Source, io::IO)
  io = LineNumberingReader(io)
  out = rnothing
  stmts(io)
  while !eof(io)
    ex = parse(io)
    out = vload(cx, ex)
    stmts(io)
  end
end

function _loadfile(f::String)
  cx = Source()
  cx.mod.defs[:__backendWasm] = true
  open(io -> loadfile(cx, io), "$base/base.rv")
  open(io -> loadfile(cx, io), f)
  finish!(cx)
  return cx.mod
end

loadfile(f::String) = Inference(_loadfile(f))
