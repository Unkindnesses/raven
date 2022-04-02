base = joinpath(@__DIR__, "../../base")

function simpleconst(cx::Inference, x)
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

function load_expr(cx::Inference, x)
  push!(cx.main, x)
end

function vload(cx::Inference, x::Syntax)
  x.name == :import && return load_import(cx, x)
  x.name == :fn || return load_expr(cx, x)
  sig = x.args[1]
  f = sig isa Operator ? sig.op : sig.func
  args = Tuple(x.args[1].args)
  pat, args = lowerpattern(args)
  method!(cx.mod, f, RMethod(f, pat, args, lowerfn(x, args)))
  return f
end

vload(cx::Inference, x::Block) = foreach(x -> vload(cx, x), x.args)

function vload(cx::Inference, x::Operator)
  if x.op == :(=) && x.args[1] isa Symbol && (c = simpleconst(cx, x.args[2])) != nothing
    cx.mod.defs[x.args[1]] = c
  else
    load_expr(cx, x)
  end
end

vload(m::Inference, x) = load_expr(m, x)

function finish!(cx::Inference)
  method!(cx.mod, :_main, RMethod(:_main, lowerpattern(Tuple([]))..., lower_toplevel(Block(cx.main))))
  fn = Syntax(:fn, [Call(:_start, []), Block([Call(:_main, []), Call(:data, [Quote(:Nothing)])])])
  method!(cx.mod, :_start, RMethod(:_start, lowerpattern(Tuple([]))..., lowerfn(fn, [])))
end

function loadfile(cx::Inference, io::IO)
  io = LineNumberingReader(io)
  out = rnothing
  stmts(io)
  while (ex = parse(io)) != nothing
    out = vload(cx, ex)
    stmts(io)
  end
end

function loadfile(mod::RModule, f::String)
  cx = Inference(mod)
  open(io -> loadfile(cx, io), "$base/base.rv")
  open(io -> loadfile(cx, io), f)
  finish!(cx)
  frame!(cx, (startmethod(cx.mod),))
  infer!(cx)
  return cx
end

loadfile(f::String) = loadfile(RModule(), f)

startmethod(mod) = mod.methods[:_start][1]
