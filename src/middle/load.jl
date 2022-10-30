using LNR

base = joinpath(@__DIR__, "../../base")

function simpleconst(cx::Inference, x)
  x isa Symbol && return cx.mod.defs[x]
  x isa Primitive && return x
  x isa AST.Quote && return x.expr
  return
end

function importpath(x)
  modname(x::AST.Operator) = Base.string(modname(x.args[1]), ".", modname(x.args[2]))
  modname(x::Symbol) = Base.string(x)
  name = x.args[1].args[1]
  name = modname(name)
  path = replace(name, "."=>"/")
end

function load_import(cx, x)
  open(io -> loadfile(cx, io), "$base/$(importpath(x)).rv")
end

function load_expr(cx::Inference, x)
  fname = Symbol(:__main, length(cx.main))
  defs = collect(keys(cx.mod.defs))
  method!(cx.mod, fname, RMethod(fname, lowerpattern(AST.List([])), lower_toplevel(x, defs)))
  push!(cx.main, fname)
end

function vload(cx::Inference, x::AST.Syntax)
  x.name == :import && return load_import(cx, x)
  x.name == :bundle && return vload(cx, datamacro(x))
  x.name == :fn || return load_expr(cx, x)
  sig = x.args[1]
  f = sig isa AST.Operator ? sig.op : sig.func
  args = AST.List(x.args[1].args)
  sig = lowerpattern(args)
  method!(cx.mod, f, RMethod(f, sig, lowerfn(x, sig)))
  return f
end

vload(cx::Inference, x::AST.Block) = foreach(x -> vload(cx, x), x.args)

function vload(cx::Inference, x::AST.Operator)
  if x.op == :(=) && x.args[1] isa Symbol && (c = simpleconst(cx, x.args[2])) != nothing
    cx.mod.defs[x.args[1]] = c
  else
    load_expr(cx, x)
  end
end

vload(m::Inference, x) = load_expr(m, x)

function finish!(cx::Inference)
  fn = AST.Syntax(:fn, [AST.Call(:_start, []),
                  AST.Block([[AST.Call(f, []) for f in cx.main]..., AST.Call(:checkAllocations, [])])])
  sig = lowerpattern(AST.List([]))
  method!(cx.mod, :_start, RMethod(:_start, sig, lowerfn(fn, sig)))
end

function loadfile(cx::Inference, io::IO)
  io = LineNumberingReader(io)
  Parse.stmts(io)
  while (ex = parse(io)) != nothing
    vload(cx, ex)
    Parse.stmts(io)
  end
end

function loadfile(f::String; infer = true, partial = false)
  mod = RModule()
  cx = Inference(mod)
  open(io -> loadfile(cx, io), "$base/base.rv")
  open(io -> loadfile(cx, io), f)
  finish!(cx)
  infer || return cx
  frame!(cx, (), :malloc!, rlist(Int32))
  frame!(cx, (), :retain!, rlist(pack(:Ptr, Int32)))
  frame!(cx, (), :release!, rlist(pack(:Ptr, Int32)))
  frame!(cx, (), :blockUnique, rlist(pack(:Ptr, Int32)))
  frame!(cx, (), :println, rlist(String))
  frame!(cx, (), startmethod(cx.mod))
  infer && infer!(cx; partial)
  return cx
end

startmethod(mod) = mod.methods[:_start][1]
