using LNR

base = joinpath(@__DIR__, "../../base")

function simpleconst(cx::Inference, x)
  x isa Symbol && return cx.mod.defs[x]
  x isa Primitive && return x
  x isa AST.Quote && return x[1]
  return
end

function importpath(x)
  modname(x::AST.Operator) = Base.string(modname(x[2]), ".", modname(x[3]))
  modname(x::Symbol) = Base.string(x)
  name = x[2][1]
  name = modname(name)
  path = replace(name, "."=>"/")
end

function load_import(cx, x)
  path = "$base/$(importpath(x)).rv"
  open(io -> loadfile(cx, io; path), path)
end

function load_expr(cx::Inference, x)
  fname = Symbol(:__main, length(cx.main))
  defs = collect(keys(cx.mod.defs))
  method!(cx.mod, fname, RMethod(fname, lowerpattern(AST.List()), lower_toplevel(x, defs)))
  push!(cx.main, fname)
end

function vload(cx::Inference, x::AST.Syntax)
  x[1] == :import && return load_import(cx, x)
  x[1] == :bundle && return vload(cx, datamacro(x))
  x[1] == :fn || return load_expr(cx, x)
  sig = x[2]
  f = sig[1]
  args = AST.List(sig[2:end]...)
  sig = lowerpattern(args)
  method!(cx.mod, f, RMethod(f, sig, lowerfn(x, sig)))
  return f
end

vload(cx::Inference, x::AST.Block) = foreach(x -> vload(cx, x), x[:])

function vload(cx::Inference, x::AST.Operator)
  if x[1] == :(=) && x[2] isa Symbol && (c = simpleconst(cx, x[3])) != nothing
    cx.mod.defs[x[2]] = c
  else
    load_expr(cx, x)
  end
end

vload(m::Inference, x) = load_expr(m, x)

function finish!(cx::Inference)
  fn = AST.Syntax(:fn, AST.Call(:_start),
                       AST.Block([AST.Call(f) for f in cx.main]...,
                                 AST.Call(:checkAllocations)))
  sig = lowerpattern(AST.List())
  method!(cx.mod, :_start, RMethod(:_start, sig, lowerfn(fn, sig)))
end

function loadfile(cx::Inference, io::IO; path)
  io = LineNumberingReader(io)
  Parse.stmts(io)
  while (ex = parse(io; path)) != nothing
    vload(cx, ex)
    Parse.stmts(io)
  end
end

function loadfile(f::String; infer = true, partial = false)
  mod = RModule()
  cx = Inference(mod)
  path = "$base/base.rv"
  open(io -> loadfile(cx, io; path), path)
  open(io -> loadfile(cx, io, path = f), f)
  finish!(cx)
  infer || return cx
  P = Parent((), 1)
  frame!(cx, P, :malloc!, rlist(Int32))
  frame!(cx, P, :retain!, rlist(pack(:Ptr, Int32)))
  frame!(cx, P, :release!, rlist(pack(:Ptr, Int32)))
  frame!(cx, P, :blockUnique, rlist(pack(:Ptr, Int32)))
  frame!(cx, P, :println, rlist(String))
  frame!(cx, P, startmethod(cx.mod))
  infer && infer!(cx; partial)
  return cx
end

startmethod(mod) = mod.methods[:_start][1]
