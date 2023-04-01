using LNR

struct LoadState
  mod::RModule
  main::Vector{Any}
end

LoadState(mod) = LoadState(mod, [])

const common = joinpath(@__DIR__, "../../common") |> normpath

function simpleconst(cx::LoadState, x)
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
  path = "$common/$(importpath(x)).rv"
  open(io -> loadfile(cx, io; path), path)
end

function load_expr(cx::LoadState, x; src)
  fname = Symbol(:__main, length(cx.main))
  defs = collect(keys(cx.mod.defs))
  method!(cx.mod, fname, RMethod(fname, lowerpattern(AST.List()), lower_toplevel(x, fname, src, defs)))
  push!(cx.main, fname)
end

function vload(cx::LoadState, x::AST.Syntax; src)
  x[1] == :import && return load_import(cx, x)
  x[1] == :bundle && return vload(cx, datamacro(x); src)
  x[1] == :fn || return load_expr(cx, x; src)
  sig = x[2]
  f = sig[1]
  args = AST.List(sig[2:end]...)
  sig = lowerpattern(args)
  method!(cx.mod, f, RMethod(f, sig, lowerfn(x, sig)))
  return f
end

vload(cx::LoadState, x::AST.Block; src) = foreach(x -> vload(cx, x; src), x[:])

function vload(cx::LoadState, x::AST.Operator; src)
  if x[1] == :(=) && x[2] isa Symbol && (c = simpleconst(cx, x[3])) != nothing
    cx.mod.defs[x[2]] = c
  else
    load_expr(cx, x; src)
  end
end

vload(m::LoadState, x; src) = load_expr(m, x; src)

function finish!(cx::LoadState)
  body = [AST.Call(f) for f in cx.main]
  options().memcheck && push!(body, AST.Call(:checkAllocations))
  fn = AST.Syntax(:fn, AST.Call(:_start),
                       AST.Block(body...))
  sig = lowerpattern(AST.List())
  method!(cx.mod, :_start, RMethod(:_start, sig, lowerfn(fn, sig)))
end

function loadfile(cx::LoadState, io::IO; path)
  io = LineNumberingReader(io)
  while true
    Parse.stmts(io)
    cur = cursor(io)
    (ex = parse(io; path)) == nothing && break
    vload(cx, ex, src = Source(path, cur.line, cur.column))
    Parse.stmts(io)
  end
end

function loadfile(f::String; partial = false)
  mod = RModule()
  cx = LoadState(mod)
  path = "$common/common.rv"
  open(io -> loadfile(cx, io; path), path)
  open(io -> loadfile(cx, io, path = f), f)
  finish!(cx)
  return cx.mod
end

startmethod(mod) = mod.methods[:_start][1]
