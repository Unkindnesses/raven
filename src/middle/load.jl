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
  x isa AST.Template && x[1] == :tag && return Tag(x[2])
  return
end

function load_include(cx, x)
  path = "$common/$(x[2][1])"
  open(io -> loadfile(cx, io; path), path)
end

function load_expr(cx::LoadState, x; src)
  fname = Symbol(:__main, length(cx.main))
  env = collect(keys(cx.mod.defs))
  ir, defs = lower_toplevel(x, fname, src, env)
  foreach(x -> get!(cx.mod.defs, x, ⊥), defs)
  cx.mod.defs[fname] = Tag(fname)
  method!(cx.mod, RMethod(Tag(fname), lowerpattern(AST.List()), ir))
  push!(cx.main, fname)
end

isfn(x) = x[1] == :fn || ((x[1], x[2]) == (:extend, :fn))

function vload(cx::LoadState, x::AST.Syntax; src)
  x[1] == :include && return load_include(cx, x)
  x[1] == :bundle && return vload(cx, datamacro(x); src)
  isfn(x) || return load_expr(cx, x; src)
  extend = x[1] == :extend
  sig, body = extend ? x[3:end] : x[2:end]
  var = sig[1]::Symbol
  tag = extend ? cx.mod.defs[var]::Tag : Tag(var)
  cx.mod.defs[sig[1]::Symbol] = tag
  sig = lowerpattern(AST.List(sig[2:end]...))
  method!(cx.mod, RMethod(tag, sig, lowerfn(tag, sig, body, meta = AST.meta(x))))
  return
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
  body = AST.Block([AST.Call(f) for f in cx.main]...)
  options().memcheck && push!(body.args, AST.Call(:checkAllocations))
  sig = lowerpattern(AST.List())
  cx.mod.defs[:_start] = tag"_start"
  method!(cx.mod, RMethod(tag"_start", sig, lowerfn(tag"_start", sig, body)))
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
  mod = RModule(tag"")
  prelude!(mod)
  cx = LoadState(mod)
  path = "$common/common.rv"
  open(io -> loadfile(cx, io; path), path)
  open(io -> loadfile(cx, io, path = f), f)
  finish!(cx)
  return cx.mod
end

startmethod(mod) = mod.methods[tag"_start"][1]
