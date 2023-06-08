using LNR

struct LoadState
  comp::Compilation
  mod::Union{RModule,Nothing}
  main::Vector{Tag}
end

LoadState(comp, mod = nothing) = LoadState(comp, mod, [])

const common = joinpath(@__DIR__, "../../common") |> normpath

function simpleconst(cx::LoadState, x)
  x isa Symbol && return cx.mod[x]
  x isa Primitive && return x
  x isa AST.Template && x[1] == :tag && return Tag(x[2])
  return
end

function load_include(cx, x)
  path = "$common/$(x[2][1])"
  open(io -> loadfile(cx, io; path), path)
end

function load_expr(cx::LoadState, x; src)
  fname = Tag(cx.mod.name, Symbol(:__main, length(cx.main)))
  env = collect(keys(cx.mod.defs))
  ir, defs = lower_toplevel(cx.mod, x; env, meta = FuncInfo(fname, src))
  foreach(x -> get!(cx.mod, x, ⊥), defs)
  method!(cx.mod, RMethod(fname, lowerpattern(AST.List()), ir))
  push!(cx.main, fname)
end

isfn(x) = x[1] == :fn || ((x[1], x[2]) == (:extend, :fn))

function vload(cx::LoadState, x::AST.Syntax; src)
  x[1] == :include && return load_include(cx, x)
  x[1] == :bundle && return vload(cx, datamacro(x); src)
  isfn(x) || return load_expr(cx, x; src)
  extend = x[1] == :extend
  sig, body = extend ? x[3:end] : x[2:end]
  sig = AST.ungroup(sig)
  var = sig[1]::Symbol
  tag = extend ? cx.mod[var]::Tag : Tag(cx.mod.name, var)
  cx.mod[var::Symbol] = tag
  sig = lowerpattern(AST.List(sig[2:end]...))
  method!(cx.mod, RMethod(tag, sig, lowerfn(cx.mod, sig, body, meta = FuncInfo(tag, AST.meta(x)))))
  return
end

vload(cx::LoadState, x::AST.Block; src) = foreach(x -> vload(cx, x; src), x[:])

function vload(cx::LoadState, x::AST.Operator; src)
  if x[1] == :(=) && x[2] isa Symbol && (c = simpleconst(cx, x[3])) != nothing
    cx.mod[x[2]] = c
  else
    load_expr(cx, x; src)
  end
end

vload(m::LoadState, x; src) = load_expr(m, x; src)

function finish!(cx::LoadState)
  body = AST.Block([AST.Call(AST.Template(:tag, string(f))) for f in cx.main]...)
  options().memcheck && push!(body.args, AST.Call(:checkAllocations))
  sig = lowerpattern(AST.List())
  method!(main(cx.comp), RMethod(tag"common.core.main", sig,
                                 lowerfn(main(cx.comp), sig, body,
                                         meta = FuncInfo(tag"common.core.main"))))
end

function loadfile(cx::LoadState, io::IO; path)
  io = LineNumberingReader(io)
  while true
    Parse.skip(io)
    cur = cursor(io)
    (ex = parse(io; path)) == nothing && break
    vload(cx, ex, src = Source(path, cur.line, cur.column))
  end
end

loadfile(cx::LoadState, path) =
  open(io -> loadfile(cx, io; path), path)

function loadmodule(cx::LoadState, mod::RModule, path)
  module!(cx.comp, mod)
  cx = LoadState(cx.comp, mod, cx.main)
  loadfile(cx, path)
  return mod
end

function loadmodule(cx::LoadState, mod::Tag, path)
  mod = haskey(cx.comp.mods, mod) ? cx.comp[mod] : prelude!(cx.comp, RModule(mod))
  return loadmodule(cx, mod, path)
end

function load(f::String)
  comp = Compilation()
  cx = LoadState(comp)

  module!(comp, core())
  loadmodule(cx, tag"common.core", "$common/core.rv")

  loadmodule(cx, tag"", "$common/common.rv")
  loadmodule(cx, tag"", f)

  finish!(cx)
  return comp
end

startmethod(mod::RModule) = mod.methods[tag"common.core.main"][1]
startmethod(cmp::Compilation) = startmethod(cmp.mods[tag""])
