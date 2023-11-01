using LNR

struct SourceString
  path::String
  source::String
end

macro src_str(s)
  SourceString("$(__source__.file):$(__source__.line)", s)
end

struct LoadState
  comp::Modules
  mod::RModule
end

const common = joinpath(@__DIR__, "../../common") |> normpath

resolve_static(cx::LoadState, x::Symbol) =
  resolve_static(cx.comp, Binding(cx.mod.name, x))

function simpleconst(cx::LoadState, x)
  x isa Symbol && return cx.mod[x]
  x isa Primitive && return x
  x isa AST.Template && x[1] == :tag && return modtag(cx.mod.name, x[2])
  return
end

function load_export(cx, x)
  names = (x[2]::AST.Block)[:]
  union!(cx.mod.exports, names)
  return
end

function load_import(cx, x)
  path = x[4]::String
  mod = Tag(tag"common", pathtag(path).path...)
  @assert haskey(cx.comp.mods, mod)
  mod = cx.comp.mods[mod]
  import!(cx.mod, mod, x[2][:])
  return
end

function load_include(cx, x)
  path = "$common/$(x[2][1])"
  open(io -> loadfile(cx, io; path), path)
end

function load_expr(cx::LoadState, x; src)
  ir, defs = lower_toplevel(cx.mod, x; meta = FuncInfo(tag"common.core.main", src), resolve = x -> resolve_static(cx, x))
  foreach(x -> get!(cx.mod, x, ⊥), defs)
  method!(cx.mod, RMethod(tag"common.core.main", lowerpattern(AST.List()), ir))
  emit(ir)
end

isfn(x) = x[1] == :fn || ((x[1], x[2]) == (:extend, :fn))

function vload(cx::LoadState, x::AST.Syntax; src)
  x[1] == :include && return load_include(cx, x)
  x[1] == :export && return load_export(cx, x)
  x[1] == :import && return load_import(cx, x)
  x[1] == :bundle && return vload(cx, bundlemacro(x); src)
  isfn(x) || return load_expr(cx, x; src)
  extend = x[1] == :extend
  sig, body = extend ? x[3:end] : x[2:end]
  sig = AST.ungroup(sig)
  var = sig[1]::Symbol
  tag = extend ? resolve_static(cx, var)::Tag : Tag(cx.mod.name, var)
  cx.mod[var::Symbol] = tag
  resolve = x -> resolve_static(cx, x)
  sig = lowerpattern(AST.List(sig[2:end]...); mod = cx.mod.name, resolve)
  method!(cx.mod, RMethod(tag, sig,
                          lowerfn(cx.mod.name, sig, body; resolve,
                                  meta = FuncInfo(tag, AST.meta(x)))))
  return
end

vload(cx::LoadState, x::AST.Group; src) = foreach(x -> vload(cx, x; src), x[:])

function vload(cx::LoadState, x::AST.Operator; src)
  if x[1] == :(=) && x[2] isa Symbol && (c = simpleconst(cx, x[3])) != nothing
    cx.mod[x[2]] = c
  else
    load_expr(cx, x; src)
  end
end

vload(m::LoadState, x; src) = load_expr(m, x; src)

function loadfile(cx::LoadState, io::IO; path)
  io = LineNumberingReader(io)
  while true
    Parse.skip(io)
    cur = cursor(io)
    (ex = parse(io; path)) == nothing && break
    vload(cx, ex, src = Source(path, cur.line, cur.column))
  end
end

loadfile(cx::LoadState, path::String) =
  open(io -> loadfile(cx, io; path), path)

loadfile(cx::LoadState, src::SourceString) =
  loadfile(cx, IOBuffer(src.source), path = src.path)

function loadmodule(comp::Modules, mod::RModule, path)
  cx = LoadState(comp, mod)
  loadfile(cx, path)
  return mod
end

function loadmodule(comp::Modules, mod::Tag, path)
  return loadmodule(comp, module!(comp, mod), path)
end

function reload!(comp::Modules, src)
  main = module!(comp, tag"")
  empty!(main)
  common = comp[tag"common"]
  import!(main, common, common.exports)
  loadmodule(comp, main, src)
  return comp
end

function load(src)
  comp = Modules()
  module!(comp, core())
  loadmodule(comp, tag"common.core", "$common/core.rv")
  com = loadmodule(comp, tag"common", "$common/common.rv")
  reload!(comp, src)
  return comp
end
