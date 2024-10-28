using .WebAssembly: WType, WTuple, i32, i64, f32, f64

# WASM partial primitives
# These are supposed to be defined in Raven, but we don't yet have a mechanism
# for const prop, so this is a stopgap.

const wasmPartials = Dict{WebAssembly.Op,Any}()

wasmPartials[i64.add] = +
wasmPartials[i64.sub] = -
wasmPartials[i64.mul] = *

wasmPartials[i64.eq] = (a, b) -> Int32(a==b)
wasmPartials[i64.gt_s] = (a, b) -> Int32(a>b)
wasmPartials[i64.lt_s] = (a, b) -> Int32(a<b)
wasmPartials[i32.eqz] = x -> Int32(x==0)
wasmPartials[i32.wrap_i64] = Int32

rvtype(x::WType) = WebAssembly.jltype(x)
rvtype(x::WTuple) = pack(tag"common.List", map(rvtype, x.parts)...)
rvtype(::typeof(⊥)) = ⊥

struct WIntrinsic
  op
  ret
end

_typeof(x::WIntrinsic) = x

function intrinsic(ex)
  if ex isa AST.Operator && ex[1] == :(:)
    typ = ex[3]
    typ = typ == :unreachable ? ⊥ : WType(typ)
    ex = ex[2]
  else
    typ = WTuple()
  end
  op = AST.ungroup(ex[1])
  if op == :call
    WIntrinsic(WebAssembly.Call(Symbol(ex[2])), typ)
  elseif op == rvx"global.get"
    WIntrinsic(WebAssembly.GetGlobal(ex[2]::Integer), typ)
  else
    namify(x::Symbol) = x
    namify(x::AST.Field) = Symbol(join(namify.(x[:]), "."))
    WIntrinsic(WebAssembly.Op(namify(op)), typ)
  end
end

function intrinsic_args(ex)
  ex isa AST.Operator && ex[1] == :(:) && return intrinsic_args(ex[2])
  args = filter(x -> x isa AST.Operator && x[1] == :(:), ex[2:end])
  return map(x -> x[2], args)
end

function wlayout(x)
  l = layout(x)
  l isa Tuple ? WTuple(collect(WType.(l))) : WType(l)
end

function wparts(x)
  ly = wlayout(x)
  return ly isa WTuple ? ly.parts : [ly]
end

struct WGlobals
  types::Vector{WType}
  globals::Cache{Binding,Vector{Int}}
end

function WGlobals(types)
  gtypes = WType[]
  globals = Cache{Binding,Vector{Int}}() do self, b
    T = types[b]
    T isa Binding && return self[T]
    start = length(gtypes)-1
    l = wparts(T)
    append!(gtypes, l)
    collect(start .+ (1:length(l)))
  end
  return WGlobals(gtypes, globals)
end

Base.getindex(gs::WGlobals, b::Binding) = gs.globals[b]

Caches.subcaches(g::WGlobals) = (g.globals,)

function tableid!(xs, x)
  i = findfirst(==(x), xs)
  i === nothing || return Int32(i-1)
  push!(xs, x)
  return Int32(length(xs)-1)
end

struct Tables
  strings::Vector{String}
  funcs::Vector{Symbol}
end

Tables() = Tables(String[], Symbol[])

stringid!(w::Tables, s) = tableid!(w.strings, s)
funcid!(w::Tables, f) = tableid!(w.funcs, f)

function lowerwasm(ir::IR, names, globals, tables)
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    if !isexpr(st)
      pr[v] = stmt(st.expr, type = wlayout(st.type))
    elseif isexpr(st, :ref) && st.expr.args[1] isa String
      pr[v] = stringid!(tables, st.expr.args[1])
    elseif isexpr(st, :func)
      f, I, O = st.expr.args
      pr[v] = funcid!(tables, names[(f, I)])
    elseif isexpr(st, :tuple, :ref)
    elseif isexpr(st, :cast)
      @assert layout(st.type) == layout(exprtype(ir, st.expr.args[1]))
      pr[v] = st.expr.args[1]
    elseif isexpr(st, :global)
      l = globals[st.expr.args[1]::Binding]
      ps = [insert!(pr, v, stmt(Expr(:call, WebAssembly.GetGlobal(id)), type = T))
            for (id, T) in zip(l, wparts(st.type))]
      pr[v] = length(ps) == 1 ? only(ps) : Expr(:tuple, ps...)
    elseif isexpr(st, :call) && st.expr.args[1] isa WIntrinsic
      int = st.expr.args[1]
      ex = xcall(int.op, st.expr.args[2:end]...)
      pr[v] = stmt(st, expr = ex, type = int.ret == ⊥ ? WTuple() : int.ret)
      if int.ret == ⊥
        IRTools.push!(pr, stmt(xcall(WebAssembly.unreachable), type = WTuple()))
      end
    elseif isexpr(st, :call)
      Ts = (exprtype(ir, st.expr.args)...,)
      @assert !any(==(⊥), Ts)
      pr[v] = stmt(st,
                   expr = xcall(WebAssembly.Call(names[Ts]), st.expr.args[2:end]...),
                   type = wlayout(st.type))
    elseif isexpr(st, :call_indirect)
      f, args = st.expr.args
      I = wlayout(exprtype(ir, args)) |> WebAssembly.flattype
      O = wlayout(st.type) |> WebAssembly.flattype
      pr[v] = stmt(st,
                   expr = xcall(WebAssembly.CallIndirect(I => O, 0), args, f),
                   type = wlayout(st.type))
    elseif isexpr(st, :branch)
    elseif isexpr(st, :(=)) && (g = st.expr.args[1]) isa Binding
    else
      error("unrecognised $(st.expr.head) expression")
    end
  end
  ir = IRTools.finish(pr)
  for b in blocks(ir)
    IRTools.argtypes(b) .= wlayout.(IRTools.argtypes(b))
  end
  return ir
end

function lowerwasm_globals(ir::IR, types)
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    isexpr(st, :(=)) && (g = st.expr.args[1]) isa Binding || continue
    delete!(pr, v)
    l = types[g]
    for i in 1:length(l)
      p = st.expr.args[2]
      wlayout(st.type) isa WTuple &&
        (p = push!(pr, Expr(:ref, p, i)))
      push!(pr, stmt(xcall(WebAssembly.SetGlobal(l[i]), p), type = WTuple()))
    end
  end
  return IRTools.finish(pr)
end

struct Wasm
  globals::WGlobals
  names::DualCache{Any,Symbol}
  funcs::Cache{Any,WebAssembly.Func}
end

function Wasm(defs::Definitions, code, tables)
  globals = WGlobals(defs)
  count = Dict{Symbol,Int}()
  # TODO should be `funcs`, not `code`, to make global redefs of the same type
  # more efficient. But that creates an awkward cycle between names and funcs.
  names = DualCache{Any,Symbol}() do self, sig
    code[sig] # new name if code changes
    id = sig[1] isa Tag ? Symbol(sig[1]) : Symbol(Symbol(sig[1].name), ":method")
    c = count[id] = get(count, id, 0)+1
    return Symbol(id, ":", c)
  end
  strings = String[]
  table = Symbol[]
  funcs = Cache{Any,WebAssembly.Func}() do self, sig
    # TODO: we use `frame` to avoid redirects, but this can duplicate function
    # bodies. Should instead avoid calling redirected sigs, eg via casting.
    ir = lowerwasm(frame(code, sig), names, globals, tables)
    return WebAssembly.irfunc(names[sig], ir)
  end
  return Wasm(globals, names, funcs)
end

Base.getindex(w::Wasm, sig::Tuple) = w.funcs[sig]
Base.getindex(w::Wasm, name::Symbol) = w[Caches.getkey(w.names,name)]

Caches.subcaches(w::Wasm) = (w.globals, w.names, w.funcs)

Caches.reset!(w::Wasm; deps = []) =
  reset!(Pipeline([w.globals, w.names, w.funcs]); deps)

lowerwasm(ir, w::Wasm, t) = lowerwasm(ir, w.names, w.globals, t)

# Batch emitter, for AOT compilation

struct BatchEmitter
  main::Vector{Symbol}
  seen::Set{Symbol}
  tables::Tables
  funcs::Vector{WebAssembly.Func}
end

BatchEmitter() = BatchEmitter([], Set(), Tables(), [])

Base.copy(em::BatchEmitter) =
  BatchEmitter(copy(em.main), copy(em.seen), em.tables, copy(em.funcs))

tables(em::BatchEmitter) = em.tables

function _emit!(e::BatchEmitter, mod::Wasm, func::WebAssembly.Func)
  func.name in e.seen && return
  push!(e.seen, func.name)
  push!(e.funcs, func)
  for f in WebAssembly.callees(func)
    Caches.hasvalue(mod.names, f) || continue
    _emit!(e, mod, f)
  end
end

_emit!(e::BatchEmitter, mod::Wasm, f::Symbol) = _emit!(e, mod, mod[f])

function emit!(e::BatchEmitter, mod::Wasm, func::WebAssembly.Func)
  _emit!(e, mod, func)
  foreach(f -> _emit!(e, mod, f), e.tables.funcs)
  push!(e.main, func.name)
end

default_imports = [
  WebAssembly.Import(:support, :global, :jsglobal, [] => [i32]),
  WebAssembly.Import(:support, :property, :jsproperty, [i32, i32] => [i32]),
  WebAssembly.Import(:support, :call, :jscall0, [i32, i32] => [i32]),
  WebAssembly.Import(:support, :call, :jscall1, [i32, i32, i32] => [i32]),
  WebAssembly.Import(:support, :abort, :abort, [i32] => []),
  WebAssembly.Import(:support, :createRef, :jsbox, [f64] => [i32]),
  WebAssembly.Import(:support, :fromRef, :jsunbox, [i32] => [f64]),
  WebAssembly.Import(:support, :await, :jsawait, [i32] => [i32]),
  WebAssembly.Import(:support, :equal, :jseq, [i32, i32] => [i32]),
  WebAssembly.Import(:support, :release, :jsfree, [i32] => [])]

startfunc(main) =
  WebAssembly.Func(:_start, []=>[], [],
    WebAssembly.Block([WebAssembly.Call(m) for m in main]),
    FuncInfo(tag"common.core.main", trampoline = true))

function wasmmodule(em::BatchEmitter, globals::WGlobals)
  pushfirst!(em.funcs, startfunc(em.main))
  wmod = WebAssembly.Module(
    funcs = em.funcs,
    imports = default_imports,
    exports = [WebAssembly.Export(:_start, :_start)],
    globals = [WebAssembly.Global(t) for t in globals.types],
    tables = [WebAssembly.Table(length(em.tables.funcs))],
    elems = [WebAssembly.Elem(0, em.tables.funcs)],
    mems = [WebAssembly.Mem(0)])
  return wmod
end

pscmd(cmd) = Sys.iswindows() ? `powershell -command $cmd` : cmd

function tmp()
  # For some reason wat2wasm can't read the temp file on Windows.
  Sys.iswindows() ? tempname(".") : tempname()
end

function binary(m::WebAssembly.Module, file; path)
  open(file, "w") do io
    WebAssembly.binary(io, m; path)
  end
  return
end

base64(m::WebAssembly.Module; path) = base64encode(io -> WebAssembly.binary(io, m; path))

function emitwasm(em::BatchEmitter, mod::Wasm, out; path)
  binary(wasmmodule(em, mod.globals), out; path)
  return em.tables.strings
end

# JS support

const support = joinpath(@__DIR__, "support.js")

jsstring(s) = Base.string('"', escape_string(s), '"')

function emitjs(path, wasm, strings)
  open(path, "w") do io
    println(io, "// This file contains generated code.\n")
    write(io, Base.read(support))
    println(io)
    println(io, "const wasmFile = '$wasm';")
    println(io, "[$(join(jsstring.(strings), ", "))].forEach(registerString)")
    if options().jsalloc
      println(io, "main();")
    else
      println(io, "main({memcheck: false});")
    end
  end
end

# Stream emitter, for REPL

mutable struct StreamEmitter
  seen::Set{Symbol}
  queue::Vector{WebAssembly.Module}
  tables
  globals::Int
end

StreamEmitter(tables = Tables()) = StreamEmitter(Set(), [], tables, -1)

Base.copy(em::StreamEmitter) = StreamEmitter(copy(em.seen), copy(em.queue), em.tables, em.globals)

tables(em::StreamEmitter) = em.tables

function _emit!(e::StreamEmitter, mod::Wasm, func::WebAssembly.Func, fs, imports)
  push!(imports, func.name)
  func.name in e.seen && return
  push!(e.seen, func.name)
  push!(fs, func)
  for f in WebAssembly.callees(func)
    Caches.hasvalue(mod.names, f) || continue
    _emit!(e, mod, f, fs, imports)
  end
  return fs, imports
end

_emit!(e::StreamEmitter, mod::Wasm, f::Symbol, fs, imports) = _emit!(e, mod, mod[f], fs, imports)

function emit!(e::StreamEmitter, mod::Wasm, func::WebAssembly.Func)
  first = e.globals == -1
  first && (e.globals = 0)
  fs, imports = _emit!(e, mod, func, WebAssembly.Func[], Symbol[])
  foreach(f -> _emit!(e, mod, f, fs, imports), tables(e).funcs)
  pushfirst!(fs, startfunc([func.name]))
  imports = filter(x -> !any(f -> f.name == x, fs), unique(imports))
  imports = [WebAssembly.Import(:wasm, f, f, mod[f].sig) for f in imports]
  exports = [WebAssembly.Export(f.name, f.name) for f in fs]
  gimports = [WebAssembly.Import(:wasm, Symbol(:global, i-1), WebAssembly.Global(mod.globals.types[i])) for i = 1:e.globals]
  globals = [WebAssembly.Global(mod.globals.types[i], name = Symbol(:global, i-1)) for i in e.globals+1:length(mod.globals.types)]
  first || push!(imports, WebAssembly.Import(:wasm, :memory, WebAssembly.Mem(0)))
  # TODO shared table
  wmod = WebAssembly.Module(
    funcs = fs,
    imports = vcat(default_imports, imports, gimports),
    exports = [WebAssembly.Export(f.name, f.name) for f in fs],
    globals = globals,
    tables = [WebAssembly.Table(length(e.tables.funcs))],
    elems = [WebAssembly.Elem(0, copy(e.tables.funcs))],
    mems = first ? [WebAssembly.Mem(0, name = :memory)] : [])
  push!(e.queue, wmod)
  e.globals = length(mod.globals.types)
end
