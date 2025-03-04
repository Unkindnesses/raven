using .WebAssembly: WType, WTuple, i32, i64, f32, f64

# WASM partial primitives
# These are supposed to be defined in Raven, but we don't yet have a mechanism
# for const prop, so this is a stopgap.

const wasmPartials = Dict{WebAssembly.Op,Any}()

wasmPartials[i64.add] = (a, b) -> Bits{64}(a + b)
wasmPartials[i64.sub] = (a, b) -> Bits{64}(a - b)
wasmPartials[i64.mul] = (a, b) -> Bits{64}(a * b)

wasmPartials[i64.eq] = (a, b) -> Bits{32}(a==b)
wasmPartials[i64.gt_s] = (a, b) -> Bits{32}(a>b)
wasmPartials[i64.lt_s] = (a, b) -> Bits{32}(a<b)
wasmPartials[i64.le_s] = (a, b) -> Bits{32}(a<=b)

rvtype(x::WType) = Dict(i32 => Bits{32}, i64 => Bits{64}, f32 => Float32, f64 => Float64)[x] |> RType
rvtype(x::WTuple) = pack(tag"common.List", map(rvtype, x.parts)...)
rvtype(::typeof(⊥)) = ⊥

_typeof(x::WebAssembly.Instruction) = x

struct WImport <: WebAssembly.Instruction
  mod::Symbol
  name::Symbol
end

function intrinsic(ex)
  if ex isa AST.Operator && ex[1] == :(:)
    typ = ex[3]
    typ =
      typ == :unreachable ? ⊥ :
      typ isa AST.Group ? rlist(rvtype.(WType.(typ[:]))...) :
      rvtype(WType(typ))
    ex = ex[2]
  else
    typ = nil
  end
  op = AST.ungroup(ex[1])
  if op == :call
    @assert ex[2] isa AST.Field
    return WImport(Symbol.(ex[2][:])...), typ
  elseif op == rvx"global.get"
    WebAssembly.GetGlobal(ex[2]::Integer), typ
  else
    namify(x::Symbol) = x
    namify(x::AST.Field) = Symbol(join(namify.(x[:]), "."))
    WebAssembly.Op(namify(op)), typ
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

wlayout(::Unreachable) = WTuple()

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
  globals = Cache{Binding,Vector{Int}}() do T
    while T isa Binding
      T = types[T]
    end
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
      name = names[(WImport(:support, :string), (i32,), (i32,))]
      pr[v] = stmt(Expr(:call, WebAssembly.Call(name), stringid!(tables, st.expr.args[1])), type = i32)
    elseif isexpr(st, :func)
      f, I, O = st.expr.args
      pr[v] = funcid!(tables, names[(f, I)])
    elseif isexpr(st, :tuple, :ref)
    elseif isexpr(st, :cast)
      @assert tlayout(st.type) == tlayout(exprtype(ir, st.expr.args[1]))
      pr[v] = st.expr.args[1]
    elseif isexpr(st, :global)
      l = globals[st.expr.args[1]::Binding]
      ps = [insert!(pr, v, stmt(Expr(:call, WebAssembly.GetGlobal(id)), type = T))
            for (id, T) in zip(l, wparts(st.type))]
      pr[v] = length(ps) == 1 ? only(ps) : Expr(:tuple, ps...)
    elseif isexpr(st, :call) && st.expr.args[1] isa WebAssembly.Instruction
      if st.expr.args[1] isa WImport
        args = st.expr.args[2:end]
        name = names[(st.expr.args[1],
                      Tuple(WType.(atom.(abstract.(exprtype(ir, args))))),
                      Tuple(wparts(st.type)))]
        st = stmt(st, expr = xcall(WebAssembly.Call(name), args...))
      end
      T = st.type
      pr[v] = stmt(st, type = T == ⊥ ? WTuple() : wlayout(T))
      if T == ⊥
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
  tables::Tables
  names::DualCache{Any,Symbol}
  funcs::Cache{Any,WebAssembly.Func}
end

wname(x::Tag) = Symbol(x)
wname(x::RType) = wname(atom(x)::Tag)
wname(x::RMethod) = Symbol(Symbol(x.name), ":method")
wname(x::WImport) = Symbol(x.mod, ":", x.name)

function Wasm(defs, code)
  globals = WGlobals(defs)
  tables = Tables()
  count = Dict{Symbol,Int}()
  # TODO should be `funcs`, not `code`, to make global redefs of the same type
  # more efficient. But that creates an awkward cycle between names and funcs.
  names = DualCache{Any,Symbol}() do sig
    sig[1] isa WImport || code[sig] # new name if code changes
    id = wname(sig[1])
    c = count[id] = get(count, id, 0)+1
    return Symbol(id, ":", c)
  end
  funcs = Cache{Any,WebAssembly.Func}() do sig
    # TODO: we use `frame` to avoid redirects, but this can duplicate function
    # bodies. Should instead avoid calling redirected sigs, eg via casting.
    ir = lowerwasm(frame(code, sig), names, globals, tables)
    return WebAssembly.irfunc(names[sig], ir)
  end
  return Wasm(globals, tables, names, funcs)
end

Base.getindex(w::Wasm, sig::Tuple) = w.funcs[sig]
Base.getindex(w::Wasm, name::Symbol) = w[Caches.getkey(w.names, name)]
Base.haskey(w::Wasm, name::Symbol) = Caches.hasvalue(w.names, name) && !(Caches.getkey(w.names, name)[1] isa WImport)

Caches.subcaches(w::Wasm) = (w.globals, w.names, w.funcs)

Caches.reset!(w::Wasm; deps = []) =
  reset!(Pipeline([w.globals, w.names, w.funcs]); deps)

lowerwasm(ir, w::Wasm) = lowerwasm(ir, w.names, w.globals, w.tables)

# Batch emitter, for AOT compilation

struct BatchEmitter
  main::Vector{Symbol}
  seen::Set{Symbol}
  funcs::Vector{WebAssembly.Func}
  imports::Vector{WebAssembly.Import}
end

BatchEmitter() = BatchEmitter([], Set(), [], [])

Base.copy(em::BatchEmitter) =
  BatchEmitter(copy(em.main), copy(em.seen), copy(em.funcs), copy(em.imports))

function _emit!(e::BatchEmitter, mod::Wasm, func::WebAssembly.Func)
  push!(e.funcs, func)
  for f in WebAssembly.callees(func)
    _emit!(e, mod, f)
  end
end

function _emit!(e::BatchEmitter, mod::Wasm, f::Symbol)
  f in e.seen && return
  push!(e.seen, f)
  if haskey(mod, f)
    _emit!(e, mod, mod[f])
  elseif Caches.hasvalue(mod.names, f)
    imp, I, O = Caches.getkey(mod.names, f)
    push!(e.imports, WebAssembly.Import(imp.mod, imp.name, f, I => O))
  end
end

function emit!(e::BatchEmitter, mod::Wasm, func::WebAssembly.Func)
  _emit!(e, mod, func)
  foreach(f -> _emit!(e, mod, f), mod.tables.funcs)
  push!(e.main, func.name)
end

startfunc(main) =
  WebAssembly.Func(:_start, []=>[i32], [],
    WebAssembly.Block([[WebAssembly.Call(m) for m in main]..., WebAssembly.Const(Int32(0))]),
    FuncInfo(tag"common.core.main", trampoline = true))

function wasmmodule(em::BatchEmitter, globals::WGlobals, tables::Tables)
  pushfirst!(em.funcs, startfunc(em.main))
  wmod = WebAssembly.Module(
    funcs = em.funcs,
    imports = em.imports,
    exports = [WebAssembly.Export(:_start, :_start),
               WebAssembly.Export(Symbol("cm32p2|wasi:cli/run@0.2|run"), :_start)],
    globals = [WebAssembly.Global(t) for t in globals.types],
    tables = [WebAssembly.Table(length(tables.funcs))],
    elems = [WebAssembly.Elem(0, tables.funcs)],
    mems = [WebAssembly.Mem(0, name = :cm32p2_memory)])
  return wmod
end

function binary(m::WebAssembly.Module, file; path)
  open(file, "w") do io
    WebAssembly.binary(io, m; path)
  end
  return
end

base64(m::WebAssembly.Module; path) = base64encode(io -> WebAssembly.binary(io, m; path))

function emitwasm(em::BatchEmitter, mod::Wasm, out; path)
  binary(wasmmodule(em, mod.globals, mod.tables), out; path)
  return mod.tables.strings
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
    println(io, "support.strings = [$(join(jsstring.(strings), ", "))]")
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
  globals::Int
end

StreamEmitter() = StreamEmitter(Set(), [], -1)

function _emit!(e::StreamEmitter, mod::Wasm, func::WebAssembly.Func, fs, imports)
  push!(fs, func)
  for f in WebAssembly.callees(func)
    _emit!(e, mod, f, fs, imports)
  end
  return fs, imports
end

function _emit!(e::StreamEmitter, mod::Wasm, f::Symbol, fs, imports)
  push!(imports, f)
  f in e.seen && return
  push!(e.seen, f)
  if haskey(mod, f)
    _emit!(e, mod, mod[f], fs, imports)
  else
    push!(imports, f)
  end
end

function wimport(mod::Wasm, f::Symbol)
  sig = Caches.getkey(mod.names, f)
  if sig[1] isa WImport
    imp, I, O = sig
    WebAssembly.Import(imp.mod, imp.name, f, I => O)
  else
    WebAssembly.Import(:wasm, f, f, mod[f].sig)
  end
end

function emit!(e::StreamEmitter, mod::Wasm, func::WebAssembly.Func)
  first = e.globals == -1
  first && (e.globals = 0)
  fs, imports = _emit!(e, mod, func, WebAssembly.Func[], Symbol[])
  foreach(f -> _emit!(e, mod, f, fs, imports), mod.tables.funcs)
  pushfirst!(fs, startfunc([func.name]))
  imports = setdiff(imports, map(f -> f.name, fs))
  imports = [wimport(mod, f) for f in imports]
  gimports = [WebAssembly.Import(:wasm, Symbol(:global, i-1), WebAssembly.Global(mod.globals.types[i])) for i = 1:e.globals]
  globals = [WebAssembly.Global(mod.globals.types[i], name = Symbol(:global, i-1)) for i in e.globals+1:length(mod.globals.types)]
  first || push!(imports, WebAssembly.Import(:wasm, :memory, WebAssembly.Mem(0)))
  # TODO shared table
  wmod = WebAssembly.Module(
    funcs = fs,
    imports = vcat(gimports, imports),
    exports = [WebAssembly.Export(f.name, f.name) for f in fs],
    globals = globals,
    tables = [WebAssembly.Table(length(mod.tables.funcs))],
    elems = [WebAssembly.Elem(0, copy(mod.tables.funcs))],
    mems = first ? [WebAssembly.Mem(0, name = :memory)] : [])
  push!(e.queue, wmod)
  e.globals = length(mod.globals.types)
end
