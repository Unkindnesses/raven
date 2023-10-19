using .WebAssembly: WType, WTuple, i32, i64, f32, f64, externref

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

rvtype(x::WType) = WebAssembly.jltype(x)
rvtype(x::WTuple) = pack(tag"common.List", map(rvtype, x.parts)...)
rvtype(::typeof(⊥)) = ⊥

struct WIntrinsic
  op
  ret
end

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

function WGlobals(defs::Definitions, types)
  gtypes = WType[]
  globals = Cache{Binding,Vector{Int}}() do ch, b
    b′ = defs[b]
    b′ isa Binding && (b′ == b || return ch[b′])
    start = length(gtypes)
    l = wparts(types[b])
    append!(gtypes, l)
    collect(start .+ (1:length(l)))
  end
  return WGlobals(gtypes, globals)
end

Base.getindex(gs::WGlobals, b::Binding) = gs.globals[b]

struct WEnv
  globals::WGlobals
  strings::Vector{String}
  table::Vector{Symbol}
end

WEnv(gs::WGlobals) = WEnv(gs, String[], Symbol[])

function tableid!(xs, x)
  i = findfirst(==(x), xs)
  i === nothing || return Int32(i-1)
  push!(xs, x)
  return Int32(length(xs)-1)
end

function lowerwasm(ir::IR, names, env)
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    if !isexpr(st)
      pr[v] = stmt(st.expr, type = wlayout(st.type))
    elseif isexpr(st, :ref) && st.expr.args[1] isa String
      pr[v] = tableid!(env.strings, st.expr.args[1])
    elseif isexpr(st, :func)
      f, I, O = st.expr.args
      pr[v] = tableid!(env.table, names[(f, I)])
    elseif isexpr(st, :tuple, :ref)
    elseif isexpr(st, :cast)
      @assert layout(st.type) == layout(exprtype(ir, st.expr.args[1]))
      pr[v] = st.expr.args[1]
    elseif isexpr(st, :global)
      l = env.globals[st.expr.args[1]::Binding]
      pr[v] = Expr(:tuple, [WebAssembly.GetGlobal(id) for id in l]...)
    elseif isexpr(st, :(=)) && (g = st.expr.args[1]) isa Global
      delete!(pr, v)
      l = env.globals[g.name]
      for i in 1:length(l)
        p = st.expr.args[2]
        wlayout(st.type) isa WTuple &&
          (p = push!(pr, Expr(:ref, p, i)))
        push!(pr, stmt(xcall(WebAssembly.SetGlobal(l[i]), p), type = WTuple()))
      end
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
      I = wlayout(exprtype(ir, args)) |> WebAssembly.flattentype
      O = wlayout(st.type) |> WebAssembly.flattentype
      pr[v] = stmt(st,
                   expr = xcall(WebAssembly.CallIndirect(I => O, 0), args, f),
                   type = wlayout(st.type))
    elseif isexpr(st, :branch)
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

struct WModule
  env::WEnv
  names::DualCache{Any,Symbol}
  funcs::Cache{Any,WebAssembly.Func}
end

function WModule(c::Compiler)
  env = WEnv(WGlobals(c.defs, c.inf))
  count = Dict{Symbol,Int}()
  names = DualCache{Any,Symbol}() do ch, sig
    id = sig[1] isa Tag ? Symbol(sig[1]) : Symbol(Symbol(sig[1].name), ":method")
    local c = count[id] = get(count, id, 0)+1
    return Symbol(id, ":", c)
  end
  strings = String[]
  table = Symbol[]
  funcs = Cache{Any,WebAssembly.Func}() do ch, sig
    # TODO: we use `frame` to avoid redirects, but this can duplicate function
    # bodies. Should instead avoid calling redirected sigs, eg via casting.
    ir = lowerwasm(frame(c, sig), names, env)
    return WebAssembly.irfunc(names[sig], ir)
  end
  return WModule(env, names, funcs)
end

default_imports = [
  WebAssembly.Import(:support, :global, :jsglobal, [] => [i32]),
  WebAssembly.Import(:support, :property, :jsproperty, [i32, i32] => [i32]),
  WebAssembly.Import(:support, :call, :jscall0, [i32, i32] => [i32]),
  WebAssembly.Import(:support, :call, :jscall1, [i32, i32, i32] => [i32]),
  WebAssembly.Import(:support, :panic, :panic, [i32] => []),
  WebAssembly.Import(:support, :createRef, :jsbox, [f64] => [i32]),
  WebAssembly.Import(:support, :fromRef, :jsunbox, [i32] => [f64]),
  WebAssembly.Import(:support, :await, :jsawait, [externref, i32] => [i32]),
  WebAssembly.Import(:support, :equal, :jseq, [i32, i32] => [i32]),
  WebAssembly.Import(:support, :release, :jsfree, [i32] => [])]

function appendfunc!(funcs, func::WebAssembly.Func, mod, seen)
  func.name in seen && return
  push!(seen, func.name)
  push!(funcs, func)
  for f in WebAssembly.callees(func)
    Caches.hasvalue(mod.names, f) || continue
    appendfunc!(funcs, f, mod, seen)
  end
end

appendfunc!(funcs, f::Symbol, mod, seen) =
  appendfunc!(funcs, mod.funcs[Caches.getkey(mod.names, f)], mod, seen)

function wasmmodule(mod::WModule, defs)
  main = [mod.names[(m,)] for m in defs[tag"common.core.main"]]
  options().memcheck && push!(main, mod.names[(defs[tag"common.checkAllocations"][1],)])
  funcs = WebAssembly.Func[]
  start = WebAssembly.Func(:_start, [externref]=>[], [],
    WebAssembly.Block([
      WebAssembly.Local(0),
      WebAssembly.SetGlobal(0),
      [WebAssembly.Call(m) for m in main]...
      ]),
    FuncInfo(tag"common.core.main", trampoline = true))
  done = Set{Symbol}()
  appendfunc!(funcs, start, mod, done)
  foreach(f -> appendfunc!(funcs, f, mod, done), mod.env.table)
  wmod = WebAssembly.Module(
    funcs = funcs,
    imports = default_imports,
    exports = [WebAssembly.Export(:_start, :_start)],
    globals = [WebAssembly.Global(externref), [WebAssembly.Global(t) for t in mod.env.globals.types]...],
    tables = [WebAssembly.Table(length(mod.env.table))],
    elems = [WebAssembly.Elem(0, mod.env.table)],
    mems = [WebAssembly.Mem(0)])
  return wmod, mod.env.strings
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

function emitwasm(c::Compiler, out; path)
  mod = WModule(c)
  mod, strings = wasmmodule(mod, c.defs)
  binary(mod, out; path)
  return strings
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
    println(io, "registerStrings([$(join(jsstring.(strings), ", "))])")
    if options().jsalloc
      println(io, "main();")
    else
      println(io, "main({memcheck: false});")
    end
  end
end
