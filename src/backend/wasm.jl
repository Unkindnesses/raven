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

rvtype(x::WType) = WebAssembly.jltype(x)
rvtype(x::WTuple) = pack(:List, map(rvtype, x.parts)...)
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
  op = ex[1]
  if op == :call
    WIntrinsic(WebAssembly.Call(ex[2][1]), typ)
  else
    namify(x::Symbol) = x
    namify(x::AST.Operator) = Symbol(join(namify.(x[2:end]), x[1]))
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

struct WModule
  inf::Cache
  symbols::Dict{Symbol,Int}
  strings::Vector{String}
  table::Vector{Symbol}
  funcs::IdDict{Any,Tuple{Symbol,Union{IR,Nothing}}}
  globals::Dict{Global,Vector{Int}}
  gtypes::Vector{WType}
end

function name(mod::WModule, s::Symbol)
  s == :_start && return s
  c = mod.symbols[s] = get(mod.symbols, s, 0)+1
  return Symbol(s, ":", c)
end

function stringid!(mod::WModule, s::String)
  i = findfirst(==(s), mod.strings)
  i === nothing || return Int32(i-1)
  push!(mod.strings, s)
  return Int32(length(mod.strings)-1)
end

function funcid!(mod::WModule, f, I, O)
  name = lowerwasm!(mod, (f, I))
  i = findfirst(s -> s === name, mod.table)
  i === nothing || return Int32(i-1)
  push!(mod.table, name)
  return Int32(length(mod.table)-1)
end

function global!(mod::WModule, g::Global, T)
  get!(mod.globals, g) do
    start = sum([length(gs) for gs in values(mod.globals)])
    l = wparts(T)
    append!(mod.gtypes, l)
    collect(start:start+length(l)-1)
  end
end

WModule(inf) = WModule(inf, Dict(), [], [], Dict(), Dict(), [])

function lowerwasm!(mod::WModule, ir::IR)
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    if !isexpr(st)
      pr[v] = stmt(st.expr, type = wlayout(st.type))
    elseif isexpr(st, :ref) && st.expr.args[1] isa String
      pr[v] = stringid!(mod, st.expr.args[1])
    elseif isexpr(st, :func)
      pr[v] = funcid!(mod, st.expr.args...)
    elseif isexpr(st, :tuple, :ref)
    elseif isexpr(st, :cast)
      @assert layout(st.type) == layout(exprtype(ir, st.expr.args[1]))
      pr[v] = st.expr.args[1]
    elseif isexpr(st, :global)
      g = Global(st.expr.args[1])
      l = global!(mod, g, st.type)
      pr[v] = Expr(:tuple, [WebAssembly.GetGlobal(id) for id in l]...)
    elseif isexpr(st, :(=)) && (g = st.expr.args[1]) isa Global
      delete!(pr, v)
      l = global!(mod, g, st.type)
      for i in 1:length(l)
        p = st.expr.args[2]
        wlayout(st.type) isa WTuple &&
          (p = push!(pr, Expr(:ref, p, i)))
        push!(pr, stmt(xcall(WebAssembly.SetGlobal(l[i]), p), type = WTuple()))
      end
    elseif isexpr(st, :call) && st.expr.args[1] isa WIntrinsic
      int = st.expr.args[1]
      ex = xcall(int.op, st.expr.args[2:end]...)
      pr[v] = stmt(st.expr, expr = ex, type = int.ret == ⊥ ? WTuple() : int.ret)
      if int.ret == ⊥
        IRTools.push!(pr, stmt(xcall(WebAssembly.unreachable), type = WTuple()))
      end
    elseif isexpr(st, :call)
      Ts = (exprtype(ir, st.expr.args)...,)
      @assert !any(==(⊥), Ts)
      func = lowerwasm!(mod, Ts)
      pr[v] = stmt(st,
                   expr = xcall(WebAssembly.Call(func), st.expr.args[2:end]...),
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

function lowerwasm!(mod::WModule, T)
  haskey(mod.funcs, T) && return mod.funcs[T][1]
  f = T[1]::Union{Symbol,RMethod}
  id = name(mod, f isa Symbol ? f : Symbol(f.name, ":method"))
  mod.funcs[T] = (id, nothing)
  ir = lowerwasm!(mod, frame(mod.inf, T))
  mod.funcs[T] = (id, ir)
  return id
end

default_imports = [
  WebAssembly.Import(:support, :global, :jsglobal, [] => [i32]),
  WebAssembly.Import(:support, :property, :jsproperty, [i32, i32] => [i32]),
  WebAssembly.Import(:support, :call, :jscall0, [i32, i32] => [i32]),
  WebAssembly.Import(:support, :call, :jscall1, [i32, i32, i32] => [i32]),
  WebAssembly.Import(:support, :panic, :panic, [i32] => []),
  WebAssembly.Import(:support, :createRef, :jsbox, [f64] => [i32]),
  WebAssembly.Import(:support, :fromRef, :jsunbox, [i32] => [f64]),
  WebAssembly.Import(:support, :equal, :jseq, [i32, i32] => [i32])]

function wasm_ir(inf::Cache, start)
  mod = WModule(inf)
  lowerwasm!(mod, (start,))
  return mod
end

function wasmmodule(inf::Cache, start)
  mod = wasm_ir(inf, start)
  strings = mod.strings
  fs = [WebAssembly.irfunc(name, ir) for (name, ir) in values(mod.funcs)]
  sort!(fs, by = f -> f.name)
  mod = WebAssembly.Module(
    funcs = fs,
    imports = default_imports,
    exports = [WebAssembly.Export(:_start, Symbol("_start:method:1"))],
    globals = [WebAssembly.Global(t) for t in mod.gtypes],
    tables = [WebAssembly.Table(length(mod.table))],
    elems = [WebAssembly.Elem(0, mod.table)],
    mems = [WebAssembly.Mem(0)])
  return mod, strings
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

function emitwasm(file, out)
  mod = loadfile(file)
  comp = mod |> infer |> lowerir |> refcounts
  mod, strings = wasmmodule(comp, startmethod(mod))
  binary(mod, out; path = normpath(joinpath(pwd(), file)))
  return strings
end
