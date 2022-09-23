using WebAssembly: WType, WTuple, i32, i64, f32, f64

# WASM partial primitives
# These are supposed to be defined in Raven, but we don't yet have a mechanism
# for const prop, so this is a stopgap.

const wasmPartials = Dict{WebAssembly.Op,Any}()

wasmPartials[i64.add] = +

rvtype(x::WType) = WebAssembly.jltype(x)
rvtype(x::WTuple) = pack(:List, map(rvtype, x.parts)...)
rvtype(::typeof(⊥)) = ⊥

struct WIntrinsic
  op
  ret
end

function intrinsic(ex)
  if ex isa AST.Operator && ex.op == :(:)
    typ = ex.args[2] == :unreachable ? ⊥ : WType(ex.args[2])
    ex = ex.args[1]
  else
    typ = WTuple()
  end
  op = ex.func
  if op == :call
    WIntrinsic(WebAssembly.Call(ex.args[1].expr), typ)
  else
    namify(x::Symbol) = x
    namify(x::AST.Operator) = Symbol(join(namify.(x.args), x.op))
    WIntrinsic(WebAssembly.Op(namify(op)), typ)
  end
end

function intrinsic_args(ex)
  ex isa AST.Operator && ex.op == :(:) && return intrinsic_args(ex.args[1])
  args = filter(x -> x isa AST.Operator && x.op == :(:), ex.args)
  return map(x -> x.args[1], args)
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
  inf::Compilation
  symbols::Dict{Symbol,Int}
  strings::Vector{String}
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

function global!(mod::WModule, g::Global, T)
  get!(mod.globals, g) do
    start = sum([length(gs) for gs in values(mod.globals)])
    l = wparts(T)
    append!(mod.gtypes, l)
    collect(start:start+length(l)-1)
  end
end

WModule(inf) = WModule(inf, Dict(), [], Dict(), Dict(), [])

function sigs!(mod::RModule, ir::IR)
  for (v, st) in ir
    if isexpr(st.expr, :call)
      if st.expr.args[1] isa WIntrinsic
      elseif st.expr.args[1] isa RMethod
        ir[v] = xcall((exprtype(mod, ir, st.expr.args)...,), st.expr.args...)
      else
        f, xs = exprtype(mod, ir, st.expr.args)
        ir[v] = xcall((f, xs), st.expr.args...)
      end
    end
  end
  return ir
end

function lowerwasm!(mod::WModule, ir::IR)
  sigs!(mod.inf.mod, ir)
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    if !isexpr(st.expr)
      pr[v] = stmt(st.expr, type = wlayout(st.type))
      continue
    elseif isexpr(st.expr, :ref) && st.expr.args[1] isa String
      pr[v] = stringid!(mod, st.expr.args[1])
      continue
    elseif isexpr(st.expr, :tuple, :ref)
      continue
    elseif isexpr(st.expr, :cast)
      @assert layout(st.type) == layout(exprtype(mod, ir, st.expr.args[1]))
      pr[v] = st.expr.args[1]
      continue
    elseif isexpr(st.expr, :global)
      g = Global(st.expr.args[1])
      l = global!(mod, g, st.type)
      pr[v] = Expr(:tuple, [WebAssembly.GetGlobal(id) for id in l]...)
      continue
    elseif isexpr(st.expr, :(=)) && (g = st.expr.args[1]) isa Global
      delete!(pr, v)
      l = global!(mod, g, st.type)
      for i in 1:length(l)
        p = st.expr.args[2]
        wlayout(st.type) isa WTuple &&
          (p = push!(pr, Expr(:ref, p, i)))
        push!(pr, stmt(xcall(WebAssembly.SetGlobal(l[i]), p), type = WTuple()))
      end
      continue
    elseif !isexpr(st.expr, :call)
      error("unrecognised $(st.expr.head) expression")
    end
    Ts, args = st.expr.args[1], st.expr.args[2:end]
    if Ts isa WIntrinsic
      ex = xcall(st.expr.args[1].op, st.expr.args[2:end]...)
      pr[v] = stmt(st.expr, expr = ex, type = Ts.ret == ⊥ ? WTuple() : Ts.ret)
      if Ts.ret == ⊥
        IRTools.push!(pr, stmt(xcall(WebAssembly.unreachable), type = WTuple()))
      end
    elseif any(x -> x == ⊥, Ts)
      pr[v] = stmt(xcall(WebAssembly.unreachable), type = WTuple())
    else
      func = lowerwasm!(mod, Ts)
      pr[v] = stmt(xcall(WebAssembly.Call(func), args[2:end]...), type = wlayout(ir[v].type))
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
  ir = lowerwasm!(mod, mod.inf.frames[T])
  mod.funcs[T] = (id, ir)
  return id
end

default_imports = [
  WebAssembly.Import(:support, :global, :jsglobal, :func, [], [i32]),
  WebAssembly.Import(:support, :property, :jsproperty, :func, [i32, i32], [i32]),
  WebAssembly.Import(:support, :call, :jscall0, :func, [i32, i32], [i32]),
  WebAssembly.Import(:support, :call, :jscall1, :func, [i32, i32, i32], [i32]),
  WebAssembly.Import(:support, :panic, :panic, :func, [i32], []),
  WebAssembly.Import(:support, :createRef, :jsbox, :func, [f64], [i32]),
  WebAssembly.Import(:support, :fromRef, :jsunbox, :func, [i32], [f64]),
  WebAssembly.Import(:support, :equal, :jseq, :func, [i32, i32], [i32])]

function wasm_ir(inf::Compilation)
  mod = WModule(inf)
  lowerwasm!(mod, (startmethod(inf.mod),))
  return mod
end

function wasmmodule(inf::Compilation)
  mod = wasm_ir(inf)
  strings = mod.strings
  fs = [WebAssembly.irfunc(name, ir) for (name, ir) in values(mod.funcs)]
  sort!(fs, by = f -> f.name)
  mod = WebAssembly.Module(
    funcs = fs,
    imports = default_imports,
    exports = [WebAssembly.Export(:_start, Symbol("_start:method:1"), :func)],
    globals = [WebAssembly.Global(t) for t in mod.gtypes],
    mems = [WebAssembly.Mem(0)])
  return mod, strings
end

function runps(cmd)
  Sys.iswindows() ? run(`powershell -command $cmd`) : run(cmd)
end

function tmp(file)
  # For some reason wat2wasm can't read the temp file on Windows.
  Sys.iswindows() ? file : tempname()
end

function binary(m::WebAssembly.Module, file; optimise = true)
  wat = tmp(file) * ".wat"
  WebAssembly.write_wat(wat, m)
  try
    runps(`wat2wasm $wat -o $file`)
    optimise && runps(`wasm-opt --enable-multivalue --enable-bulk-memory $file -O4 -o $file`)
  finally
    rm(wat)
  end
  return
end

function emitwasm(file, out; optimise = true)
  mod, strings = wasmmodule(lowerir(loadfile(file)))
  binary(mod, out; optimise)
  return strings
end
