using WebAssembly: WType, WTuple, i32, i64, f32, f64

rvtype(x::WType) = WebAssembly.jltype(x)
rvtype(x::WTuple) = data(:Tuple, map(rvtype, x.parts)...)

struct WIntrinsic
  op
  ret
end

function intrinsic(ex)
  if ex isa Operator && ex.op == :(:)
    typ = WType(ex.args[2])
    ex = ex.args[1]
  else
    typ = WebAssembly.WTuple()
  end
  op = ex.func
  if op == :call
    WIntrinsic(WebAssembly.Call(ex.args[1].expr), typ)
  else
    namify(x::Symbol) = x
    namify(x::Raven.Operator) = Symbol(join(namify.(x.args), x.op))
    WIntrinsic(WebAssembly.Op(namify(op)), typ)
  end
end

function intrinsic_args(ex)
  ex isa Operator && ex.op == :(:) && return intrinsic_args(ex.args[1])
  args = filter(x -> x isa Operator && x.op == :(:), ex.args)
  return map(x -> Call(:widen, [x.args[1]]), args)
end

WNum = Union{Int32,Int64,Float32,Float64}

function cat_layout(xs...; result = [])
  for x in xs
    x isa WTuple ? cat_layout(x.parts..., result = result) : push!(result, x)
  end
  return length(result) == 1 ? result[1] : WTuple(result)
end

layout(::Type{T}) where T = WebAssembly.WType(T)
layout(x::Union{Primitive,Quote}) = WTuple()
layout(x::Data) = cat_layout(layout.(x.parts)...)

nregisters(l::WType) = 1
nregisters(l::WTuple) = length(l.parts)

function sublayout(T, i)
  before = data(T.parts[1:i]...)
  offset = nregisters(layout(before))
  length = nregisters(layout(T.parts[i+1]))
  offset .+ (1:length)
end

function wparts(x)
  ly = layout(x)
  return ly isa WTuple ? ly.parts : [ly]
end

# Create a `part` method to dynamically index tuples allocated as registers.
# TODO: should make sure this comes out as a switch / branch table.
function partir(x, i)
  i <: Int64 || error("Only i64 indexes are supported.")
  T = partial_part(x, i)
  ir = IR()
  _ = argument!(ir, type = WTuple())
  vx = argument!(ir, type = layout(x))
  vi = argument!(ir, type = layout(i))
  xlayout = layout(x)
  part(i) = xlayout isa WTuple ? push!(ir, Base.Expr(:ref, vx, i)) : vx
  for i = 1:nparts(x)
    cond = push!(ir, IRTools.stmt(Base.Expr(:call, i64.eq, i, vi), type = i32))
    branch!(ir, length(ir.blocks) + 2, unless = cond)
    block!(ir)
    range = sublayout(x, i)
    T′ = partial_part(x, i)
    ex = layout(T′) isa WTuple ?
      Base.Expr(:tuple, part.(range)...) :
      part(range[1])
    y = push!(ir, IRTools.stmt(ex, type = layout(T′)))
    if T′ != T
      T′ isa Number && T == typeof(T′) || error("unsupported cast")
      y = push!(ir, IRTools.stmt(T′, type = layout(T)))
    end
    return!(ir, y)
    block!(ir)
  end
  push!(ir, WebAssembly.unreachable)
  return ir
end

struct WModule
  inf::Inference
  symbols::Dict{Symbol,Int}
  strings::Vector{String}
  funcs::Dict{Any,Base.Tuple{Symbol,IR}}
end

function name(mod::WModule, s::Symbol)
  s == :_start && return s
  c = mod.symbols[s] = get(mod.symbols, s, 0)+1
  return Symbol(s, ":", c)
end

function stringid!(mod::WModule, s::String)
  i = findfirst(==(s), mod.strings)
  i === nothing || return i-1
  push!(mod.strings, s)
  return length(mod.strings)-1
end

WModule(inf) = WModule(inf, Dict(), [], Dict())

function sigs!(ir::IR)
  for (v, st) in ir
    if isexpr(st.expr, :call)
      if st.expr.args[1] isa WIntrinsic
      elseif st.expr.args[1] isa RMethod || st.expr.args[1] == :cast
        ir[v] = Base.Expr(:call, exprtype.((ir,), st.expr.args), st.expr.args...)
      else
        f, xs = exprtype.((ir,), st.expr.args)
        ir[v] = Base.Expr(:call, [f, parts(xs)...], st.expr.args...)
      end
    elseif isexpr(st.expr, :tuple)
    else
      error("unrecognised $(st.expr.head) expr")
    end
  end
  return ir
end

# Arguments are turned into a tuple when calling any function, so this is
# basically just a cast.
function lowerdata!(mod::WModule, ir, v)
  ir[v] = ir[v].expr.args[3]
end

# TODO assumes tags are not present at runtime.
function lowerdatacat!(mod::WModule, ir, v)
  ir[v] = ir[v].expr.args[3]
end

ismethod(m, name) = m isa RMethod && m.name == name

function lowerwasm!(mod::WModule, ir::IR)
  prune!(ir)
  casts!(ir)
  sigs!(ir)
  for b in blocks(ir)
    IRTools.argtypes(b) .= layout.(IRTools.argtypes(b))
    for (v, st) in b
      if isexpr(st.expr, :tuple)
        # remove constants, which have zero width
        args = filter(x -> x isa Variable, st.expr.args)
        ir[v] = length(args) == 1 ? args[1] : Base.Expr(:tuple, args...)
        continue
      end
      Ts, args = st.expr.args[1], st.expr.args[2:end]
      if Ts isa WIntrinsic
        ex = Base.Expr(:call, st.expr.args[1].op, st.expr.args[2:end]...)
        ir[v] = IRTools.stmt(st.expr, expr = ex, type = Ts.ret)
      elseif ismethod(Ts[1], :widen)
        val = Ts[2] isa Integer ? Ts[2] : st.expr.args[3]
        ir[v] = IRTools.stmt(val, type = layout(st.type))
      elseif Ts[1] == :cast # TODO make this an expr type
        _, T, val = Ts
        (val isa Number && T == typeof(val)) || error("unsupported cast")
        ir[v] = IRTools.stmt(val, type = layout(T))
      elseif ismethod(Ts[1], :data) # TODO: should specifically check this is the fallback method
        lowerdata!(mod, ir, v)
      elseif ismethod(Ts[1], :datacat)
        lowerdatacat!(mod, ir, v)
      elseif ismethod(Ts[1], :part) # TODO: same
        x::Union{String,Data}, i = Ts[2:end]
        if x isa String && i == 1
          ir[v] = IRTools.stmt(Int32(stringid!(mod, x)), type = layout(st.type))
          continue
        end
        if i isa Int
          xlayout = layout(x)
          part(i) = xlayout isa WTuple ? insert!(ir, v, Base.Expr(:ref, args[2], i)) : args[2]
          range = sublayout(x, i)
          ex = layout(st.type) isa WTuple ?
            Base.Expr(:tuple, part.(range)...) :
            part(range[1])
          ir[v] = IRTools.stmt(ex, type = layout(st.type))
        else
          func = partmethod!(mod, Ts[1], x, i)
          ir[v] = Base.Expr(:call, WebAssembly.Call(func), args[2:end]...)
          ir[v] = IRTools.stmt(ir[v], type = layout(ir[v].type))
        end
      elseif ismethod(Ts[1], :nparts)
        ir[v] = nparts(Ts[2])
      else
        func = lowerwasm!(mod, rtuple(Ts...))
        ir[v] = Base.Expr(:call, WebAssembly.Call(func), args[2:end]...)
        ir[v] = IRTools.stmt(ir[v], type = layout(ir[v].type))
      end
    end
  end
  return ir
end

function lowerwasm!(mod::WModule, T)
  haskey(mod.funcs, T) && return mod.funcs[T][1]
  f = part(T, 1)::Union{Symbol,RMethod}
  id = name(mod, f isa Symbol ? f : Symbol(f.name, ":method"))
  ir = lowerwasm!(mod, mod.inf.frames[T].ir)
  mod.funcs[T] = (id, ir)
  return id
end

function partmethod!(mod::WModule, m::RMethod, x, i)
  T = rtuple(m, x, i)
  haskey(mod.funcs, T) && return mod.funcs[T][1]
  id = name(mod, Symbol("part:method"))
  ir = partir(x, i)
  mod.funcs[rtuple(m, x, i)] = (id, ir)
  return id
end

default_imports = [
  WebAssembly.Import(:support, :global, :jsglobal, :func, [], [i32]),
  WebAssembly.Import(:support, :property, :jsproperty, :func, [i32, i32], [i32]),
  WebAssembly.Import(:support, :call, :jscall0, :func, [i32, i32], [i32]),
  WebAssembly.Import(:support, :call, :jscall1, :func, [i32, i32, i32], [i32]),
  WebAssembly.Import(:support, :panic, :panic, :func, [i32], []),
  WebAssembly.Import(:support, :retain, :retain, :func, [i32], []),
  WebAssembly.Import(:support, :release, :release, :func, [i32], []),
  WebAssembly.Import(:support, :createRef, :jsbox, :func, [f64], [i32]),
  WebAssembly.Import(:support, :fromRef, :jsunbox, :func, [i32], [f64])]

function wasm_ir(inf::Inference)
  mod = WModule(inf)
  lowerwasm!(mod, rtuple(startmethod(inf.mod)))
  return mod
end

function wasmmodule(inf::Inference)
  mod = wasm_ir(inf)
  strings = mod.strings
  fs = [WebAssembly.irfunc(name, ir) for (name, ir) in values(mod.funcs)]
  mod = WebAssembly.Module(
    funcs = fs,
    imports = default_imports,
    exports = [WebAssembly.Export(:_start, Symbol("_start:method:1"), :func)],
    mems = [WebAssembly.Mem(0)])
  return mod, strings
end

function binary(m::WebAssembly.Module, file; optimise = true)
  wat = tempname() * ".wat"
  WebAssembly.write_wat(wat, m)
  try
    run(`wat2wasm $wat -o $file`)
    optimise && run(`wasm-opt --enable-multivalue $file -O4 -o $file`)
  finally
    rm(wat)
  end
  return
end

function emitwasm(file, out)
  mod, strings = wasmmodule(loadfile(file))
  binary(mod, out)
  return strings
end

sigmatch(sig, func) = sig[1] == func || ismethod(sig[1], func)

function code_wasm(src, func = :_main)
  mod = loadfile(src)
  mod = wasm_ir(mod)
  Dict{Any,IR}(sig => fr[2] for (sig, fr) in mod.funcs if sigmatch(sig, func))
end

function code_typed(src, func = :_main)
  inf = loadfile(src)
  Dict{Any,IR}(sig => fr.ir for (sig, fr) in inf.frames if sigmatch(sig, func))
end

function code_lowered(src, func = :_main)
  inf = loadfile(src)
  return Dict(meth.pattern => meth.func for meth in inf.mod.methods[func])
end
