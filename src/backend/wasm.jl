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
    st.expr.args[1] isa WIntrinsic && continue
    ir[v] = Base.Expr(:call, exprtype.((ir,), st.expr.args), st.expr.args...)
  end
  return ir
end

function lowerdata!(mod::WModule, ir, v)
  Ts, args = ir[v].expr.args[1][2:end], ir[v].expr.args[3:end]
  parts = []
  for (T, x) in zip(Ts, args)
    if nregisters(layout(T)) == 1
      push!(parts, x)
    else
      for i = 1:nregisters(layout(T))
        push!(parts, insert!(ir, v, Base.Expr(:ref, x, i)))
      end
    end
  end
  ir[v] = IRTools.stmt(length(parts) == 1 ? parts[1] : Base.Expr(:tuple, parts...),
                       type = layout(ir[v].type))
end

ismethod(m, name) = m isa RMethod && m.name == name

function lowerwasm!(mod::WModule, ir::IR)
  prune!(ir)
  casts!(ir)
  sigs!(ir)
  for b in blocks(ir)
    IRTools.argtypes(b) .= layout.(IRTools.argtypes(b))
    for (v, st) in b
      Ts, args = st.expr.args[1], st.expr.args[2:end]
      if Ts isa WIntrinsic
        ex = Base.Expr(:call, st.expr.args[1].op, st.expr.args[2:end]...)
        ir[v] = IRTools.stmt(st.expr, expr = ex, type = Ts.ret)
      elseif ismethod(Ts[1], :widen)
        val = Ts[2] isa Integer ? Ts[2] : st.expr.args[3]
        ir[v] = IRTools.stmt(val, type = layout(st.type))
      elseif Ts[1] == :cast
        _, T, val = Ts
        (val isa Number && T isa Type) || error("unsupported cast")
        ir[v] = IRTools.stmt(Ts[3], type = layout(T))
      elseif ismethod(Ts[1], :data)
        lowerdata!(mod, ir, v)
      elseif ismethod(Ts[1], :part)
        x::Data, i::Integer = Ts[2:end]
        xlayout = layout(x)
        part(i) = xlayout isa WTuple ? insert!(ir, v, Base.Expr(:ref, args[2], i)) : args[2]
        range = sublayout(x, i)
        ex = layout(st.type) isa WTuple ?
          Base.Expr(:tuple, part.(range)...) :
          part(range[1])
        ir[v] = IRTools.stmt(ex, type = layout(st.type))
      elseif ismethod(Ts[1], :nparts)
        ir[v] = nparts(Ts[2])
      elseif ismethod(Ts[1], :tojs) && Ts[2] isa String
        ir[v] = IRTools.stmt(Int32(stringid!(mod, Ts[2])), type = layout(st.type))
      else
        func = lowerwasm!(mod, rtuple(Ts...))
        # Filter gets rid of constants
        ir[v] = Base.Expr(:call, WebAssembly.Call(func), filter(x -> x isa Variable, args[2:end])...)
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

# TODO hacky
function wasm_primitives!(mod::RModule)
  method!(mod, :tojs, RMethod(:tojs, lowerpattern(rvx"(s: PrimitiveString,)")..., _ -> data(:JSObject, Int32), true))
end

function wasmmodule(mod::RModule)
  wasm_primitives!(mod)
  wasmmodule(Inference(mod))
end

function binary(m::WebAssembly.Module, file; optimise = true)
  wat = tempname() * ".wat"
  WebAssembly.write_wat(wat, m)
  run(`wat2wasm $wat -o $file`)
  optimise && run(`wasm-opt --enable-multivalue $file -O4 -o $file`)
  rm(wat)
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
  wasm_primitives!(mod)
  mod = wasm_ir(Inference(mod))
  Dict{Any,IR}(sig => fr[2] for (sig, fr) in mod.funcs if sigmatch(sig, func))
end

function code_typed(src, func = :_main)
  inf = Inference(loadfile(src))
  Dict{Any,IR}(sig => fr.ir for (sig, fr) in inf.frames if sigmatch(sig, func))
end
