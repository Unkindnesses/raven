using WebAssembly: WType, WTuple, i32, i64, f32, f64

function intrinsic(ex)
  @assert ex isa Operator && ex.op == :.
  typ = WType(ex.args[1])
  ex = ex.args[2]
  if ex isa Call
    name = ex.func
    args = ex.args
  elseif ex isa Operator && ex.op == :/
    name = Symbol(ex.args[1], "/", ex.args[2].func)
    args = ex.args[2].args
  else
    error("Unrecognised intrinsic $(repr(ex))")
  end
  WebAssembly.Op(typ, name)
end

function intrinsic_args(ex)
  ex isa Operator && return intrinsic_args(ex.args[2])
  return map(x -> x.args[1], ex.args)
end

WNum = Union{Int32,Int64,Float32,Float64}

function cat_layout(xs...; result = [])
  for x in xs
    x isa WTuple ? cat_layout(x.parts..., result = result) : push!(result, x)
  end
  return length(result) == 1 ? result[1] : WTuple(result)
end

layout(::PrimitiveHole{T}) where T = WebAssembly.WType(T)
layout(x::Union{Primitive,Quote}) = WTuple()
layout(x::Data) = cat_layout(layout.(x.parts)...)

function wparts(x)
  ly = layout(x)
  return ly isa WTuple ? ly.parts : [ly]
end

wasmops = Dict(
  (:+, i64, i64) => (i64.add, i64),
  (:*, i64, i64) => (i64.mul, i64),
  (:-, i64, i64) => (i64.sub, i64),
  (:>, i64, i64) => (i64.gt_s, i32))

intrinsic(op::Symbol, args::Union{WNum,PrimitiveHole{<:WNum}}...) =
  get(wasmops, (op, WType.(jtype.(args))...), nothing)

intrinsic(op, args...) = nothing

struct WModule
  inf::Inference
  symbols::Dict{Symbol,Int}
  funcs::Dict{Any,Base.Tuple{Symbol,IR}}
end

function name(mod::WModule, s::Symbol)
  s == :_start && return s
  c = mod.symbols[s] = get(mod.symbols, s, 0)+1
  return Symbol(s, ":", c)
end

WModule(inf) = WModule(inf, Dict(), Dict())

function sigs!(ir::IR)
  for (v, st) in ir
    st.expr.args[1] isa WebAssembly.Op && continue
    ir[v] = Base.Expr(:call, exprtype.((ir,), st.expr.args), st.expr.args...)
  end
  return ir
end

function lowerdata!(mod::WModule, ir, v)
  Ts, args = ir[v].expr.args[1][2:end], ir[v].expr.args[3:end]
  ps = filter(i -> !isempty(wparts(Ts[i])), 1:length(args))
  if length(ps) == 1
    ir[v] = IRTools.stmt(args[ps[]], type = layout(ir[v].type))
  else
    error("composite data not implemented")
  end
end

function lowerwasm!(mod::WModule, ir::IR)
  sigs!(ir)
  for b in blocks(ir)
    IRTools.argtypes(b) .= layout.(IRTools.argtypes(b))
    for (v, st) in b
      Ts, args = st.expr.args[1], st.expr.args[2:end]
      if Ts isa WebAssembly.Op
        ir[v] = IRTools.stmt(st.expr, type = Ts.typ)
      elseif Ts[1] == :widen
        ir[v] = IRTools.stmt(st.expr.args[3], type = layout(st.type))
      elseif Ts[1] == :data
        lowerdata!(mod, ir, v)
      elseif Ts[1] == :part
        x::Data, i::Integer = Ts[2:end]
        if length(wparts(st.type)) == 1 && length(wparts(part(x, i))) == 1
          ir[v] = IRTools.stmt(args[2], type = layout(st.type))
        else error("composite data not implemented")
        end
      elseif (int = intrinsic(Ts...)) != nothing
        op, T = int
        args = st.expr.args[2:end]
        ir[v] = IRTools.stmt(st, expr = Base.Expr(:call, op, args[2:end]...), type = T)
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
  id = part(T, 1)::Symbol
  id = name(mod, id)
  ir = lowerwasm!(mod, mod.inf.frames[T].ir)
  mod.funcs[T] = (id, ir)
  return id
end

function wasmmodule(inf::Inference)
  mod = WModule(inf)
  lowerwasm!(mod, rtuple(:_start))
  fs = [WebAssembly.irfunc(name, ir) for (name, ir) in values(mod.funcs)]
  WebAssembly.Module(funcs = fs, exports = [WebAssembly.Export(:_start, :_start, :func)])
end

function compile(file, out)
  WebAssembly.binary(wasmmodule(loadfile(file)), out)
end
