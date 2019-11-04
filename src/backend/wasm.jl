using WebAssembly: WType, WTuple, i32, i64, f32, f64

wasmtype(::PrimitiveHole{T}) where T = WebAssembly.WType(T)
wasmtype(x::Primitive) = WTuple()

wasmops = Dict(
  (:+, i64, i64) => (i64.add, i64),
  (:*, i64, i64) => (i64.mul, i64),
  (:-, i64, i64) => (i64.sub, i64),
  (:>, i64, i64) => (i64.gt_s, i32))

intrinsic(op::Symbol, args::Union{Primitive,PrimitiveHole}...) =
  get(wasmops, (op, WType.(jtype.(args))...), nothing)

intrinsic(op, args...) = nothing

struct WModule
  inf::Inference
  funcs::Dict{Any,Base.Tuple{Symbol,IR}}
end

WModule(inf) = WModule(inf, Dict())

function sigs!(ir::IR)
  for (v, st) in ir
    ir[v] = Base.Expr(:call, exprtype.((ir,), st.expr.args), st.expr.args...)
  end
  return ir
end

function lowerwasm!(mod::WModule, ir::IR)
  sigs!(ir)
  for b in blocks(ir)
    IRTools.argtypes(b) .= wasmtype.(IRTools.argtypes(b))
    for (v, st) in b
      Ts, args = st.expr.args[1], st.expr.args[2:end]
      if Ts[1] == :widen
        ir[v] = IRTools.stmt(st.expr.args[3], type = wasmtype(st.type))
      elseif (int = intrinsic(Ts...)) != nothing
        op, T = int
        args = st.expr.args[2:end]
        ir[v] = IRTools.stmt(st, expr = Base.Expr(:call, op, args[2:end]...), type = T)
      else
        func = lowerwasm!(mod, rtuple(Ts...))
        ir[v] = Base.Expr(:call, WebAssembly.Call(func), args[2:end]...)
        ir[v] = IRTools.stmt(ir[v], type = wasmtype(ir[v].type))
      end
    end
  end
  return ir
end

function lowerwasm!(mod::WModule, T)
  haskey(mod.funcs, T) && return mod.funcs[T][1]
  name = part(T, 1)::Symbol
  ir = lowerwasm!(mod, mod.inf.frames[T].ir)
  mod.funcs[T] = (name, ir)
  return name
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
