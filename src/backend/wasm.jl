using WebAssembly: WType, i32, i64, f32, f64

wasmtype(::PrimitiveHole{T}) where T = WebAssembly.WType(T)
wasmtype(T::Type) = WType(T)
wasmtype(T::WType) = T

wasmops = Dict(
  (:+, i64, i64) => (i64.add, i64),
  (:*, i64, i64) => (i64.mul, i64),
  (:-, i64, i64) => (i64.sub, i64),
  (:>, i64, i64) => (i64.gt_s, i32))

struct WModule
  inf::Inference
  funcs::Dict{Any,Base.Tuple{Symbol,IR}}
end

WModule(inf) = WModule(inf, Dict())

function lowerwasm!(mod::WModule, ir::IR)
  for b in blocks(ir)
    IRTools.argtypes(b) .= wasmtype.(IRTools.argtypes(b))
    for (v, st) in b
      op = st.expr.args[1]
      args = st.expr.args[2:end]
      Ts = wasmtype.(IRTools.exprtype.((ir,), args))
      if haskey(wasmops, (op, Ts...))
        op, T = wasmops[(op, Ts...)]
        ir[v] = IRTools.stmt(st, expr = Base.Expr(:call, op, args...), type = T)
      else
        T = vtuple(exprtype.((ir,), st.expr.args)...)
        func = lowerwasm!(mod, T)
        ir[v] = Base.Expr(:call, WebAssembly.Call(func), args...)
        ir[v] = IRTools.stmt(ir[v], type = wasmtype(ir[v].type))
      end
    end
  end
  return ir
end

function lowerwasm!(mod::WModule, T)
  haskey(mod.funcs, T) && return mod.funcs[T][1]
  name = T.data[2]::Symbol
  ir = lowerwasm!(mod, mod.inf.frames[T].ir)
  mod.funcs[T] = (name, ir)
  return name
end

function wasmmodule(inf::Inference)
  mod = WModule(inf)
  lowerwasm!(mod, vtuple(:_start))
  fs = [WebAssembly.irfunc(name, ir) for (name, ir) in values(mod.funcs)]
  WebAssembly.Module(funcs = fs, exports = [WebAssembly.Export(:_start, :_start, :func)])
end

function compile(file, out)
  WebAssembly.binary(wasmmodule(loadfile(file)), out)
end
