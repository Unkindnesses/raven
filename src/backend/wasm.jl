using WebAssembly: WType, i32, i64, f32, f64

wasmtype(::PrimitiveHole{T}) where T = WebAssembly.WType(T)
wasmtype(T::Type) = WType(T)
wasmtype(T::WType) = T

wasmops = Dict(
  (:+, i64, i64) => (i64.add, i64),
  (:*, i64, i64) => (i64.mul, i64),
  (:-, i64, i64) => (i64.sub, i64))

function wasmops!(ir)
  for (v, st) in ir
    op = st.expr.args[1]
    args = st.expr.args[2:end]
    Ts = wasmtype.(IRTools.exprtype.((ir,), args))
    op, T = wasmops[(op, Ts...)]
    ir[v] = IRTools.stmt(st, expr = Base.Expr(:call, op, args...), type = T)
  end
  return ir
end

function wasmmodule(mod::Inference)
  main = mod.frames[vtuple(:_start)].ir
  f = WebAssembly.irfunc(:_start, wasmops!(main))
  WebAssembly.Module(funcs = [f], exports = [WebAssembly.Export(:_start, :_start, :func)])
end

function compile(file, out)
  WebAssembly.binary(wasmmodule(loadfile(file)), out)
end
