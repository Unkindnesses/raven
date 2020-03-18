using WebAssembly: WType, WTuple, i32, i64, f32, f64

rvtype(x::WType) = PrimitiveHole{WebAssembly.jltype(x)}()
rvtype(x::WTuple) = data(:Tuple, map(rvtype, x.parts)...)

struct WIntrinsic
  op::WebAssembly.Op
  ret
end

function intrinsic(ex)
  if ex isa Operator && ex.op == :(::)
    typ = WType(ex.args[2])
    ex = ex.args[1].func
  else
    typ = WebAssembly.WTuple()
    ex = ex.func
  end
  namify(x::Symbol) = x
  namify(x::Raven.Operator) = Symbol(join(namify.(x.args), x.op))
  WIntrinsic(WebAssembly.Op(namify(ex)), typ)
end

function intrinsic_args(ex)
  ex isa Operator && ex.op == :(::) && return intrinsic_args(ex.args[1])
  return map(x -> Call(:widen, [x.args[1]]), ex.args)
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
    st.expr.args[1] isa WIntrinsic && continue
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
    ir[v] = IRTools.stmt(Base.Expr(:tuple, map(p -> args[p], ps)...), type = layout(ir[v].type))
  end
end

function lowerwasm!(mod::WModule, ir::IR)
  casts!(ir)
  sigs!(ir)
  for b in blocks(ir)
    IRTools.argtypes(b) .= layout.(IRTools.argtypes(b))
    for (v, st) in b
      Ts, args = st.expr.args[1], st.expr.args[2:end]
      if Ts isa WIntrinsic
        ex = Base.Expr(:call, st.expr.args[1].op, st.expr.args[2:end]...)
        ir[v] = IRTools.stmt(st.expr, expr = ex, type = Ts.ret)
      elseif Ts[1] == :widen
        val = Ts[2] isa Integer ? Ts[2] : st.expr.args[3]
        ir[v] = IRTools.stmt(val, type = layout(st.type))
      elseif Ts[1] == :cast
        _, T, val = Ts
        (val isa Number && T isa PrimitiveHole) || error("unsupported cast")
        ir[v] = IRTools.stmt(Ts[3], type = layout(T))
      elseif Ts[1] == :data
        lowerdata!(mod, ir, v)
      elseif Ts[1] == :part
        x::Data, i::Integer = Ts[2:end]
        if length(wparts(st.type)) == 1 && length(wparts(x)) == 1
          ir[v] = IRTools.stmt(args[2], type = layout(st.type))
        elseif length(wparts(st.type)) == 1
          ir[v] = IRTools.stmt(Base.Expr(:ref, args[2], args[3]),
                       type = layout(st.type))
        else error("composite `part` not supported")
        end
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
  mod = WebAssembly.Module(
    funcs = fs,
    exports = [WebAssembly.Export(:_start, :_start, :func)],
    mems = [WebAssembly.Mem(0)])
  WebAssembly.multivalue_shim!(mod)
  return mod
end

function compile(file, out)
  WebAssembly.binary(wasmmodule(loadfile(file)), out)
end
