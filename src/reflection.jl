ismethod(m, name) = m isa RMethod && m.name == name
sigmatch(sig, func) = sig[1] == func || ismethod(sig[1], func)

sigmatch(sig, func, Ts) =
  sigmatch(sig, func) &&
  (sig[1] isa RMethod ? sig[2:end] == (Ts...,) :
   sig[2] == rlist(Ts...))

function code_lowered(cx::Inference, func)
  return IdDict(meth.sig.pattern => meth.func for meth in cx.mod.methods[func])
end

function code_typed(cx::Inference, func...)
  IdDict{Any,IR}(sig => unloop(fr.ir) for (sig, fr) in cx.frames if fr isa Frame && sigmatch(sig, func...))
end

function code_final(cx::Inference, func...)
  cx = lowerir(cx)
  IdDict{Any,IR}(sig => ir for (sig, ir) in cx.frames if sigmatch(sig, func...))
end

function code_wasm(cx::Inference, func)
  mod = wasm_ir(lowerir(cx))
  IdDict{Any,IR}(sig => fr[2] for (sig, fr) in mod.funcs if sigmatch(sig, func))
end

code_lowered(src::AbstractString, func) = code_lowered(loadfile(src, infer = false), func)
code_typed(src::AbstractString, func...) = code_typed(loadfile(src), func...)
code_final(src::AbstractString, func...) = code_final(loadfile(src), func...)
code_wasm(src::AbstractString, func) = code_wasm(loadfile(src), func)
