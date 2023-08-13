ismethod(m, name) = m isa RMethod && m.name == name
sigmatch(sig, func) = sig[1] == func || ismethod(sig[1], func)

sigmatch(sig, func, Ts) =
  sigmatch(sig, func) &&
  (sig[1] isa RMethod ? sig[2:end] == (Ts...,) :
   sig[2] == rlist(Ts...))

function code_lowered(cx::Compilation, func)
  return IdDict(meth.sig.pattern => meth.func for meth in methods(cx, func))
end

function code_typed(mod::Compilation, func...)
  cx = mod |> Definitions |> infer
  cx[(tag"common.core.main",rlist())]
  IdDict{Any,IR}(sig => fr[1] for (sig, fr) in IdDict(cx) if !(fr isa Redirect) && sigmatch(sig, func...))
end

function code_final(mod::Compilation, func...)
  mod = mod |> Definitions
  cx = mod |> infer |> lowerir |> refcounts
  wasmmodule(mod, cx, startmethod(mod))
  IdDict{Any,IR}(sig => ir for (sig, ir) in IdDict(cx) if sigmatch(sig, func...))
end

function code_wasm(cx::Compilation, func)
  cx = Definitions(cx)
  mod = cx |> infer |> lowerir |> refcounts |> (x -> wasm_ir(cx, x, startmethod(cx)))
  IdDict{Any,IR}(sig => fr[2] for (sig, fr) in mod.funcs if sigmatch(sig, func))
end

code_lowered(src::AbstractString, func) = code_lowered(load(src), func)
code_typed(src::AbstractString, func...) = code_typed(load(src), func...)
code_final(src::AbstractString, func...) = code_final(load(src), func...)
code_wasm(src::AbstractString, func) = code_wasm(load(src), func)
