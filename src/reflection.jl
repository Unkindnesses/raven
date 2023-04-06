ismethod(m, name) = m isa RMethod && m.name == name
sigmatch(sig, func) = Symbol(sig[1]) == func || ismethod(sig[1], func)

sigmatch(sig, func, Ts) =
  sigmatch(sig, func) &&
  (sig[1] isa RMethod ? sig[2:end] == (Ts...,) :
   sig[2] == rlist(Ts...))

function code_lowered(cx::RModule, func)
  return IdDict(meth.sig.pattern => meth.func for meth in cx.methods[Tag(func)])
end

function code_typed(mod::RModule, func...)
  cx = infer(mod)
  cx |> lowerir |> refcounts |> (x -> wasmmodule(x, startmethod(mod)))
  IdDict{Any,IR}(sig => fr[1] for (sig, fr) in cx.data if !(fr isa Redirect) && sigmatch(sig, func...))
end

function code_final(mod::RModule, func...)
  cx = mod |> infer |> lowerir |> refcounts
  wasmmodule(cx, startmethod(mod))
  IdDict{Any,IR}(sig => ir for (sig, ir) in cx.data if sigmatch(sig, func...))
end

function code_wasm(cx::RModule, func)
  mod = cx |> infer |> lowerir |> refcounts |> (x -> wasm_ir(x, startmethod(cx)))
  IdDict{Any,IR}(sig => fr[2] for (sig, fr) in mod.funcs if sigmatch(sig, func))
end

code_lowered(src::AbstractString, func) = code_lowered(loadfile(src), func)
code_typed(src::AbstractString, func...) = code_typed(loadfile(src), func...)
code_final(src::AbstractString, func...) = code_final(loadfile(src), func...)
code_wasm(src::AbstractString, func) = code_wasm(loadfile(src), func)
