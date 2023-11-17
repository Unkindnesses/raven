ismethod(m, name) = m isa RMethod && m.name == name
sigmatch(sig, func) = sig isa Tuple && (sig[1] == func || ismethod(sig[1], func))

sigmatch(sig, func, Ts) =
  sigmatch(sig, func) &&
  (sig[1] isa RMethod ? sig[2:end] == (Ts...,) :
   sig[2] == rlist(Ts...))

function code_lowered(c::Compiler, func)
  return IdDict(meth.sig.pattern => meth.func for meth in methods(c.pipe.sources, func))
end

function code_typed(c::Compiler, func...)
  inf = c.pipe.inferred
  IdDict{Any,Any}(sig => fr for (sig, fr) in IdDict(inf.results) if !(fr isa Redirect) && sigmatch(sig, func...))
end

function code_final(c::Compiler, func...)
  cx = c.pipe.counted
  IdDict{Any,IR}(sig => ir for (sig, ir) in IdDict(cx) if sigmatch(sig, func...))
end

function code_wasm(c::Compiler, func)
  mod = c.pipe.wasm
  IdDict{Any,WebAssembly.Func}(sig => fr for (sig, fr) in IdDict(mod.funcs) if sigmatch(sig, func))
end

code_lowered(src, func) = code_lowered(Compiler(src), func)
code_typed(src, func...) = code_typed(Compiler(src), func...)
code_final(src, func...) = code_final(Compiler(src), func...)
code_wasm(src, func) = code_wasm(Compiler(src), func)
