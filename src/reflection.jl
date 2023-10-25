ismethod(m, name) = m isa RMethod && m.name == name
sigmatch(sig, func) = sig isa Tuple && (sig[1] == func || ismethod(sig[1], func))

sigmatch(sig, func, Ts) =
  sigmatch(sig, func) &&
  (sig[1] isa RMethod ? sig[2:end] == (Ts...,) :
   sig[2] == rlist(Ts...))

function code_lowered(cx::Modules, func)
  return IdDict(meth.sig.pattern => meth.func for meth in methods(cx, func))
end

function code_typed(mod::Modules, func...)
  c = Compiler(mod)
  wasmmodule(c.pipe.caches[end], c.defs[tag"common.core.main"])
  inf = c.pipe.caches[3]
  IdDict{Any,IR}(sig => fr[1] for (sig, fr) in IdDict(inf.results) if !(fr isa Redirect) && sigmatch(sig, func...))
end

function code_final(mod::Modules, func...)
  c = Compiler(mod)
  wasmmodule(c.pipe.caches[end], c.defs[tag"common.core.main"])
  cx = c.pipe.caches[end-1]
  IdDict{Any,IR}(sig => ir for (sig, ir) in IdDict(cx) if sigmatch(sig, func...))
end

function code_wasm(mod::Modules, func)
  c = Compiler(mod)
  mod = c.pipe.caches[end]
  wasmmodule(mod, c.defs[tag"common.core.main"])
  IdDict{Any,WebAssembly.Func}(sig => fr for (sig, fr) in IdDict(mod.funcs) if sigmatch(sig, func))
end

code_lowered(src, func) = code_lowered(load(src), func)
code_typed(src, func...) = code_typed(load(src), func...)
code_final(src, func...) = code_final(load(src), func...)
code_wasm(src, func) = code_wasm(load(src), func)
