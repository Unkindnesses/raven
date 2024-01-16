function interpret(int, ir::IR, args...)
  env = Dict{Variable,Any}()
  resolve(x) = x
  resolve(v::Variable) = env[v]
  resolve(v::Binding) = resolve_global(int.defs, v)
  foreach(((v, x),) -> env[v] = x, zip(arguments(ir), args))
  bl = 1
  while true
    for (v, st) in block(ir, bl)
      args = resolve.(st.expr.args)
      any(==(⊥), args) && return
      if isexpr(st, :call)
        if (w = args[1]) isa WIntrinsic
          haskey(wasmPartials, w.op) || return
          args = args[2:end]
          env[v] = all(isvalue, args) ? wasmPartials[w.op](args...) : rvtype(w.ret)
        else
          result = int[(args...,)]
          isnothing(result) && return
          env[v] = result
        end
      elseif isexpr(st, :pack)
        env[v] = pack(args...)
      elseif isexpr(st, :branch)
        if isreturn(st.expr)
          return args[3]
        elseif isunreachable(st.expr)
          return
        else
          target, cond, args... = args
          if !isnothing(cond)
            cond isa Int32 || return
            Bool(cond) || continue
          end
          bl = target
          foreach(((v, x),) -> env[v] = x, zip(arguments(block(ir, bl)), args))
          break
        end
      elseif isexpr(st, :(=))
        return
      else
        error("Unknown expr type $(st.expr.head)")
      end
    end
  end
end

function interpret(int, meth::RMethod, args...)
  meth == invoke_method && return
  meth.func isa IR ? interpret(int, meth.func, args...) : meth.func(args...)
end

function interpret(int, func::Tag, args)
  for meth in reverse(int.defs.methods[func])
    m = partial_match(int, meth.sig.pattern, args)
    isnothing(m) && continue
    ismissing(m) && return
    args = [m[a][1] for a in meth.sig.args]
    result = interpret(int, meth, args...)
    isnothing(result) && return
    return isempty(meth.sig.swap) ? rlist(result) : result
  end
end

struct Interpreter
  defs::Definitions
  results::Cache{Any,Any}
end

function Interpreter(defs::Definitions)
  ch = Cache{Any,Any}() do self, sig
    self = Interpreter(defs, self)
    interpret(self, sig...)
  end
  return Interpreter(defs, ch)
end

Base.getindex(int::Interpreter, sig) = int.results[sig]

Caches.subcaches(int::Interpreter) = (int.results,)
