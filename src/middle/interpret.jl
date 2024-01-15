function interpret(int, ir::IR, args...)
  env = Dict{Variable,Any}()
  resolve(x) = x
  resolve(v::Variable) = env[v]
  resolve(v::Binding) = resolve_global(int.defs, v)
  foreach(((v, x),) -> env[v] = x, zip(arguments(ir), args))
  bl = 1
  while true
    for (v, st) in block(ir, bl)
      if isexpr(st, :branch)
        if isreturn(st.expr)
          return arguments(st.expr)[1] |> resolve
        else
          error("branch")
        end
      else
        error("Unknown expr type $(st.expr.head)")
      end
    end
  end
end

interpret(int, meth::RMethod, args...) =
  meth.func isa IR ? interpret(int, meth.func, args...) : meth.func(args...)

function interpret(int, func::Tag, args)
  for meth in reverse(int.defs.methods[func])
    m = partial_match(int, meth.sig.pattern, args)
    isnothing(m) && continue
    ismissing(m) && return
    args = [m[a][1] for a in meth.sig.args]
    return interpret(int, meth, args...)
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
