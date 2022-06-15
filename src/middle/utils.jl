# HACK: belongs in IRTools.
# Also, could be written as `pr[x] = y`
function Base.replace!(pr::IRTools.Pipe, x, y)
  IRTools.substitute!(pr, x, IRTools.substitute(pr, y))
end

function tuplecse(ir)
  pr = IRTools.Pipe(ir)
  env = Dict{Any,Variable}()
  tuples = Dict{Variable,Expr}()
  for (v, st) in pr
    if isexpr(st.expr, :ref, :tuple)
      isexpr(st.expr, :tuple) && (tuples[v] = st.expr)
      ex = IRTools.substitute(pr, st.expr)
      key = (ex, st.type)
      if isexpr(st.expr, :ref) && st.type == Any && haskey(tuples, st.expr.args[1])
        delete!(pr, v)
        replace!(pr, v, tuples[st.expr.args[1]].args[st.expr.args[2]])
      elseif haskey(env, key)
        delete!(pr, v)
        replace!(pr, v, env[key])
      else
        env[key] = v
      end
    end
  end
  return IRTools.finish(pr)
end
