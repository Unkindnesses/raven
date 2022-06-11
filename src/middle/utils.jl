# HACK: belongs in IRTools.
function Base.replace!(pr::IRTools.Pipe, x, y)
  IRTools.substitute!(pr, x, IRTools.substitute(pr, y))
end

function tuplecse(ir)
  pr = IRTools.Pipe(ir)
  env = Dict{Any,Variable}()
  for (v, st) in pr
    if isexpr(st.expr, :ref, :tuple)
      ex = IRTools.substitute(pr, st.expr)
      key = (ex, st.type)
      if haskey(env, key)
        delete!(pr, v)
        replace!(pr, v, env[key])
      else
        env[key] = v
      end
    end
  end
  return IRTools.finish(pr)
end
