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

reachable(b::IRTools.Block) =
  any(((v, st),) -> st.type == ⊥, b) ? Set(b.id) :
    reduce(Base.union, [Set(b.id), [reachable(c) for c in successors(b) if c.id > b.id]...])

reachable(ir::IR) = reachable(block(ir, 1))

function pruneblocks!(ir::IR)
  bs = reachable(ir)
  for i = length(blocks(ir)):-1:2
    i in bs || IRTools.deleteblock!(ir, i)
  end
  return ir
end

function trim_unreachable!(ir)
  ir = pruneblocks!(ir)
  pr = IRTools.Pipe(ir)
  flag = false
  IRTools.branches(pr) do br
    flag ? nothing : br
  end
  for (v, st) in pr
    if v == first(IRTools.block(ir, v))[1]
      flag = false
    end
    if flag
      delete!(pr, v)
      replace!(pr, v, nothing) # slight kludge, pipes should support this
    elseif st.type == ⊥
      flag = true
    end
  end
  return IRTools.finish(pr)
end
