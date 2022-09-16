# Belongs in IRTools.
# Also, could be written as `pr[x] = y`
function Base.replace!(pr::IRTools.Pipe, x, y)
  IRTools.substitute!(pr, x, IRTools.substitute(pr, y))
end

reachable(b::IRTools.Block) =
  any(((v, st),) -> st.type == ⊥, b) ? Set(b.id) :
    reduce(Base.union, [Set(b.id),
                        [reachable(block(b.ir, br.block))
                         for br in openbranches(nothing, b)
                         if br.block > b.id]...])

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
    if flag
      br = nothing
    elseif br.condition == nothing
      flag = true
    else
      cond = exprtype(nothing, ir, br.condition)
      if cond == true
        br = nothing
      elseif cond == false
        flag = true
        br = IRTools.branch(br.block)
      end
    end
    return br
  end
  IRTools.blocks(pr) do b
    flag = false
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

# Simply dynamic binding for recursive types

function withrecur(f, T)
  stack = get!(task_local_storage(), :recur, [])
  try
    push!(stack, T)
    f()
  finally
    pop!(stack)
  end
end

recur() = task_local_storage()[:recur][end]

# Union splitting

function union_cases!(f, ir, T::Or, x)
  j = push!(ir, Expr(:ref, x, 1))
  for case in 1:length(T.patterns)
    cond = push!(ir, xcall(WIntrinsic(i32.eq, i32), j, Int32(case)))
    branch!(ir, length(blocks(ir))+2, unless = cond)
    block!(ir)
    val = union_downcast!(ir, T, case, x)
    ret = f(T.patterns[case], val)
    return!(ir, ret)
    block!(ir)
  end
  push!(ir, xcall(WIntrinsic(WebAssembly.unreachable, ⊥)))
  return ir
end
