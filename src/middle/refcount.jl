# Reference Counting
#
# The unoptimised approach is:
# * Insert a `retain` whenever we pass a live variable as an argument.
# * Insert a `release` whenever a variable is dropped.
#
# Passing a variable as an argument means code like:
#
# ```
# x = ...
# y = f(x)
# ```
#
# Here `x` only needs to be retained if it is used again later on. If not,
# `f(x)` will free it. (Branch arguments are treated the same way.)
#
# Return values aren't dropped, so we don't need to `release` them before
# returning. The caller will take care of them. If `f(x) = x`, the code above
# doesn't actually need any counting instructions to be emitted. (If `f(x)`
# drops `x`, eg `f(x) = nothing`, we do need to release it before returning.)
#
# A variable can be dropped immediately (if it is never used) or after a branch
# (because the variable is not used in the current path). An edge case is when a
# block has multiple predecessors, one of which has multiple successors, such
# that the block only sometimes releases a preceding variable. For now this case
# is an error.

function liveness_after(block, lv)
  live = Set{Variable}()
  for b in successors(block)
    union!(live, setdiff(lv[b.id], arguments(b)))
  end
  return live
end

# Variables needed before each block is run, and after each statement is run.
# Block variables include their arguments.
function liveness(ir)
  result = Dict(v => Set{Variable}() for v in keys(ir))
  result = merge(result, Dict(b.id => Set{Variable}() for b in blocks(ir)))
  queue = WorkQueue(1:length(blocks(ir)))
  while !isempty(queue)
    b = block(ir, pop!(queue))
    live = liveness_after(b, result)
    for br in branches(b)
      foreach(x -> x isa Variable && push!(live, x), arguments(br))
      br.condition isa Variable && push!(live, br.condition)
    end
    for v in reverse(keys(b))
      union!(result[v], live)
      delete!(live, v)
      IRTools.Inner.varmap(x -> push!(live, x), b[v])
    end
    if !isempty(setdiff(live, result[b.id]))
      union!(result[b.id], live)
      foreach(b -> push!(queue, b.id), predecessors(b))
    end
  end
  return result
end

isreftype(::Union{Primitive,Type,Unreachable}) = false
isreftype(xs::Or) = any(isreftype, xs.patterns)
isreftype(xs::Pack) = any(isreftype, xs.parts)
isreftype(x::VPack) = layout(x.parts) != ()
isreftype(x::Recursive) = true

isglobal(ir, v) = haskey(ir, v) && isexpr(ir[v].expr, :global)

function retain(f, T::VPack, x)
  ptr = f(stmt(Expr(:ref, x, 2), type = rlist(pack(:Ptr, Int32))))
  f(stmt(xcall(:retain!, ptr), type = pack(:Nil)))
end

function retain(f, T::Recursive, x)
  ptr = f(stmt(Expr(:ref, x, 1), type = rlist(pack(:Ptr, Int32))))
  f(stmt(xcall(:retain!, ptr), type = pack(:Nil)))
end

function retain(f, T::Pack, x)
  for i = 0:nparts(T)
    isreftype(part(T, i)) || continue
    p = _indexer!(f, T, i, x)
    retain(f, part(T, i), p)
  end
end

function release(f, T::VPack, x)
  ptr = f(stmt(Expr(:ref, x, 2), type = rlist(pack(:Ptr, Int32))))
  f(stmt(xcall(:release!, ptr), type = pack(:Nil)))
end

function retain(f, T::Or, x)
  # TODO implement
end

function release(f, T::Or, x)
  # TODO implement
end

function release(f, T::Recursive, x)
  ptr = f(stmt(Expr(:ref, x, 1), type = rlist(pack(:Ptr, Int32))))
  f(stmt(xcall(:release!, ptr), type = pack(:Nil)))
end

function release(f, T::Pack, x)
  for i = 0:nparts(T)
    isreftype(part(T, i)) || continue
    p = _indexer!(f, T, i, x)
    release(f, part(T, i), p)
  end
end

function refcounts!(ir)
  lv = liveness(ir)
  isref(v) = isreftype(IRTools.exprtype(ir, v))
  # Would be better to do this as part of the pipe – fix this in IRTools2.
  for b in IRTools.blocks(ir)
    # Could pushfirst! to free earlier, but this causes issues with multiple
    # statements.
    rel(x) = isref(x) && release(ex -> push!(b, ex), IRTools.exprtype(ir, x), x)
    # unused block arguments
    foreach(rel, filter(x -> !(x in lv[b.id]), arguments(b)))
    # conditionally dropped variables
    dropped = [filter(isref, setdiff(liveness_after(c, lv), lv[b.id])) for c in predecessors(b)]
    if !isempty(dropped)
      @assert all(xs -> xs == dropped[1], dropped) # condition mentioned above
      foreach(rel, dropped[1])
    end
    lafter = liveness_after(b, lv)
    for br in branches(b)
      # reused branch args
      @assert !any(x -> x in lafter, filter(isref, arguments(br)))
    end
  end
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    if isexpr(st.expr, :release)
      delete!(pr, v)
      x = st.expr.args[1]
      isref(x) && !(x in lv[v]) || continue
      release(ex -> push!(pr, ex), IRTools.exprtype(ir, x), x)
    elseif isexpr(st.expr, :retain)
      delete!(pr, v)
      x = st.expr.args[1]
      isref(x) || continue
      retain(ex -> push!(pr, ex), IRTools.exprtype(ir, x), x)
    elseif isexpr(st.expr, :call, :tuple) && haskey(lv, v)
      # dropped variable
      isref(v) && (v in lv[v] || release(ex -> push!(pr, ex), IRTools.exprtype(ir, v), v))
      # reused argument
      for x in unique(st.expr.args)
        x isa Variable && isref(x) || continue
        ret = isglobal(ir, x) + count(==(x), st.expr.args) - !(x in lv[v])
        for _ = 1:ret
          retain(ex -> insert!(pr, v, ex), IRTools.exprtype(ir, x), x)
        end
      end
    end
  end
  ir = IRTools.finish(pr)
  return ir
end

function refcounts(c::Compilation)
  comp = Compilation(c.mod)
  for (k, ir) in c.frames
    comp.frames[k] = ir |> copy |> refcounts!
  end
  return comp
end
