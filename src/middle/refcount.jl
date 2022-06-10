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

function refcounts!(ir)
  pr = IRTools.Pipe(ir)
  lv = liveness(ir)
  for b in IRTools.blocks(ir)
    # unused block arguments
    foreach(x -> pushfirst!(b, xcall(:release, x)), filter(x -> !(x in lv[b.id]), arguments(b)))
    # conditionally dropped variables
    dropped = [setdiff(liveness_after(c, lv), lv[b.id]) for c in predecessors(b)]
    if !isempty(dropped)
      @assert all(xs -> xs == dropped[1], dropped) # condition mentioned above
      foreach(x -> pushfirst!(b, xcall(:release, x)), dropped[1])
    end
    lafter = liveness_after(b, lv)
    for br in branches(b)
      # reused branch args
      @assert !any(x -> x in lafter, arguments(br))
    end
  end
  for (v, st) in pr
    haskey(lv, v) || continue
    # dropped variable
    v in lv[v] || push!(pr, xcall(:release, v))
    # reused argument
    for x in st.expr.args
      x isa Variable || continue
      x in lv[v] && insert!(pr, v, xcall(:retain, x))
    end
  end
  ir = IRTools.finish(pr)
  return ir
end
