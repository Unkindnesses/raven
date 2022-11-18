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

# Define refcount methods for different types
# ===========================================

@enum CountMode retain release

# Used as a key for generated methods
retain_method = RMethod(:retain, lowerpattern(rvx"args"), nothing, false)
release_method = RMethod(:release, lowerpattern(rvx"args"), nothing, false)

function countptr!(ir, ptr, mode)
  f = mode == retain ? :retain! : :release!
  push!(ir, stmt(xcall(f, ptr), type = nil))
end

function count!(cx, ir, T::Pack, x, mode)
  for i = 0:nparts(T)
    isreftype(part(T, i)) || continue
    p = indexer!(ir, T, i, x, nothing)
    f = mode == retain ? retain! : release!
    f(cx, ir, part(T, i), p)
  end
end

function count!(cx, ir, T::VPack, x, mode)
  # TODO release children
  ptr = push!(ir, stmt(Expr(:ref, x, 2), type = rlist(pack(:Ptr, Int32))))
  countptr!(ir, ptr, mode)
end

function count!(cx, ir, T::Or, x, mode)
  union_cases!(ir, T, x) do T, x
    f = mode == retain ? retain! : release!
    if isreftype(T)
      f(cx, ir, T, x)
    else
      push!(ir, stmt(Expr(:tuple), type = nil))
    end
  end
end

function count!(cx, ir, T::Recursive, x, mode)
  ptr = push!(ir, stmt(Expr(:ref, x, 1), type = rlist(pack(:Ptr, Int32))))
  if mode == release
    unique = push!(ir, stmt(xcall(:blockUnique, ptr), type = rlist(Int32)))
    unique = push!(ir, Expr(:ref, unique, 1))
    branch!(ir, length(blocks(ir))+2, unless = unique)
    block!(ir)
    T = unroll(T)
    inner = unbox!(ir, T, x, count = false)
    release!(cx, ir, T, inner)
    block!(ir)
  end
  countptr!(ir, ptr, mode)
end

function count_ir(cx, T, mode)
  ir = IR()
  x = argument!(ir, type = T)
  count!(cx, ir, T, x, mode)
  return!(ir, push!(ir, stmt(xtuple(), type = nil)))
end

function retain!(cx, ir, T, x)
  sig = (retain_method, T)
  if !haskey(cx.frames, sig)
    cx.frames[sig] = IR()
    cx.frames[sig] = count_ir(cx, T, retain)
  end
  push!(ir, stmt(xcall(retain_method, x), type = nil))
end

function release!(cx, ir, T, x)
  sig = (release_method, T)
  if !haskey(cx.frames, sig)
    cx.frames[sig] = IR()
    cx.frames[sig] = count_ir(cx, T, release)
  end
  push!(ir, stmt(xcall(release_method, x), type = nil))
end

# Actually insert counting instructions into code
# ===============================================

function refcounts!(cx, ir)
  lv = liveness(ir)
  isref(v) = isreftype(IRTools.exprtype(ir, v))
  # Would be better to do this as part of the pipe – fix this in IRTools2.
  for b in IRTools.blocks(ir)
    # Could pushfirst! to free earlier, but this causes issues with multiple
    # statements.
    rel(x) = isref(x) && release!(cx, b, IRTools.exprtype(ir, x), x)
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
      release!(cx, pr, IRTools.exprtype(ir, x), x)
    elseif isexpr(st.expr, :retain)
      delete!(pr, v)
      x = st.expr.args[1]
      isref(x) || continue
      retain!(cx, pr, IRTools.exprtype(ir, x), x)
    elseif isexpr(st.expr, :call, :tuple) && haskey(lv, v)
      delete!(pr, v)
      # reused argument
      for x in unique(st.expr.args)
        x isa Variable && isref(x) || continue
        ret = isglobal(ir, x) + count(==(x), st.expr.args) - !(x in lv[v])
        for _ = 1:ret
          retain!(cx, pr, IRTools.exprtype(ir, x), x)
        end
      end
      v′ = push!(pr, st)
      replace!(pr, v, v′)
      # dropped variable
      isref(v) && (v in lv[v] || release!(cx, pr, IRTools.exprtype(ir, v), v′))
    end
  end
  ir = IRTools.finish(pr)
  return ir
end

function refcounts(c::Compilation)
  comp = Compilation(c.mod)
  for (k, ir) in c.frames
    comp.frames[k] =
      ir isa Redirect ? ir : refcounts!(comp, copy(ir))
  end
  return comp
end
