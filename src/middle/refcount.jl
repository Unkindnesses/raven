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
# TODO rethink for branch statements
function liveness(ir)
  result = Dict(v => Set{Variable}() for v in keys(ir))
  result = merge(result, Dict(b.id => Set{Variable}() for b in blocks(ir)))
  queue = WorkQueue(1:length(blocks(ir)))
  while !isempty(queue)
    b = block(ir, pop!(queue))
    live = liveness_after(b, result)
    for br in branches(b)
      foreach(x -> x isa Variable && push!(live, x), arguments(br))
      br.args[2] isa Variable && push!(live, br.args[2])
    end
    for v in reverse(keys(b))
      isexpr(ir[v], :branch) && continue
      union!(result[v], live)
      delete!(live, v)
      IRTools.varmap(x -> push!(live, x), b[v])
    end
    if !isempty(setdiff(live, result[b.id]))
      union!(result[b.id], live)
      foreach(b -> push!(queue, b.id), predecessors(b))
    end
  end
  return result
end

isrefobj(x::Pack) = tag(x) == :Ref

isreftype(::Union{Primitive,Type,Unreachable}) = false
isreftype(xs::Or) = any(isreftype, xs.patterns)
isreftype(xs::Pack) = isrefobj(xs) || any(isreftype, xs.parts)
isreftype(x::VPack) = layout(x.parts) != ()
isreftype(x::Recursive) = true

isglobal(ir, v) = haskey(ir, v) && isexpr(ir[v].expr, :global)

# Define refcount methods for different types
# ===========================================

@enum CountMode retain release

# Used as a key for generated methods
retain_method = RMethod(:retain, lowerpattern(rvx"args"), nothing, false)
release_method = RMethod(:release, lowerpattern(rvx"args"), nothing, false)

function retain!(ir, x)
  push!(ir, stmt(xcall(retain_method, x), type = nil))
end

function release!(ir, x)
  push!(ir, stmt(xcall(release_method, x), type = nil))
end

function countptr!(ir, ptr, mode)
  f = mode == retain ? :retain! : :release!
  push!(ir, stmt(xcall(f, ptr), type = nil))
end

function count!(ir, T::Pack, x, mode)
  if isrefobj(T)
    P = partial_part(T, 1)
    ptr = indexer!(ir, T, 1, x, nothing)
    ptr = push!(ir, stmt(Expr(:tuple, ptr), type = rlist(P)))
    if mode == release
      cleanup = push!(ir, stmt(xcall(:i32load, ptr), type = Int32))
      args = push!(ir, stmt(Expr(:tuple, ptr, cleanup), type = rlist(P, Int32)))
      push!(ir, stmt(xcall(:release!, args), type = nil))
    else
      push!(ir, stmt(xcall(:retain!, ptr), type = nil))
    end
  else
    for i = 0:nparts(T)
      isreftype(part(T, i)) || continue
      p = indexer!(ir, T, i, x, nothing)
      f = mode == retain ? retain! : release!
      f(ir, p)
    end
  end
end

function count!(ir, T::VPack, x, mode)
  # TODO release children
  ptr = push!(ir, stmt(Expr(:ref, x, 2), type = rlist(pack(:Ptr, Int32))))
  countptr!(ir, ptr, mode)
end

function count!(ir, T::Or, x, mode)
  union_cases!(ir, T, x) do T, x
    f = mode == retain ? retain! : release!
    if isreftype(T)
      f(ir, x)
    else
      push!(ir, stmt(Expr(:tuple), type = nil))
    end
  end
end

function count!(ir, T::Recursive, x, mode)
  ptr = push!(ir, stmt(Expr(:ref, x, 1), type = rlist(pack(:Ptr, Int32))))
  if mode == release
    unique = push!(ir, stmt(xcall(:blockUnique, ptr), type = rlist(Int32)))
    unique = push!(ir, Expr(:ref, unique, 1))
    branch!(ir, length(blocks(ir))+1, when = unique)
    branch!(ir, length(blocks(ir))+2)
    block!(ir)
    T = unroll(T)
    inner = unbox!(ir, T, x, count = false)
    release!(ir, inner)
    branch!(ir, length(blocks(ir))+1)
    block!(ir)
  end
  countptr!(ir, ptr, mode)
end

function count_ir(T, mode)
  ir = IR(meta = FuncInfo(Symbol(mode)))
  x = argument!(ir, type = T)
  count!(ir, T, x, mode)
  return!(ir, push!(ir, stmt(xtuple(), type = nil)))
end

# Insert counting instructions
# ============================

function refcounts!(ir)
  lv = liveness(ir)
  isref(v) = isreftype(IRTools.exprtype(ir, v))
  # Would be better to do this as part of the pipe – fix this in IRTools2.
  for b in IRTools.blocks(ir)
    # TODO: could pushfirst! to free earlier, but this causes issues with multiple
    # statements. This is an issue if the block errors.
    rel(x) = isref(x) && release!(b, x)
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
      for x in unique(arguments(br))
        x isa Variable && isref(x) || continue
        ret = isglobal(ir, x) + count(==(x), arguments(br)) - !(x in lafter)
        for _ = 1:ret
          retain!(b, x)
        end
      end
    end
  end
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    if isexpr(st, :release)
      delete!(pr, v)
      x = st.expr.args[1]
      isref(x) && !(x in lv[v]) || continue
      release!(pr, x)
    elseif isexpr(st, :retain)
      delete!(pr, v)
      x = st.expr.args[1]
      isref(x) && x in lv[v] || continue
      retain!(pr, x)
    elseif isexpr(st, :call, :tuple) && haskey(lv, v)
      delete!(pr, v)
      # reused argument
      for x in unique(st.expr.args)
        x isa Variable && isref(x) || continue
        ret = isglobal(ir, x) + count(==(x), st.expr.args) - !(x in lv[v])
        for _ = 1:ret
          retain!(pr, x)
        end
      end
      v′ = push!(pr, st)
      replace!(pr, v, v′)
      # dropped variable
      isref(v) && (v in lv[v] || release!(pr, v′))
    end
  end
  ir = IRTools.finish(pr)
  return ir
end

function refcounts(c::Cache)
  Cache{Any,Union{Redirect,IR}}() do cx, sig
    if sig[1] in (retain_method, release_method)
      count_ir(sig[2], sig[1] == retain_method ? retain : release)
    else
      ir = c[sig]
      ir isa Redirect ? ir : refcounts!(copy(ir))
    end
  end
end
