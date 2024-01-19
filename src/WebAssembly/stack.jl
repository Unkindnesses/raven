# Say the stack contains values [%6, %3, %2]. We need the top to be [%3, %4] for
# the next call. %3 is live afterwards; live variables must either be underneath
# the target stack or get moved to a local.
#
# In this case a good solution would be
#   drop
#   local.tee N
#   local.get M
#
# where N and M are the locals assigned to `%3` and `%2` respectively.
#
# A sequence of stack operations represents a path through the space of possible
# stacks. So we can use a path finding algorithm (A*) to find the optimal
# sequence / shortest path.
#
# We can generate stack ops as if all SSA variables had their own local slot,
# but only those for which a `local.get` was generated actually need one. Once
# we have sequences, we can run register allocation for those variables and fill
# in the `local.*` instructions accordingly.

using DataStructures: PriorityQueue, dequeue!
using ..IRTools: Variable

struct Locals{T}
  stack::Vector{Union{T,Const}}
  store::Set{T}
end

Base.:(==)(a::Locals, b::Locals) = (a.stack, a.store) == (b.stack, b.store)
Base.hash(ls::Locals, h::UInt64) = hash((ls.stack, ls.store), h ⊻ 0x205dbdcc02f546cb)

Locals(stack, store = Set{eltype(stack)}()) =
  Locals{Union{eltype(stack),eltype(store)}}(stack, store)

top(ls::Locals) = ls.stack[end]
top(ls::Locals, n) = ls.stack[max(1,end-n+1):end]
drop(ls::Locals) = typeof(ls)(ls.stack[1:end-1], ls.store)
set(ls::Locals) = typeof(ls)(ls.stack[1:end-1], push!(copy(ls.store), top(ls)))
tee(ls::Locals) = typeof(ls)(ls.stack, push!(copy(ls.store), top(ls)))
load(ls::Locals, v) = typeof(ls)([ls.stack..., v], ls.store)

matches(state::Locals, target::Locals; strict = false) =
  (!strict || length(state.stack) == length(target.stack)) &&
  top(state, length(target.stack)) == target.stack &&
  state.stack[end-length(target.stack)+1:end] == target.stack &&
  all(v -> v in state.stack[1:end-length(target.stack)] ||
           v in state.store,
      target.store)

# Lower bound number of ops needed to reach target
heuristic(state::Locals, target::Locals) =
  length(setdiff(target.stack, top(state, length(target.stack))))

function stackshuffle(locals::Locals, target::Locals; strict = false)
  stored = setdiff(target.stack, locals.stack)
  paths = Dict{Locals,Vector{Union{Instruction,Expr}}}()
  q = PriorityQueue{Locals,Tuple{Int,Int,Int}}()
  function visit!(locals, path)
    haskey(paths, locals) && return
    paths[locals] = path
    q[locals] = (length(path) + heuristic(locals, target), length(locals.stack), length(locals.store))
  end
  visit!(locals, [])
  while true
    locals = dequeue!(q)
    matches(locals, target; strict) && return paths[locals], locals
    if !isempty(locals.stack)
      if !(top(locals) in union(target.stack, target.store))
        visit!(drop(locals), [paths[locals]..., Drop()])
      end
      if !(top(locals) isa Const)
        visit!(set(locals), [paths[locals]..., Expr(:set, top(locals))])
        visit!(tee(locals), [paths[locals]..., Expr(:tee, top(locals))])
      end
    end
    for v in union(stored, locals.store)
      visit!(load(locals, v), [paths[locals]..., v isa Instruction ? v : Expr(:get, v)])
    end
  end
end
