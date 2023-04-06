# Cache

import Base: getindex, setindex!, haskey, get

struct Cache{K,V}
  default::Any
  data::IdDict{K,V}
end

Cache{K,V}(f) where {K,V} = Cache{K,V}(f, Dict{K,V}())

Cache(f) = Cache{Any,Any}(f)

setindex!(c::Cache{K,V}, v::V, k::K) where {K,V} = c.data[k] = v

getindex(c::Cache{K,V}, k::K) where {K,V} =
  haskey(c.data, k) ? c.data[k] : (c.data[k] = c.default(c, k))

haskey(c::Cache, k) = haskey(c.data, k)

get(c::Cache, k, v) = get(c.data, k, v)

# Unique Work Queue

struct WorkQueue{T}
  items::Vector{T}
end

WorkQueue{T}() where T = WorkQueue(T[])

WorkQueue() = WorkQueue{Any}()

WorkQueue(xs) = WorkQueue(collect(xs))

function Base.delete!(q::WorkQueue, x)
  i = findfirst(y -> x === y, q.items)
  i === nothing || (deleteat!(q.items, i))
  return q
end

function Base.push!(q::WorkQueue, x)
  delete!(q, x)
  push!(q.items, x)
  return q
end

function Base.pushfirst!(q::WorkQueue, x)
  delete!(q, x)
  pushfirst!(q.items, x)
  return q
end

Base.pop!(q::WorkQueue) = pop!(q.items)
Base.isempty(q::WorkQueue) = isempty(q.items)

# Trim unreachable code

reachable(b::IRTools.Block) =
  any(((v, st),) -> !isexpr(st, :branch) && st.type == ⊥, b) ? Set(b.id) :
    reduce(Base.union, [Set(b.id),
                        [reachable(block(b.ir, br.args[1]))
                         for br in openbranches(b)
                         if br.args[1] > b.id]...])

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
  for (v, st) in pr
    if v == first(IRTools.block(ir, v))[1]
      flag = false
    end
    if isexpr(st, :branch)
      br = st.expr
      if flag
        pr[v] = IRTools.unreachable
      elseif br.args[2] == nothing
        flag = true
      else
        cond = exprtype(ir, br.args[2])
        if cond == false
          delete!(pr, v)
        elseif cond == true
          flag = true
          pr[v] = IRTools.branch(br.args[1], arguments(br)...)
        end
      end
    elseif flag
      delete!(pr, v)
    elseif st.type == ⊥
      flag = true
    end
  end
  return IRTools.finish(pr)
end

# Simple dynamic binding for recursive types

function dynamic_bind(f, k, v)
  stack = get!(task_local_storage(), k, [])
  try
    push!(stack, v)
    f()
  finally
    pop!(stack)
  end
end

dynamic_value(k) = task_local_storage()[k][end]

function dynamic_value(k, default)
  tls = task_local_storage()
  (!haskey(tls, k) || isempty(tls[k])) ? default :
  tls[k][end]
end

withpath(f, p) = dynamic_bind(f, :path, p)
path() = dynamic_value(:path)

# Compiler options

struct Options
  # checkAllocations() call after main
  memcheck::Bool
  # Use JS interop for error handling
  jspanic::Bool
end

Options(; memcheck = true, jspanic = true) =
  Options(memcheck, jspanic)

withoptions(f, p) = dynamic_bind(f, :options, p)
options() = dynamic_value(:options, Options())::Options

# Union splitting

function union_downcast!(ir, T::Or, i::Integer, x)
  offset = sum(length, layout.(T.patterns[1:i-1]), init = 0)+1
  parts = [push!(ir, Expr(:ref, x, j+offset)) for j = 1:length(layout(T.patterns[i]))]
  return layout(T.patterns[i]) isa Tuple ? push!(ir, stmt(Expr(:tuple, parts...), type = T.patterns[i])) : parts[1]
end

# `f` is reponsible for freeing its argument value, but not for freeing `x`
# (since they are the same object)
function union_cases!(f, ir, T::Or, x)
  j = push!(ir, Expr(:ref, x, 1))
  for case in 1:length(T.patterns)
    cond = push!(ir, xcall(WIntrinsic(i32.eq, i32), j, Int32(case)))
    branch!(ir, length(blocks(ir))+1, when = cond)
    branch!(ir, length(blocks(ir))+2)
    block!(ir)
    val = union_downcast!(ir, T, case, x)
    ret = f(T.patterns[case], val)
    return!(ir, ret)
    block!(ir)
  end
  push!(ir, xcall(WIntrinsic(WebAssembly.unreachable, ⊥)))
  return ir
end

function log!(ir, s::String)
  s = push!(ir, stmt(Expr(:ref, s), type = rlist(String)))
  push!(ir, stmt(xcall(tag"println", s), type = nil))
end
