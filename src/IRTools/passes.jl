struct CFG
  graph::Vector{Vector{Int}}
end

function CFG(ir::IR)
  graph = Vector{Int}[]
  for b in blocks(ir)
    push!(graph, Int[])
    for c in successors(b)
      push!(graph[end], c.id)
    end
  end
  return CFG(graph)
end

Base.:(==)(a::CFG, b::CFG) = a.graph == b.graph

Base.length(c::CFG) = length(c.graph)
Base.getindex(c::CFG, b::Integer) = c.graph[b]

function definitions(b::Block)
  defs = [Variable(i) for i = 1:length(b.ir.defs) if b.ir.defs[i][1] == b.id]
end

function usages(b::Block)
  uses = Set{Variable}()
  prewalk(b) do x
    x isa Variable && push!(uses, x)
    return x
  end
  return uses
end

function usecounts(ir::IR)
  counts = Dict{Variable,Int}()
  prewalk(ir) do x
    x isa Variable && (counts[x] = get(counts, x, 0)+1)
    return x
  end
  return counts
end

function renumber(ir)
  p = Pipe(ir)
  for (v, st) in p
    ex = st.expr
    if isbits(ex) # Trivial expressions can be inlined
      delete!(p, v)
      substitute!(p, v, substitute(p, ex))
    end
  end
  return finish(p)
end

function expand!(ir::IR)
  worklist = blocks(ir)
  spats = Dict(b => Dict() for b in blocks(ir))
  while !isempty(worklist)
    b = pop!(worklist)
    b.id == 1 && continue
    defs = definitions(b)
    uses = usages(b)
    for v in setdiff(uses, defs)
      haskey(spats[b], v) && continue
      spats[b][v] = argument!(b, v, type = exprtype(ir, v))
      for c in predecessors(b)
        c in worklist || push!(worklist, c)
      end
    end
    ir.blocks[b.id] = prewalk(x -> get(spats[b], x, x), ir.blocks[b.id])
  end
  return ir
end

function prune!(ir::IR)
  usages = usecounts(ir)
  worklist = blocks(ir)
  queue!(b) = (b in worklist || push!(worklist, b))
  function rename!(env, ir)
    for b in blocks(ir)
      # TODO avoid walk
      prewalk!(b) do x
        haskey(env, x) || return x
        foreach(queue!, successors(b))
        env[x]
      end
    end
  end
  while !isempty(worklist)
    b = popfirst!(worklist)
    isempty(arguments(b)) && continue
    brs = filter(br -> br.args[1] == b.id, [br for a in blocks(ir) for br in branches(a)])
    isempty(brs) && continue
    # Redundant due to all inputs being the same
    inputs = [setdiff(in, (a,)) for (a, in) in zip(arguments(b), zip(arguments.(brs)...))]
    del = findall(x -> length(x) == 1, inputs)
    rename = Dict(zip(arguments(b)[del], first.(inputs[del])))
    if !isempty(rename)
      deletearg!(b, del)
      rename!(rename, ir)
    end
    # Redundant due to not being used
    unused = findall(x -> get(usages, x, 0) == 0, arguments(b))
    if !isempty(unused)
      for a in predecessors(b)
        for br in branches(a, b), i in unused
          arguments(br)[i] isa Variable &&
            (usages[arguments(br)[i]] -= 1)
        end
        a in worklist || push!(worklist, a)
      end
      deletearg!(b, unused)
    end
  end
  return ir
end

function ssa!(ir::IR)
  current = 1
  defs = Dict(b => Dict{Slot,Any}() for b in 1:length(ir.blocks))
  todo = Dict(b => Dict{Int,Vector{Slot}}() for b in 1:length(ir.blocks))
  catches = Dict()
  handlers = []
  function reaching(b, v)
    haskey(defs[b.id], v) && return defs[b.id][v]
    b.id == 1 && return undef
    x = defs[b.id][v] = argument!(b, type = v.type, insert = false)
    for pred in predecessors(b)
      if pred.id < current
        for br in branches(pred, b)
          push!(br.args, reaching(pred, v))
        end
      else
        push!(get!(todo[pred.id], b.id, Slot[]), v)
      end
    end
    return x
  end
  function catchbranch!(v, slot = nothing)
    for handler in handlers
      args = reaching.((block(ir, v),), catches[handler])
      insertafter!(ir, v, Expr(:catch, handler, args...))
    end
  end
  for b in blocks(ir)
    current = b.id
    rename(ex) = prewalk(x -> x isa Slot ? reaching(b, x) : x, ex)
    for (v, st) in b
      ex = st.expr
      if isexpr(ex, :(=)) && ex.args[1] isa Slot
        defs[b.id][ex.args[1]] = rename(ex.args[2])
        catchbranch!(v, ex.args[1])
        delete!(ir, v)
      elseif isexpr(ex, :enter)
        catches[ex.args[1]] = slotsused(block(ir, ex.args[1]))
        push!(handlers, ex.args[1])
        catchbranch!(v)
      elseif isexpr(ex, :leave) && !haskey(catches, current)
        pop!(handlers)
      else
        ir[v] = rename(ex)
      end
    end
    for (succ, ss) in todo[b.id], br in branches(b, succ)
      append!(br.args, [reaching(b, v) for v in ss])
    end
  end
  return ir
end

# SCCs

function strongconnected(cfg; blocks = 1:length(cfg))
  preorder = zeros(Int, length(cfg))
  S, P = Int[], Int[]
  C = Ref(0)
  components = Vector{Int}[]
  let # avoid boxing `search`
    function search(v)
      preorder[v] = (C[] += 1)
      push!(S, v)
      push!(P, v)
      for w in cfg[v]
        w in blocks || continue
        if preorder[w] == 0
          search(w)
        elseif !any(c -> w in c, components)
          while preorder[P[end]] > preorder[w] pop!(P) end
        end
      end
      if P[end] == v
        pop!(P)
        pushfirst!(components, Int[])
        while true
          push!(components[1], pop!(S))
          components[1][end] == v && break
        end
      end
    end
    for b in blocks
      preorder[b] == 0 && search(b)
    end
  end
  return components
end

struct Component
  children::Vector{Union{Component,Int}}
end

blocks(c::Integer) = [c]
blocks(c::Component) = reduce(vcat, blocks.(c.children))

entry(c::Integer) = c
entry(c::Component) = first(c.children)::Int

Base.iterate(c::Component, st...) = iterate(c.children, st...)

function components(cfg::CFG; blocks = 1:length(cfg))
  if length(blocks) == 1
    bl = only(blocks)
    # Singleton components represent one-block loops
    bl in cfg[bl] ? Component([bl]) : bl
  else
    # Assume the first block is the entry.
    cs = strongconnected(cfg, blocks = blocks[2:end])
    Component([blocks[1],
              [components(cfg, blocks = sort(c)) for c in cs]...])
  end
end

# Inlining

function inlinehere!(ir, source, args...)
  @assert length(blocks(source)) == 1
  env = Dict()
  rename(x::Variable) = env[x]
  rename(x::Expr) = Expr(x.head, rename.(x.args)...)
  rename(x::Statement) = stmt(x, expr = rename(x.expr))
  rename(x) = x
  for (name, arg) in zip(arguments(source), args)
    env[name] = arg
  end
  for (v, st) in source
    isexpr(st, :branch) && continue
    env[v] = push!(ir, rename(st))
  end
  b = block(source, 1)
  return b[end].expr == unreachable ? nothing :
    rename(returnvalue(b))
end

# Liveness

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
    for v in reverse(keys(b))
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
