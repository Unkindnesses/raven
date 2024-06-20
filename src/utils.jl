second(x) = x[2]
third(x) = x[3]

let sym = gensym(:deps)
  global deps() = get!(() -> Set{Any}[], task_local_storage(), sym)::Vector{Set{Any}}
end

function trackdeps(f)
  stack = deps()
  ds = Set()
  push!(stack, ds)
  try
    return f(), ds
  finally
    pop!(stack)
  end
end

function track!(id)
  stack = deps()
  isempty(stack) || push!(stack[end], id)
  return
end

mutable struct FixpointFrame{K,V}
  value::V
  deps::Set{K}
  edges::Set{K}
  active::Bool
end

FixpointFrame{K,V}(val) where {K,V} =
  FixpointFrame{K,V}(val, Set(), Set(), false)

struct Fixpoint{K,V}
  init
  update
  check
  frames::IdDict{K,FixpointFrame{K,V}} # val, deps, edges
  queue::WorkQueue{K}
end

default_check(x, y) = return

Fixpoint{K,V}(update, init; check = default_check) where {K,V} =
  Fixpoint{K,V}(init, update, check, Dict(), WorkQueue{K}())

Fixpoint(update, init; check = default_check) =
  Fixpoint{Any,Any}(update, init; check)

struct Inner{K,V}
  fp::Fixpoint{K,V}
end

function cleardeps!(fp::Fixpoint, k)
  fr = fp.frames[k]
  for dep in fr.deps
    delete!(fp.frames[dep].edges, k)
  end
  empty!(fr.deps)
  return
end

function update!(fp::Fixpoint, k)
  yield()
  cleardeps!(fp, k)
  fr = fp.frames[k]
  try
    fr.active = true
    val, deps = trackdeps() do
      fp.update(Inner(fp), k)
    end
    fp.check(fr.value, val)
    union!(fr.deps, deps)
    foreach(dep -> push!(fp.frames[dep].edges, k), deps)
    if val != fr.value
      fr.value = val
      foreach(s -> push!(fp.queue, s), fr.edges)
    end
  finally
    fr.active = false
  end
  return
end

function frame!(fp::Fixpoint{K,V}, k::K) where {K,V}
  fr = get!(fp.frames, k) do
    push!(fp.queue, k)
    FixpointFrame{K,V}(fp.init(k))
  end
  if !fr.active
    while k in fp.queue.items
      delete!(fp.queue, k)
      update!(fp, k)
    end
  end
  return fr
end

function Base.getindex(fp::Inner{K}, k) where K
  k = convert(K, k)
  track!(k)
  frame!(fp.fp, k).value
end

function Base.getindex(fp::Fixpoint{K}, k) where {K}
  k = convert(K, k)
  frame!(fp, k)
  while !isempty(fp.queue)
    update!(fp, popfirst!(fp.queue))
  end
  return frame!(fp, k).value
end
