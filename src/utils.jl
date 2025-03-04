second(x) = x[2]
third(x) = x[3]

let sym = gensym(:deps)
  global deps() = get!(() -> [], task_local_storage(), sym)::Vector{Any}
end

function trackdeps(f, ::Type{T}) where T
  stack = deps()
  ds = Set{T}()
  push!(stack, ds)
  try
    return f(), ds
  finally
    pop!(stack)
  end
end

function track!(id, ::Type{T}) where T
  stack = deps()
  isempty(stack) || push!(stack[end]::Set{T}, id)
  return
end

mutable struct FixpointFrame{K,V}
  value::V
  deps::Set{K}
  edges::Set{K}
end

FixpointFrame{K,V}(val) where {K,V} =
  FixpointFrame{K,V}(val, Set{K}(), Set{K}())

struct Fixpoint{K,V,F,G,H}
  init::F
  update::G
  check::H
  frames::Dict{K,FixpointFrame{K,V}}
  queue::WorkQueue{K}
end

default_check(x, y) = return

Fixpoint{K,V}(update, init; check = default_check) where {K,V} =
  Fixpoint{K,V,typeof(init),typeof(update),typeof(check)}(init, update, check, Dict{K,V}(), WorkQueue{K}())

Fixpoint(update, init; check = default_check) =
  Fixpoint{Any,Any}(update, init; check)

struct Inner{F<:Fixpoint}
  fp::F
end

function cleardeps!(fp::Fixpoint, k)
  fr = fp.frames[k]
  for dep in fr.deps
    delete!(fp.frames[dep].edges, k)
  end
  empty!(fr.deps)
  return
end

function update!(fp::Fixpoint{K}, k) where K
  cleardeps!(fp, k)
  fr = fp.frames[k]
  val, deps = trackdeps(K) do
    fp.update(Inner(fp), k)
  end
  fp.check(fr.value, val)
  union!(fr.deps, deps)
  foreach(dep -> push!(fp.frames[dep].edges, k), deps)
  if val != fr.value
    fr.value = val
    foreach(s -> push!(fp.queue, s), fr.edges)
  end
  return
end

function Base.getindex(fp::Fixpoint{K,V}, k::K; loop = true) where {K,V}
  if !haskey(fp.frames, k)
    fp.frames[k] = fr = FixpointFrame{K,V}(fp.init(k))
    update!(fp, k)
  end
  loop && while !isempty(fp.queue)
    update!(fp, pop!(fp.queue))
  end
  return fp.frames[k].value
end

Base.getindex(fp::Inner, k) = getindex(fp.fp, k, loop = false)
