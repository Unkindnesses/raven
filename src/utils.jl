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

struct Fixpoint{K,V}
  init
  update
  frames::IdDict{K,Tuple{V,Set{K},Set{K}}}
  queue::WorkQueue{K}
end

Fixpoint{K,V}(update, init) where {K,V} = Fixpoint{K,V}(init, update, Dict(), WorkQueue{K}())
Fixpoint(update, init) = Fixpoint{Any,Any}(update, init)

struct Inner{K,V}
  fp::Fixpoint{K,V}
end

function Base.getindex(fp::Inner{K}, k) where K
  k = convert(K, k)
  track!(k)
  frame!(fp.fp, k)[1]
end

function frame!(fp::Fixpoint{K,V}, k::K) where {K,V}
  haskey(fp.frames, k) && return fp.frames[k]
  fp.frames[k] = (fp.init(k), Set(), Set())
  update!(fp, k)
  return fp.frames[k]
end

function cleardeps!(fp::Fixpoint, k)
  fr = fp.frames[k]
  for dep in fr[2]
    delete!(fp.frames[dep][3], k)
  end
  empty!(fr[2])
  return
end

function update!(fp::Fixpoint, k)
  cleardeps!(fp, k)
  fr = fp.frames[k]
  val, deps = trackdeps() do
    fp.update(Inner(fp), k)
  end
  union!(fr[2], deps)
  foreach(dep -> push!(fp.frames[dep][3], k), deps)
  if val != fr[1]
    fp.frames[k] = (val, fr[2], fr[3])
    foreach(s -> push!(fp.queue, s), fr[3])
  end
  return
end

function Base.getindex(fp::Fixpoint{K}, k) where {K}
  k = convert(K, k)
  frame!(fp, k)
  while !isempty(fp.queue)
    update!(fp, pop!(fp.queue))
  end
  return frame!(fp, k)[1]
end
