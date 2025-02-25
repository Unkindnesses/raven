import ..IRTools.WorkQueue

struct CycleCacheValue{T}
  value::T
  id::NFT
  deps::Set{NFT}
  edges::Set{NFT}
  time::UInt64
end

CycleCacheValue{T}(value, id, deps, time) where T =
  CycleCacheValue{T}(value, id, deps, Set{NFT}(), time)

struct CycleCache{K,V}
  init::Any
  default::Any
  fingerprint::Set{NFT}
  keys::Base.Dict{NFT,K}
  data::IdDict{K,CycleCacheValue{V}}
  queue::WorkQueue{NFT}
end

CycleCache{K,V}(f, init) where {K,V} =
  CycleCache{K,V}(init, f, Set{NFT}(), Base.Dict{NFT,K}(), IdDict(), WorkQueue{NFT}())

CycleCache(f, init) = CycleCache{Any,Any}(f, init)

Base.show(io::IO, x::CycleCache) = print(io, typeof(x), "(...)")

struct Inner{C<:CycleCache}
  cc::C
end

keytype(::CycleCache{K,V}) where {K,V} = K
valtype(::CycleCache{K,V}) where {K,V} = V
keys(c::CycleCache) = keys(c.data)
fingerprint(c::CycleCache) = c.fingerprint

iscached(ch::CycleCache, k) = haskey(ch.data, k)
cached(ch::CycleCache, k) = ch.data[k].value
id(c::CycleCache, k) = c.data[k].id
deps(c::CycleCache, k) = c.data[k].deps
edges(c::CycleCache, k) = c.data[k].edges
time(c::CycleCache, k) = c.data[k].time

time(ch::CycleCache) = sum(v.time for (_, v) in ch.data)

function delete!(c::CycleCache{K,V}, k::K) where {K,V}
  iscached(c, k) || return
  v = c.data[k]
  delete!(c.fingerprint, v.id)
  delete!(c.keys, v.id)
  delete!(c.data, k)
  for i in v.deps
    haskey(c.keys, i) && delete!(edges(c, c.keys[i]), v.id)
  end
  for i in v.edges
    haskey(c.keys, i) && delete!(c, c.keys[i])
  end
  return c
end

function set!(c::CycleCache{K,V}, k::K, v::CycleCacheValue{V}) where {K,V}
  iscached(c, k) && @assert id(c, k) == v.id
  push!(c.fingerprint, v.id)
  c.keys[v.id] = k
  c.data[k] = v
  for id in v.deps
    haskey(c.keys, id) && push!(edges(c, c.keys[id]), v.id)
  end
  return v
end

function update!(c::CycleCache{K,V}, k) where {K,V}
  v = c.data[k]
  val, deps, time = trackdeps() do
    c.default(Inner(c), k)
  end
  time += v.time
  if !isequal(val, v.value)
    # TODO could clear old deps, but need to preserve those from `init`.
    set!(c, k, CycleCacheValue{V}(val, v.id, union(v.deps, deps), time))
    foreach(s -> push!(c.queue, s), edges(c, k))
  elseif !issubset(deps, v.deps)
    set!(c, k, CycleCacheValue{V}(val, v.id, union(v.deps, deps), time))
  end
  return
end

function getindex(c::CycleCache{K,V}, k::K; loop = true) where {K,V}
  if !haskey(c.data, k)
    val, deps, time = trackdeps(() -> c.init(k))
    set!(c, k, CycleCacheValue{V}(val, NFT(), deps, time))
    update!(c, k)
  end
  loop && while !isempty(c.queue)
    update!(c, c.keys[pop!(c.queue)])
  end
  track!(id(c, k))
  return cached(c, k)
end

function getindex(c::Inner{CycleCache{K,V}}, k::K) where {K,V}
  getindex(c.cc, k, loop = false)
end

function reset!(c::CycleCache; deps = [])
  print = fingerprint(deps)
  for k in keys(c)
    all(id -> id in print || id in fingerprint(c), Caches.deps(c, k)) ||
      delete!(c, k)
  end
end
