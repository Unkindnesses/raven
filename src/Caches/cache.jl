struct CacheValue{T}
  value::T
  id::Pair{NFT,NFT}
  deps::Set{Pair{NFT,NFT}}
end

struct Cache{K,V}
  fingerprint::Set{NFT}
  data::IdDict{K,CacheValue{V}}
  keys::Base.Dict{NFT,K}
  default::Any
end

Cache{K,V}(f) where {K,V} =
  Cache{K,V}(Set{NFT}(), IdDict{K,V}(), Base.Dict{NFT,K}(), f)

Cache(f) = Cache{Any,Any}(f)

keys(c::Cache) = keys(c.data)
keytype(c::Cache{K,V}) where {K,V} = K
valtype(c::Cache{K,V}) where {K,V} = V
fingerprint(ch::Cache) = ch.fingerprint

iscached(ch::Cache, k) = haskey(ch.data, k)
cached(ch::Cache, k) = ch.data[k].value
id(c::Cache, k) = c.data[k].id
deps(c::Cache, k) = c.data[k].deps

function Base.delete!(c::Cache{K,V}, k::K) where {K,V}
  haskey(c.data, k) || return
  ki, vi = id(c, k)
  delete!(c.keys, ki)
  delete!(c.fingerprint, vi)
  delete!(c.data, k)
  return c
end

function set!(c::Cache{K,V}, k::K, v::CacheValue{V}) where {K,V}
  delete!(c, k)
  ki, vi = v.id
  c.keys[ki] = k
  push!(c.fingerprint, vi)
  c.data[k] = v
  return v
end

function set!(c::Cache{K,V}, k::K, v::V; deps = Set()) where {K,V}
  ki = iscached(c, k) ? id(c, k)[1] : NFT()
  set!(c, k, CacheValue{V}(v, ki=>NFT(), deps))
  return v
end

function value(c::Cache{K,V}, k::K; self = c) where {K,V}
  trackdeps() do
    convert(V, c.default(self, k))::V
  end
end

function getindex(c::Cache{K,V}, k::K) where {K,V}
  if !iscached(c, k)
    v, deps = value(c, k)
    set!(c, k, v; deps)
  end
  track!(id(c, k))
  return cached(c, k)
end

function topokeys!(c::Cache, k::NFT, ks, seen)
  (haskey(c.keys, k) && !(k in seen)) || return
  push!(seen, k)
  for (k, _) in c.data[c.keys[k]].deps
    topokeys!(c, k, ks, seen)
  end
  push!(ks, c.keys[k])
  return
end

function topokeys(c::Cache)
  ks = keytype(c)[]
  seen = Set{NFT}()
  for (k, _) in c.keys
    topokeys!(c, k, ks, seen)
  end
  return ks
end

function invalid(c::Cache; deps = [])
  print = fingerprint(deps)
  keys = keytype(c)[]
  rm = Set{NFT}()
  for k in topokeys(c)
    v = c.data[k]
    all(((_, id),) -> id in print || (id in c.fingerprint && !(id in rm)), v.deps) && continue
    push!(rm, v.id[2])
    push!(keys, k)
  end
  return keys
end

reset!(c::Cache; deps = []) = foreach(k -> delete!(c, k), invalid(c; deps))

Base.IdDict(c::Cache) = IdDict(k => v.value for (k, v) in c.data)

# Eagerly updated version

struct EagerCache{K,V}
  cache::Cache{K,V}
  EagerCache{K,V}(c::Cache{K,V}) where {K,V} = new{K,V}(c)
  EagerCache(c::Cache{K,V}) where {K,V} = new{K,V}(c)
end

EagerCache(args...) = EagerCache(Cache(args...))
EagerCache{K,V}(args...) where {K,V} = EagerCache{K,V}(Cache{K,V}(args...))

getindex(c::EagerCache, args...) = getindex(c.cache, args...)

id(c::EagerCache, k) = id(c.cache, k)

fingerprint(c::EagerCache) = fingerprint(c.cache)

function reset!(c::EagerCache{K,V}; deps = []) where {K,V}
  for k in invalid(c.cache; deps)
    v, deps = value(c.cache, k)
    if isequal(v, cached(c.cache, k))
      set!(c.cache, k, CacheValue{V}(v, id(c, k), deps))
    else
      set!(c.cache, k, v; deps)
    end
  end
end

# Fixed point iteration

struct CycleCache{K,V}
  init::Any
  cache::Cache{K,V}
  edges::Base.Dict{NFT,Set{NFT}}
end

CycleCache(f, init) = CycleCache(init, Cache(f), Base.Dict{NFT,Set{NFT}}())
CycleCache{K,V}(f, init) where {K,V} = CycleCache{K,V}(init, Cache{K,V}(f), Base.Dict{NFT,Set{NFT}}())

valtype(c::CycleCache) = valtype(c.cache)
fingerprint(c::CycleCache) = fingerprint(c.cache)
keys(c::CycleCache) = keys(c.cache)

id(c::CycleCache, k) = id(c.cache, k)
deps(c::CycleCache, k) = deps(c.cache, k)
iscached(c::CycleCache, k) = iscached(c.cache, k)
cached(c::CycleCache, k) = cached(c.cache, k)
value(c::CycleCache, k) = value(c.cache, k, self = c)

edges(c::CycleCache, k::NFT) = get!(() -> Set{NFT}(), c.edges, k)

function set!(c::CycleCache{K,V}, k::K, v::CacheValue{V}) where {K,V}
  iscached(c, k) && (id(c, k) == v.id || delete!(c, k))
  set!(c.cache, k, v)
  ki = v.id[1]
  for (k, _) in v.deps
    haskey(c.cache.keys, k) && push!(edges(c, k), ki)
  end
end

function delete!(c::CycleCache{K,V}, k::K) where {K,V}
  iscached(c, k) || return
  ki = id(c, k)[1]
  ds = deps(c, k)
  es = edges(c, ki)
  delete!(c.cache, k)
  delete!(c.edges, ki)
  for (i, _) in ds
    haskey(c.edges, i) && delete!(c.edges[i], ki)
  end
  for i in es
    haskey(c.cache.keys, i) && delete!(c, c.cache.keys[i])
  end
  return c
end

function getindex(c::CycleCache{K,V}, k::K) where {K,V}
  if !iscached(c, k)
    ki = NFT()
    v, ideps = trackdeps(() -> c.init(k))
    set!(c, k, CacheValue{V}(v, ki=>NFT(), ideps))
    while true
      v, deps = value(c, k)
      deps = union(deps, ideps)
      if v == cached(c, k)
        set!(c, k, CacheValue{valtype(c)}(v, id(c, k), deps))
      else
        set!(c, k, CacheValue{V}(v, ki=>NFT(), deps))
        (!isempty(edges(c, ki)) || ki in first.(deps)) && continue
      end
      break
    end
  end
  track!(id(c, k))
  return cached(c, k)
end

function reset!(c::CycleCache; deps = [])
  print = fingerprint(deps)
  for k in keys(c)
    all(((_, id),) -> id in print || id in fingerprint(c), Caches.deps(c, k)) && continue
    delete!(c, k)
  end
end

# Value -> key mapping

struct DualCache{K,V}
  cache::Cache{K,V}
  keys::IdDict{V,K}
  DualCache{K,V}(c::Cache{K,V}) where {K,V} = new{K,V}(c, IdDict{V,K}())
  DualCache(c::Cache{K,V}) where {K,V} = new{K,V}(c, IdDict{V,K}())
end

DualCache{K,V}(args...) where {K,V} = DualCache(Cache{K,V}(args...))

fingerprint(c::DualCache) = fingerprint(c.cache)

function getindex(c::DualCache, k)
  v = c.cache[k]
  c.keys[v] = k
  return v
end

function reset!(c::DualCache; deps = [])
  for k in invalid(c.cache; deps)
    v = cached(c.cache, k)
    delete!(c.cache, k)
    delete!(c.keys, v)
  end
end

hasvalue(c::DualCache, v) = haskey(c.keys, v)
getkey(c::DualCache, v) = c.keys[v]
