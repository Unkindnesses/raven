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

_cache_default(ch::Cache, x) = throw(KeyError(x))

Cache{K,V}(f = _cache_default) where {K,V} =
  Cache{K,V}(Set{NFT}(), IdDict{K,V}(), Base.Dict{NFT,K}(), f)

Cache(f = _cache_default) = Cache{Any,Any}(f)

keytype(c::Cache{K,V}) where {K,V} = K

fingerprint(ch::Cache) = ch.fingerprint

iscached(ch::Cache, k) = haskey(ch.data, k)

valueid(c::Cache, k) = c.data[k].id[2]

keys(c::Cache) = keys(c.data)

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

function Base.delete!(c::Cache{K,V}, k::K) where {K,V}
  haskey(c.data, k) || return
  kid, id = c.data[k].id
  delete!(c.keys, kid)
  delete!(c.fingerprint, id)
  delete!(c.data, k)
  return c
end

function set!(c::Cache{K,V}, k::K, v::V; deps = current_deps(objectid(c))) where {K,V}
  # TODO preserve key id
  haskey(c.data, k) && delete!(c, k)
  kid = NFT()
  id = NFT()
  c.keys[kid] = k
  c.data[k] = CacheValue{V}(v, kid=>id, deps)
  push!(c.fingerprint, id)
  return v
end

setindex!(c::Cache{K,V}, v::V, k::K) where {K,V} = set!(c, k, v)

function value(c::Cache{K,V}, k::K) where {K,V}
  if !haskey(c.data, k)
    value, deps = trackdeps(objectid(c)) do
      convert(V, c.default(c, k))::V
    end
    if haskey(c.data, k)
      delete!(deps, c.data[k].id)
      delete!(c, k)
    end
    # TODO preserve key id
    kid = NFT()
    id = NFT()
    c.keys[kid] = k
    c.data[k] = CacheValue{V}(value, kid => id, deps)
    push!(c.fingerprint, id)
  end
  return c.data[k].value
end

function getindex(c::Cache{K,V}, k::K) where {K,V}
  v = value(c, k)
  track!(c.data[k].id)
  return v
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

value(c::EagerCache, args...) = value(c.cache, args...)
getindex(c::EagerCache, args...) = getindex(c.cache, args...)
setindex!(c::EagerCache, args...) = setindex!(c.cache, args...)

valueid(c::EagerCache, k) = valueid(c.cache, k)

fingerprint(c::EagerCache) = fingerprint(c.cache)

function reset!(c::EagerCache{K,V}; deps = []) where {K,V}
  for k in invalid(c.cache; deps)
    v = c.cache.data[k]
    value, deps = trackdeps(objectid(c.cache)) do
      convert(V, c.cache.default(c, k))::V
    end
    if v.value == value
      c.cache.data[k] = CacheValue{V}(value, v.id, deps)
    else
      set!(c.cache, k, value; deps)
    end
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
    v = c.cache.data[k].value
    delete!(c.cache, k)
    delete!(c.keys, v)
  end
end

hasvalue(c::DualCache, v) = haskey(c.keys, v)
getkey(c::DualCache, v) = c.keys[v]
