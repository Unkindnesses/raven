struct CacheValue{T}
  value::T
  id::NFT
  deps::Set{NFT}
end

struct Cache{K,V}
  default::Any
  fingerprint::Set{NFT}
  data::IdDict{K,CacheValue{V}}
end

Cache{K,V}(f) where {K,V} = Cache{K,V}(f, Set{NFT}(), IdDict())

Cache(f) = Cache{Any,Any}(f)

Base.show(io::IO, x::Cache) = print(io, typeof(x), "(...)")

keytype(::Cache{K,V}) where {K,V} = K
valtype(::Cache{K,V}) where {K,V} = V
keys(c::Cache) = keys(c.data)
fingerprint(ch::Cache) = ch.fingerprint

iscached(ch::Cache, k) = haskey(ch.data, k)
cached(ch::Cache, k) = ch.data[k].value
id(c::Cache, k) = c.data[k].id
deps(c::Cache, k) = c.data[k].deps

function Base.delete!(c::Cache{K,V}, k::K) where {K,V}
  haskey(c.data, k) || return
  delete!(c.fingerprint, id(c, k))
  delete!(c.data, k)
  return c
end

function set!(c::Cache{K,V}, k::K, v::CacheValue{V}) where {K,V}
  delete!(c, k)
  push!(c.fingerprint, v.id)
  c.data[k] = v
  return v
end

function set!(c::Cache{K,V}, k::K, v::V; deps = Set()) where {K,V}
  set!(c, k, CacheValue{V}(v, NFT(), deps))
  return v
end

function value(c::Cache{K,V}, k::K; self = c) where {K,V}
  trackdeps() do
    convert(V, c.default(k))::V
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

function invalid(c::Cache; deps = [])
  print = fingerprint(deps)
  (k for k in keys(c)
   if !all(id -> id in print, c.data[k].deps))
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
