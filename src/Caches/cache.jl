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

fingerprint(ch::Cache) = ch.fingerprint

iscached(ch::Cache, k) = haskey(ch.data, k)

function set!(c::Cache{K,V}, k::K, v::V; deps = current_deps()) where {K,V}
  haskey(c.data, k) && delete!(c.fingerprint, c.data[k].id[2])
  kid = NFT()
  id = NFT()
  c.data[k] = CacheValue{V}(v, kid=>id, deps)
  push!(c.fingerprint, id)
  return v
end

setindex!(c::Cache{K,V}, v::V, k::K) where {K,V} = set!(c, k, v)

function getindex(c::Cache{K,V}, k::K) where {K,V}
  if !haskey(c.data, k)
    value, deps = trackdeps() do
      convert(V, c.default(c, k))::V
    end
    kid = NFT()
    id = NFT()
    c.keys[kid] = k
    c.data[k] = CacheValue{V}(value, kid => id, deps)
    push!(c.fingerprint, id)
  end
  track!(c.data[k].id)
  return c.data[k].value
end

function reset!(c::Cache; deps = [])
  print = reduce(union!, fingerprint.(deps), init = Set{NFT}())
  changed = true
  # TODO this is a suboptimal way to handle internal dependencies
  while changed
    changed = false
    for (k, v) in c.data
      all(((k, id),) -> id in print || id in c.fingerprint, v.deps) && continue
      delete!(c.data, k)
      delete!(c.keys, v.id[1])
      delete!(c.fingerprint, v.id[2])
      changed = true
    end
  end
end

Base.IdDict(c::Cache) = IdDict(k => v.value for (k, v) in c.data)
