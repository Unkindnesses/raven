import Base: RefValue, getindex, setindex!, haskey, get, iterate, length, eltype

const Edge = RefValue{Bool}

let sym = gensym(:cache_deps)
  global cache_deps() = get!(() -> Set{Edge}[], task_local_storage(), sym)::Vector{Set{Edge}}
end

function trackdeps(f)
  stack = cache_deps()
  deps = Set{Edge}()
  push!(stack, deps)
  local result
  try
    result = f()
  finally
    pop!(stack)
  end
  return result, deps
end

function trackdep!(t::Edge)
  stack = cache_deps()
  isempty(stack) || push!(stack[end], t)
  return
end

struct CacheValue{T}
  value::T
  edge::Edge
  deps::Set{Edge}
end

CacheValue{T}(x) where T = CacheValue{T}(x, Edge(true), Set{Edge}())

struct Cache{K,V}
  data::IdDict{K,CacheValue{V}}
  default::Any
end

_cache_default(ch::Cache, x) = throw(KeyError(x))

Cache{K,V}(f = _cache_default) where {K,V} = Cache{K,V}(Dict{K,V}(), f)

Cache(f = _cache_default) = Cache{Any,Any}(f)

function setindex!(c::Cache{K,V}, v::V, k::K) where {K,V}
  haskey(c.data, k) && (c.data[k].edge[] = false)
  c.data[k] = CacheValue{V}(v)
  return v
end

function getindex(c::Cache{K,V}, k::K) where {K,V}
  if !haskey(c.data, k) || any(!x[] for x in c.data[k].deps)
    haskey(c.data, k) && (c.data[k].edge[] = false)
    value, deps = trackdeps() do
      c.default(c, k)::V
    end
    c.data[k] = CacheValue{V}(value, Edge(true), deps)
  end
  trackdep!(c.data[k].edge)
  foreach(trackdep!, c.data[k].deps)
  return c.data[k].value
end

haskey(c::Cache, k) = haskey(c.data, k)

function iterate(c::Cache, st...)
  (k, v), st = @something iterate(c.data, st...) return
  return k => v.value, st
end

length(c::Cache) = length(c.data)
eltype(c::Cache{K,V}) where {K,V} = Pair{K,V}
