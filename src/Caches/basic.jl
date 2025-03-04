# Simple Ref-like type containing a single value. Tracked by `Cache`s.

mutable struct Ref{T}
  value::T
  id::NFT
end

fingerprint(ch::Ref) = Set([ch.id])

Ref{T}(x::T) where T = Ref{T}(x, NFT())
Ref(x) = Ref{typeof(x)}(x)

Base.copy(x::Ref{T}) where T = Ref{T}(x.value, x.id)

function getindex(ch::Ref)
  track!(ch.id)
  return ch.value
end

function setindex!(ch::Ref{T}, x::T) where T
  ch.id = NFT()
  ch.value = x
end

reset!(ch::Ref; deps = []) = return

time(::Ref) = UInt64(0)

# Just a dict, with tracking enabled for `Cache`s.

struct Dict{K,V}
  fingerprint::Set{NFT}
  data::Base.Dict{K,Tuple{NFT,V}}
  haskey::Base.Dict{K,NFT}
end

Dict{K,V}() where {K,V} = Dict{K,V}(Set{NFT}(), Base.Dict{K,Tuple{NFT,V}}(), Base.Dict{K,NFT}())
Dict() = Dict{Any,Any}()

Base.copy(d::Dict{K,V}) where {K,V} =
  Dict{K,V}(copy(d.fingerprint), copy(d.data), copy(d.haskey))

fingerprint(ch::Dict) = ch.fingerprint

keys(ch::Dict) = keys(ch.data)

reset!(ch::Dict; deps = []) = return

id(ch::Dict, k) = ch.data[k][1]

function getindex(ch::Dict{K,V}, k::K) where {K,V}
  (id, value) = ch.data[k]
  track!(id)
  return value
end

function setindex!(ch::Dict{K,V}, v::V, k::K) where {K,V}
  if haskey(ch.data, k)
    delete!(ch.fingerprint, ch.data[k][1])
    delete!(ch.data, k)
  elseif haskey(ch.haskey, k)
    delete!(ch.fingerprint, ch.haskey[k])
    delete!(ch.haskey, k)
  end
  id = NFT()
  ch.data[k] = (id, v)
  push!(ch.fingerprint, id)
  return v
end

function delete!(ch::Dict{K,V}, k::K) where {K,V}
  if haskey(ch.data, k)
    delete!(ch.fingerprint, ch.data[k][1])
    delete!(ch.data, k)
    if haskey(ch.haskey, k)
      delete!(ch.fingerprint, ch.haskey[k])
      delete!(ch.haskey, k)
    end
  end
  return ch
end

function Base.empty!(ch::Dict)
  # TODO do this in bulk
  for k in keys(ch)
    delete!(ch, k)
  end
  return ch
end

iscached(ch::Dict{K,V}, k::K) where {K,V} = haskey(ch.data, k)

function haskey(ch::Dict{K,V}, k::K) where {K,V}
  id = get!(() -> NFT(), ch.haskey, k)
  push!(ch.fingerprint, id)
  track!(id)
  return haskey(ch.data, k)
end

function get(ch::Dict{K}, k::K, default) where K
  haskey(ch, k) ? ch[k] : default
end

function get!(ch::Dict{K,V}, k::K, default::V) where {K,V}
  haskey(ch, k) ? ch[k] : (ch[k] = default)
end

Base.Dict(ch::Dict) = copy(ch.data)

time(::Dict) = UInt64(0)
