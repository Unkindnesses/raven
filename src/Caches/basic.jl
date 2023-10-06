# Simple Ref-like type containing a single value. Tracked by `Cache`s.

mutable struct Ref{T}
  value::T
  id::NFT
end

fingerprint(ch::Ref) = Set([ch.id])

Ref{T}(x::T) where T = Ref{T}(x, NFT())
Ref(x) = Ref{typeof(x)}(x)

function getindex(ch::Ref)
  track!(ch.id)
  return ch.value
end

function setindex!(ch::Ref{T}, x::T) where T
  ch.id = NFT()
  ch.value = x
end

reset!(ch::Ref; deps = []) = return

# Just an IdDict, with tracking enabled for `Cache`s.

struct Dict{K,V}
  fingerprint::Set{NFT}
  data::IdDict{K,Tuple{NFT,V}}
  haskey::IdDict{K,NFT}
end

Dict{K,V}() where {K,V} = Dict{K,V}(Set{NFT}(), IdDict{K,Tuple{NFT,V}}(), IdDict{K,NFT}())
Dict() = Dict{Any,Any}()

fingerprint(ch::Dict) = ch.fingerprint

keys(ch::Dict) = keys(ch.data)

reset!(ch::Dict; deps = []) = return

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
