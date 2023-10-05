# Unique ID type

const nft_id = Base.Ref(UInt64(0))

struct NonFungibleToken
  id::UInt64
end

NonFungibleToken() = NonFungibleToken(nft_id[] += 1)

const NFT = NonFungibleToken

function Base.show(io::IO, t::NFT)
  print(io, "NFT(")
  show(io, t.id)
  print(io, ")")
end

# Track dependencies through the call stack

const nullkey = NFT()

let sym = gensym(:cache_deps)
  global cache_deps() = get!(() -> Tuple{UInt64,Set{Pair{NFT,NFT}}}[], task_local_storage(), sym)::Vector{Tuple{UInt64,Set{Pair{NFT,NFT}}}}
end

function trackdeps(f, id::UInt64)
  stack = cache_deps()
  deps = Set{Pair{NFT,NFT}}()
  push!(stack, (id,deps))
  result = nothing
  try
    result = f()
  finally
    pop!(stack)
  end
  return result, deps
end

function track!(id::Pair{NFT,NFT})
  stack = cache_deps()
  isempty(stack) || push!(stack[end][2], id)
  return
end

track!(v::NFT) = track!(nullkey => v)

function current_deps(id::UInt64)::Set{Pair{NFT,NFT}}
  deps = cache_deps()
  (isempty(deps) || deps[end][1] != id) && return Set{Pair{NFT,NFT}}()
  return copy(deps[end][2])
end

# Collect IDs available in a given cache

subcaches(ch) = error("$(typeof(ch)) isn't a cache")
fingerprint(ch) = reduce(union!, (fingerprint(ch) for ch in subcaches(ch)), init = Set{NFT}())
fingerprint(s::Set{NFT}) = s
fingerprint(a, b, c...) = union(fingerprint(a), fingerprint(b, c...))

reset!(ch; init = []) = foreach(ch -> reset!(ch; init), subcaches(ch))
