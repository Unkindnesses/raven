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
  global cache_deps() = get!(() -> Set{Pair{NFT,NFT}}[], task_local_storage(), sym)::Vector{Set{Pair{NFT,NFT}}}
end

function trackdeps(f)
  stack = cache_deps()
  deps = Set{Pair{NFT,NFT}}()
  push!(stack, deps)
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
  isempty(stack) || push!(stack[end], id)
  return
end

track!(v::NFT) = track!(nullkey => v)

current_deps() = isempty(cache_deps()) ? Set{Pair{NFT,NFT}}() : copy(cache_deps()[end])

# Collect IDs available in a given cache

fingerprint(ch) = error("$(typeof(ch)) isn't a cache")
fingerprint(a, b, c...) = union(fingerprint(a), fingerprint(b, c...))
