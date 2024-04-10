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
  try
    return f(), deps
  finally
    pop!(stack)
  end
end

function track!(id::Pair{NFT,NFT})
  stack = cache_deps()
  isempty(stack) || push!(stack[end], id)
  return
end

track!(v::NFT) = track!(nullkey => v)

# Collect IDs available in a given cache

subcaches(ch) = error("$(typeof(ch)) isn't a cache")
fingerprint(ch) = reduce(union!, (fingerprint(ch) for ch in subcaches(ch)), init = Set{NFT}())
fingerprint(s::Set{NFT}) = s
fingerprint(s::NFT) = Set([s])
fingerprint(a, b, c...) = union(fingerprint(a), fingerprint(b, c...))

fingerprint(s::AbstractVector) =
  reduce(union!, (fingerprint(x) for x in s), init = Set{NFT}())

# TODO should probably assume dependencies
reset!(ch; deps = []) = foreach(ch -> reset!(ch; deps), subcaches(ch))
