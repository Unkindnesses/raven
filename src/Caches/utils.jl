# Unique ID type

const nft_id = Base.Ref(UInt64(0))

struct NonFungibleToken
  id::UInt64
end

NonFungibleToken() = NonFungibleToken(nft_id[] += 1)

const NFT = NonFungibleToken

# Track dependencies through the call stack

let sym = gensym(:cache_deps)
  global cache_deps() = get!(() -> Set{NFT}[], task_local_storage(), sym)::Vector{Set{NFT}}
end

function trackdeps(f)
  stack = cache_deps()
  deps = Set{NFT}()
  push!(stack, deps)
  result = nothing
  try
    result = f()
  finally
    pop!(stack)
  end
  return result, deps
end

function track!(t::NFT)
  stack = cache_deps()
  isempty(stack) || push!(stack[end], t)
  return
end

current_deps() = isempty(cache_deps()) ? Set{NFT}() : copy(cache_deps()[end])

# Collect IDs available in a given cache

fingerprint(ch) = error("$(typeof(ch)) isn't a cache")
fingerprint(a, b, c...) = union(fingerprint(a), fingerprint(b, c...))
