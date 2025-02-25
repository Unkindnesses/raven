# Timer

macro time(ex)
  quote
    start = Base.time_ns()
    stack = get!(task_local_storage(), :timer, Base.RefValue{UInt64}[])::Vector{Base.RefValue{UInt64}}
    offset = Base.RefValue(UInt64(0))
    push!(stack, offset)
    local result
    try
      result = $(esc(ex))
    finally
      pop!(stack)
    end
    span = Base.time_ns() - start
    isempty(stack) || (stack[end][] += span)
    result, span-offset[]
  end
end

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

let sym = gensym(:cache_deps)
  global cache_deps() = get!(() -> Set{NFT}[], task_local_storage(), sym)::Vector{Set{NFT}}
end

function trackdeps(f)
  stack = cache_deps()
  deps = Set{NFT}()
  push!(stack, deps)
  try
    result, t = @time f()
    return result, deps, t
  finally
    pop!(stack)
  end
end

function track!(t::NFT)
  stack = cache_deps()
  isempty(stack) || push!(stack[end], t)
  return
end

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

time(ch) = sum(time(c) for c in subcaches(ch))
