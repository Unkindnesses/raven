module Match

# A Julia implementation of runtime matching. Raven can't make use of this,
# but it's useful for speccing out the behaviour.

using ..Raven: Data, Hole, Literal, Repeat, Bind, Isa, Or, And, nparts, parts

function assoc(bs, k, v)
  haskey(bs, k) || return merge(bs, Dict(k => v))
  bs[k] == v ? bs : nothing
end

function _merge(as, bs)
  for (k, v) in bs
    as = assoc(as, k, v)
    as == nothing && return
  end
  return as
end

_merge(as, ::Nothing) = return

function match(pat::Hole, val)
  return Dict()
end

function match(pat::Literal, val)
  pat.value == val ? Dict() : nothing
end

function match(pat::Data, val)
  nparts(pat) == nparts(val) || return
  bs = Dict()
  for (a, b) in zip(parts(pat), parts(val))
    bs = _merge(bs, match(a, b))
    bs == nothing && return
  end
  return bs
end

end

using .Match: match
