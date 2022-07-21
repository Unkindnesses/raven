module Match

# A Julia implementation of runtime matching. Raven can't make use of this,
# but it's useful for speccing out the behaviour.

using ..Raven: Data, Hole, Literal, Repeat, Bind, Isa, Or, And

function match(pat::Hole, val)
  return Dict()
end

function match(pat::Literal, val)
  pat.value == val ? Dict() : nothing
end

end

using .Match: match
