# Pattern Matching

# Right now there's some confusion over type-level vs value-level matching;
# this code was originaly written for values and now sort-of works to produce
# types in simple cases. We should clean up and make sure it's clear what
# matches we're willing to shortcut this way.

bindings(x) = Set()
bindings(p::Data) =
  tag(p) == :Bind ? Set([part(p, 1)]) :
  reduce(Base.union, map(bindings, parts(p)))

# Check for mismatched literals first
# This lets us fail faster in some cases, and also avoids some cases becoming circular.
function quickcheck(p, x)
  nparts(p) == nparts(x) + 1 || return true
  for i = 0:nparts(x)
    pi = part(p, i+1)
    if tag(pi) == :Literal && part(pi, 1) != part(x, i)
      return false
    end
  end
  return true
end

function matchdata(mod, bs, pat, x)
  xs = collect(x.parts)
  for p in parts(pat)
    if tag(p) == :Bind && tag(part(p, 2)) == :Repeat
      @assert part(part(p, 2), 1) == hole
      return merge(bs, phmap(part(p, 1) => rtuple(xs...)))
    else
      isempty(xs) && return
      x = popfirst!(xs)
      bs = match(mod, bs, p, x)
    end
    bs == nothing && return
  end
  isempty(xs) || return
  return bs
end

function match(mod, bs, p, x)
  if p isa Function
    return p(x) ? bs : nothing
  elseif p == hole
    return bs
  elseif tag(p) == :Literal
    p = part(p, 1)
    return p == x ? bs : nothing
  elseif tag(p) == :Data
    quickcheck(p, x) || return
    return matchdata(mod, bs, p, x)
  elseif tag(p) == :Isa
    return tag(x) == mod[part(p, 1)] ? bs : nothing
  elseif tag(p) == :Or
    for i = 1:nparts(p)
      bs′ = match(mod, bs, part(p, i), x)
      bs′ == nothing || return bs′
    end
    return
  elseif tag(p) == :Bind
    bs = match(mod, bs, part(p, 2), x)
    bs == nothing && return
    # TODO handle duplicate names
    assoc(bs, part(p, 1), x)
  else
    error("Invalid pattern $p")
  end
end

match(mod, p, x) = match(mod, phmap(), p, x)

ismatch(mod, p, x) = match(mod, p, x) != nothing

function simple_match(mod, p, x)
  tag(p) == :Bind && ismatch(mod, p, x) && return (:)
  (tag(p) == :Data && part(p, 1) == data(:Literal, :Tuple)) || return
  is = []
  for i = 1:nparts(x)
    pi = p[i+1]
    if tag(pi) == :Bind && part(pi, 2) == data(:Repeat, hole)
      push!(is, i:nparts(x))
      break
    elseif tag(pi) == :Bind && ismatch(mod, pi, x[i])
      push!(is, i)
    elseif tag(pi) == :Literal && pi[1] == x[i]
    else
      return
    end
  end
  return is
end
