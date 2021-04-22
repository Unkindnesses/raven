# Pattern Matching

# Check for mismatched literals first
# This lets us fail faster in some cases, and also avoids some cases becoming circular.
function quickcheck(p, x)
  nparts(p) == nparts(x) + 1 || return false
  for i = 0:nparts(x)
    pi = part(p, i+1)
    if tag(pi) == :Literal && part(pi, 1) != part(x, i)
      return false
    end
  end
  return true
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
    for i = 0:nparts(x)
      bs = match(mod, bs, part(p, i+1), part(x, i))
      bs == nothing && return
    end
    return bs
  elseif tag(p) == :Isa
    return Bool(vinvoke(mod, Symbol("isa?"), x, mod[part(p, 1)])) ? bs : nothing
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
    if tag(pi) == :Bind && ismatch(mod, pi, x[i])
      push!(is, i)
    elseif tag(pi) == :Literal && pi[1] == x[i]
    else
      return
    end
  end
  return is
end
