# Pattern Matching

isrepeat(x) = false
isrepeat(x::Repeat) = true
isrepeat(x::Bind) = isrepeat(x.pattern)

# Match types (partial values inferred by the interpreter) against user-
# specified patterns at compile time.
# Returns either a dictionary (if the match is certain to succeed), `nothing`
# (if the match is certain to fail) or `missing` (we can't handle the match
# statically).
#
# The returned dictionary contains a mapping from binding names to (type, path)
# pairs, where `path` tells the compiler where to find the matched value in the
# original data. `path` is a list of indexes; the last index might be a
# UnitRange, representing a splat.

@eval macro $:try(x)
  quote
    local result = $(esc(x))
    (result === nothing || result === missing) && return result
    result
  end
end

function _merge(as, bs)
  return merge(as, bs)
end

_assoc(bs, pair) = _merge(bs, Dict(pair))

function partial_match(mod, pat::Hole, val, path)
  return Dict()
end

function partial_match(mod, pat::Literal, val, path)
  if isvalue(val)
    return pat.value == val ? Dict() : nothing
  else
    return missing # could be more precise here
  end
end

function partial_match(mod, pat::Bind, val, path)
  bs = @try partial_match(mod, pat.pattern, val, path)
  return _assoc(bs, pat.name => (val, path))
end

function partial_match(mod, pat::Isa, val, path)
  (haskey(mod, pat.pattern) && isvalue(mod[pat.pattern])) || return missing
  # TODO remove the catchall
  return tag(val) == mod[pat.pattern] ? Dict() : nothing
end

function partial_match(mod, pat::Data, val, path)
  val isa Data || return # TODO: could be wrong. Add `parts` for natives.
  nparts(pat) > nparts(val) && return
  bs = Dict()
  for i = 1:nparts(val)
    length(parts(pat)) >= i || return nothing
    if pat[i] == Repeat(hole)
      break
    elseif pat[i] isa Bind && pat[i].pattern == Repeat(hole)
      bs = @try _assoc(bs, pat[i].name => (rtuple(parts(val)[i:end]...), [path..., i:nparts(val)]))
      break
    elseif isrepeat(pat[i])
      return missing
    else
      bs = @try _merge(bs, @try partial_match(mod, pat[i], val[i], [path..., i]))
    end
  end
  return bs
end

partial_match(mod, pat, val) = partial_match(mod, pat, val, [])

function partial_ismatch(mod, pat, val)
  result = partial_match(mod, pat, val)
  ismissing(result) && return missing
  return result isa AbstractDict
end

# TODO remove
function simple_match(mod, p, x)
  p isa Bind && (partial_ismatch(mod, p, x) === true) && return (:)
  (p isa Data && part(p, 0) == Literal(:Tuple)) || return
  is = []
  for i = 0:nparts(x)
    pi = p[i]
    if pi isa Bind && pi.pattern == Repeat(hole)
      push!(is, i:nparts(x))
      break
    elseif pi isa Bind && (partial_ismatch(mod, pi, x[i]) === true)
      push!(is, i)
    elseif pi isa Literal && pi.value == x[i]
    else
      return
    end
  end
  return is
end
