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

function _assoc(as, (name, (val, path)))
  haskey(as, name) || return merge(as, Dict(name => (val, path)))
  val′ = as[name][1]
  if isvalue(rtuple(val′, val))
    return val′ == val ? as : nothing
  else
    return missing # could reject more cases here
  end
end

function _merge(as, bs)
  for b in bs
    as = @try _assoc(as, b)
  end
  return as
end

function partial_match(mod, pat::Hole, val, path)
  return Dict()
end

function partial_match(mod, pat::Literal, val, path)
  if isvalue(val)
    return pat.value == val ? Dict() : nothing
  else
    return missing # could reject more cases here
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

function partial_match(mod, pat::Or, val, path)
  for p in pat.patterns
    m = partial_match(mod, p, val, path)
    isnothing(m) && continue
    ismissing(m) && return missing
    return m
  end
  return
end

function partial_match(mod, pat::Data, val, path)
  return # TODO: could be wrong. Add `parts` for natives.
end

function partial_match(mod, pat::Data, val::Data, path)
  bs = Dict()
  i = 0
  while true
    i <= nparts(val) || break
    i <= nparts(pat) || return nothing
    if pat[i] == Repeat(hole)
      break
    elseif pat[i] isa Bind && pat[i].pattern == Repeat(hole)
      bs = @try _assoc(bs, pat[i].name => (rtuple(parts(val)[i:end]...), [path..., i:nparts(val)]))
      return bs
    elseif isrepeat(pat[i])
      return missing
    else
      bs = @try _merge(bs, @try partial_match(mod, pat[i], val[i], [path..., i]))
    end
    i += 1
  end
  if nparts(pat) == i && isrepeat(pat[i])
    bs = pat[i] isa Bind ? (@try _assoc(bs, pat[i].name => (rtuple(), [path..., i:0]))) : bs
  elseif nparts(pat) > nparts(val)
    return
  end
  return bs
end

function partial_match(mod, pat::Data, val::VData, path)
  bs = @try partial_match(mod, tag(pat), tag(val), [path..., 0])
  (nparts(pat) == 1 && isrepeat(part(pat, 1))) || return missing
  pat = part(pat, 1)
  b, r = pat isa Bind ? (pat.name, pat.pattern) : (nothing, pat)
  bs′ = partial_match(mod, r.pattern, val.parts, path)
  isnothing(bs′) && return
  isempty(bs′) || return missing # bindings in the Repeat are equality checks
  return b == nothing ? bs : _assoc(bs, b => (val, path))
end

partial_match(mod, pat, val) = partial_match(mod, pat, val, [])

function partial_ismatch(mod, pat, val)
  result = partial_match(mod, pat, val)
  ismissing(result) && return missing
  return result isa AbstractDict
end
