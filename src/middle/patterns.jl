# Partial Pattern Matching

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
#
# Ideally we'd think of dispatchers as a simple list of `if` clauses: for each
# method, check the arg list against the signature via `match(sig, args)`, and
# if the match succeeds call the method. Then we do type inference and remove
# redundant checks using the usual, generic optimisations.
#
# However, `match` is not available until it's defined in the stdlib, for which
# we need function calls (and thus dispatchers, and thus `match`) to work. To
# break the cycle we take a shortcut in some simple cases; if an input obviously
# matches the signature, we'll generate code for the dispatcher that behaves
# like `match`.

function trym(x)
  @q begin
    local result = $(esc(x))
    (result === nothing || result === missing) && return result
    result
  end
end

@eval macro $:try(x)
  trym(x)
end

_assoc(::Missing, _) = missing

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
  (ismissing(as) || ismissing(bs)) && return missing
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
  return Dict(pat.name => (val, path))
end

function partial_match(mod, pat::Trait, val, path)
  (haskey(mod, pat.pattern) && isvalue(mod[pat.pattern])) || return missing
  T = mod[pat.pattern]
  r = trivial_isa(mod, val, pat.pattern)
  r === true ? Dict() : r === false ? nothing : missing
end

function partial_match(mod, pat::And, val, path)
  bs = Dict()
  for p in pat.patterns
    bs = @try _merge(bs, @try partial_match(mod, p, val, path))
  end
  return bs
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

partial_match(mod, pat::Constructor, val, path) = missing

isslurp(x) = x isa Repeat && x.pattern isa Union{Hole,Bind}

# Redundant, but this check prevents some `trivial_isa` cases becoming circular.
function shortcut_literals(pat::Pack, val)
  any(x -> x isa Repeat, allparts(pat)) && return false
  nparts(pat) == nparts(val) || return true
  return any(zip(allparts(pat), allparts(val))) do (a, b)
    a isa Literal && isvalue(b) && a.value != b
  end
end

function partial_match(mod, pat::Pack, val::SimpleType, path)
  bs = Dict()
  i = 0
  shortcut_literals(pat, val) && return nothing
  while true
    i <= nparts(val) || break
    i <= nparts(pat) || return nothing
    if pat[i] == Repeat(hole)
      break
    elseif isslurp(pat[i])
      bs = @try _assoc(bs, pat[i].pattern.name => (rtuple(allparts(val)[i+1:end]...), [path..., i:nparts(val)]))
      return bs
    elseif pat[i] isa Repeat
      return missing
    else
      # continue on `missing`, since we might narrow to `nothing` later
      b = partial_match(mod, part(pat, i), part(val, i), [path..., i])
      b === nothing && return
      bs = _merge(bs, b)
    end
    i += 1
  end
  if nparts(pat) == i && isslurp(pat[i])
    if pat[i].pattern != hole
      bs = @try _assoc(bs, pat[i].pattern.name => (rtuple(), [path..., i:0]))
    end
  elseif nparts(pat) > nparts(val)
    return
  end
  return bs
end

# TODO match results don't have to be identical, if
# bindings and paths are right we can merge types.
function partial_match_union(mod, pat, val::Or, path)
  ms = map(x -> partial_match(mod, pat, x, path), val.patterns)
  any(x -> x === missing, ms) && return missing
  all(==(first(ms)), ms) && return first(ms)
  return missing
end

function partial_match(mod, pat::Pack, val::Or, path)
  partial_match_union(mod, pat, val, path)
end

function partial_match(mod, pat::Or, val::Or, path)
  partial_match_union(mod, pat, val, path)
end

function partial_match(mod, pat::Pack, val::Recursive, path)
  withrecur(val.type) do
    partial_match(mod, pat, val.type, path)
  end
end

function partial_match(mod, pat::Or, val::Recursive, path)
  withrecur(val.type) do
    partial_match(mod, pat, val.type, path)
  end
end

function destruct_isa(p)
  p isa Bind && return (p.name, And([]))
  p isa And && p.patterns[1] isa Bind || return (nothing, p)
  (p.patterns[1].name, length(p.patterns) == 2 ? p.patterns[2] : And(p.patterns[2:end]))
end

function partial_match(mod, pat::Pack, val::VPack, path)
  bs = @try partial_match(mod, tag(pat), tag(val), [path..., 0])
  isempty(bs) || return missing
  (nparts(pat) == 1 && part(pat, 1) isa Repeat) || return missing
  pat = part(pat, 1).pattern
  b, r = destruct_isa(pat)
  bs′ = partial_match(mod, r, val.parts, path)
  isnothing(bs′) && return
  isempty(bs′) || return missing
  return b == nothing ? bs : _assoc(bs, b => (val, path))
end

partial_match(mod, pat, val) = partial_match(mod, pat, val, [])

function partial_ismatch(mod, pat, val)
  result = partial_match(mod, pat, val)
  ismissing(result) && return missing
  return result isa AbstractDict
end

function trivial_method(mod, func::Symbol, Ts)
  for meth in reverse(mod.methods[func])
    m = partial_match(mod, meth.sig.pattern, Ts)
    if m === nothing
      continue
    elseif m isa AbstractDict
      return meth
    else
      return nothing
    end
  end
end

function trivial_isa(mod, val, T)
  meth = trivial_method(mod, :isa, rtuple(val, T))
  meth == nothing && return missing
  ir = meth.func
  (length(ir) == 0 && length(blocks(ir)) == 1) || return missing
  ret = IRTools.returnvalue(block(ir, 1))
  ret isa Global || return missing
  ret = mod.defs[ret.name]
  ret isa Int32 || return missing
  return Bool(ret)
end
