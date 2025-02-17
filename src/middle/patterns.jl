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
  if isvalue(rlist(val′, val))
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
  bs = @try partial_match(mod, pat.pattern, val, path)
  return _assoc(bs, pat.name => (val, path))
end

function partial_match(mod, pat::Trait, val, path)
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

function partial_match(mod, pat::Constructor, val, path)
  pat = rvpattern(pat)
  pat = mod[(tag"common.constructorPattern", rlist(parts(pat)...))]
  isvalue(pat) || return missing
  return partial_match(mod, jlpattern(part(pat, 1)), val, path)
end

ishole(x::Hole) = true
ishole(x::Bind) = ishole(x.pattern)
ishole(x) = false

isslurp(x) = x isa Repeat && ishole(x.pattern)

function partial_match(mod, pat::Pack, val::SimpleType, path)
  bs = Dict()
  i = 0
  while true
    i <= nparts(val) || break
    i <= nparts(pat) || return
    if pat[i] == Repeat(hole)
      break
    elseif isslurp(pat[i])
      bs = @try _assoc(bs, pat[i].pattern.name => (rlist(allparts(val)[i+1:end]...), [path..., i:nparts(val)]))
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
      bs = @try _assoc(bs, pat[i].pattern.name => (rlist(), [path..., i:0]))
    end
  elseif nparts(pat) > nparts(val)
    return
  end
  return bs
end

# TODO match results don't have to be identical, if
# bindings and paths are right we can merge types.
function partial_match_union(mod, pat, val::Onion, path)
  ms = map(x -> partial_match(mod, pat, x, path), val.types)
  any(x -> x === missing, ms) && return missing
  all(==(first(ms)), ms) && return first(ms)
  return missing
end

function partial_match(mod, pat::Pack, val::Onion, path)
  partial_match_union(mod, pat, val, path)
end

function partial_match(mod, pat::Or, val::Onion, path)
  partial_match_union(mod, pat, val, path)
end

function partial_match(mod, pat::Pack, val::Recursive, path)
  partial_match(mod, pat, unroll(val), path)
end

function partial_match(mod, pat::Or, val::Recursive, path)
  partial_match(mod, pat, unroll(val), path)
end

function partial_match(mod, pat::Pack, val::VPack, path)
  bs = @try partial_match(mod, tag(pat), tag(val), [path..., 0])
  isempty(bs) || return missing
  (nparts(pat) == 1 && part(pat, 1) isa Repeat) || return missing
  pat = part(pat, 1).pattern
  b, r = pat isa Bind ? (pat.name, pat.pattern) : (nothing, pat)
  bs′ = partial_match(mod, r, val.parts, path)
  isnothing(bs′) && return
  isempty(bs′) || return missing
  return b == nothing ? bs : _assoc(bs, b => (val, path))
end

partial_match(mod, pat, val) = partial_match(mod, pat, val, [])

# TODO assumes the value is unchanged by the match
function trivial_isa(int, val, T)
  r = int[(tag"common.matchTrait", rlist(T, val))]
  isnothing(r) && return missing
  T = tag(part(r, 1))
  T == tag"common.Some" ? true :
  T == tag"common.Nil" ? false :
  missing
end

# Filtered methods

function MatchMethods(defs, interp)
  EagerCache() do _, (f, Ts)
    result = []
    for meth in reverse(defs[f])
      m = partial_match(interp, meth.sig.pattern, Ts)
      if isnothing(m)
        continue
      else
        push!(result, (meth, m))
        ismissing(m) || break
      end
    end
    return result
  end
end

# Generate dispatchers

dispatch_arms(T) = [T]
dispatch_arms(T::Onion) = T.types
dispatch_arms(T::Recursive) = dispatch_arms(unroll(T))

function dispatch_arms(T::Pack)
  result = [[]]
  for part in dispatch_arms.(T.parts)
    result = [[prefix..., x] for prefix in result for x in part]
  end
  return [pack(x...) for x in result]
end

function indexer!(ir::IR, T, arg, path)
  isempty(path) && return arg
  (p, rest...) = path
  if p isa AbstractVector
    ps = [push!(ir, stmt(xpart(arg, i), type = part(T, i))) for i in p]
    arg = push!(ir, stmt(xlist(ps...), type = rlist(exprtype(ir, ps)...)))
  else
    T = part(T, p)
    arg = push!(ir, stmt(xpart(arg, p), type = T))
  end
  arg = indexer!(ir, T, arg, rest)
end

function icall!(inf, ir, sig, f, args...)
  if !(f isa RMethod)
    args = [push!(ir, stmt(xlist(args...), type = rlist(exprtype(ir, args)...)))]
  end
  ex = xcall(f, args...)
  T = infercall!(inf, sig, ir, ex)
  push!(ir, stmt(ex, type = T))
end

function dispatcher(inf, func::Tag, Ts)
  ir = IR(meta = FuncInfo(func, trampoline = true))
  args = argument!(ir, type = Ts)
  ret = ⊥
  arms = dispatch_arms(Ts)
  call!(f, args...) = icall!(inf, ir, (func, Ts), f, args...)
  for (meth, m) in inf.meths[(func, Ts)]
    if ismissing(m)
      pat = rvpattern(meth.sig.pattern)
      arms = filter(T -> issubset(rlist(nil), infercall!(inf, (func, Ts), tag"common.match", rlist(T, pat))), arms)
      m = call!(tag"common.match", args, pat)
      m = call!(part_method, m, 1)
      cond = push!(ir, stmt(xcall(isnil_method, m), type = partial_isnil(exprtype(ir, m))))
      branch!(ir, length(blocks(ir))+2, when = cond)
      branch!(ir, length(blocks(ir))+1)
      block!(ir)
      m = call!(notnil_method, m)
      as = []
      for arg in meth.sig.args
        push!(as, call!(part_method, call!(tag"common.getkey", m, Tag(arg)), 1))
      end
      result = call!(meth, as...)
      isempty(meth.sig.swap) && (result = push!(ir, stmt(xlist(result), type = rlist(exprtype(ir, result)))))
      return!(ir, result)
      ret = union(ret, exprtype(ir, result))
      block!(ir)
    else # certain to match
      result = call!(meth, [indexer!(ir, Ts, args, m[x][2]) for x in meth.sig.args]...)
      isempty(meth.sig.swap) && (result = push!(ir, stmt(xlist(result), type = rlist(exprtype(ir, result)))))
      return!(ir, result)
      ret = union(ret, exprtype(ir, result))
      return ir, ret
    end
    if isempty(arms)
      unreachable!(ir)
      return ir, ret
    end
  end
  if func == tag"common.abort" && issubset(Ts, rlist(String))
    error("Compiler fault: couldn't guarantee abort method matches")
  end
  if options().jspanic
    call!(tag"common.abort", "No matching method: $func: $Ts")
  end
  unreachable!(ir)
  return ir, ret
end
