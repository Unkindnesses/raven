const hole = vstruct(:Hole)
bind(name, pattern) = vstruct(:Bind, name, pattern)

# Pattern Lowering

function lowerpattern(ex, as)
  if ex isa Symbol
    ex in as || push!(as, ex)
    return bind(ex, hole)
  elseif ex isa Union{Primitive,Quote}
    return ex
  elseif ex isa Tuple
    vstruct(:Struct, :Tuple, map(x -> lowerpattern(x, as), ex.args)...)
  else
    error("Invalid pattern syntax $(ex)")
  end
end

function lowerpattern(ex)
  as = []
  p = lowerpattern(ex, as)
  return p, as
end

# Pattern Matching

pattern(p) =
  p == :Int64 ? x -> x isa Int64 :
  error("Unknown pattern $p")

resolve(p) = p isa Symbol ? pattern(p) : p

function match(bs, p, x)
  if p isa Function
    return p(x) ? bs : nothing
  elseif p isa Union{Primitive,Quote}
    p isa Quote && (p = p.expr)
    return p == x ? bs : nothing
  elseif p == hole
    return bs
  elseif tag(p) == :Struct
    nparts(p) == nparts(x) + 1 || return
    for i = 0:nparts(x)
      bs = match(bs, part(p, i+1), part(x, i))
      bs == nothing && return
    end
    return bs
  elseif tag(p) == :Bind
    bs = match(bs, resolve(part(p, 2)), x)
    bs == nothing && return
    # TODO handle duplicate names
    assoc(bs, part(p, 1), x)
  end
end

match(p, x) = match(phmap(), p, x)
