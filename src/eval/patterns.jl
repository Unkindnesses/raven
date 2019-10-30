const hole = data(:Hole)
bind(name, pattern) = data(:Bind, name, pattern)

isprimitive(x::T, ::Type{T}) where T = true
isprimitive(x, ::Type) = false

# Pattern Lowering

function lowerisa(mod, ex, as)
  if ex isa Symbol
    return data(:Isa, mod[ex])
  else
    lowerpattern(mod, ex, as)
  end
end

function lowerpattern(mod, ex, as)
  if ex isa Symbol
    ex in as || push!(as, ex)
    return bind(ex, hole)
  elseif ex isa Union{Primitive,Quote}
    ex isa Quote && (ex = ex.expr)
    return data(:Literal, ex)
  elseif ex isa Tuple
    data(:Data, data(:Literal, :Tuple), map(x -> lowerpattern(mod, x, as), ex.args)...)
  elseif ex isa Operator && ex.op == :(::)
    name, T = ex.args
    name in as || push!(as, name)
    bind(name, lowerisa(mod, T, as))
  else
    error("Invalid pattern syntax $(ex)")
  end
end

function lowerpattern(mod, ex)
  as = []
  p = lowerpattern(mod, ex, as)
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
  elseif p == hole
    return bs
  elseif tag(p) == :Literal
    p = part(p, 1)
    return p == x ? bs : nothing
  elseif tag(p) == :Data
    nparts(p) == nparts(x) + 1 || return
    for i = 0:nparts(x)
      bs = match(bs, part(p, i+1), part(x, i))
      bs == nothing && return
    end
    return bs
  elseif tag(p) == :Isa
    return Bool(vinvoke(:isa, x, part(p, 1))) ? bs : nothing
  elseif tag(p) == :Bind
    bs = match(bs, resolve(part(p, 2)), x)
    bs == nothing && return
    # TODO handle duplicate names
    assoc(bs, part(p, 1), x)
  else
    error("Invalid pattern $p")
  end
end

match(p, x) = match(phmap(), p, x)
