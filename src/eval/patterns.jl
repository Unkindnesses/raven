const hole = data(:Hole)
bind(name, pattern) = data(:Bind, name, pattern)

isprimitive(x::T, ::Type{T}) where T = true
isprimitive(::Type{T}, ::Type{T}) where T = true
isprimitive(x, ::Type) = false

# Pattern Lowering

function lowerisa(ex, as)
  if ex isa Symbol
    return data(:Isa, ex)
  else
    lowerpattern(ex, as)
  end
end

function _lowerpattern(ex, as)
  if ex isa Symbol
    ex in as || push!(as, ex)
    return bind(ex, hole)
  elseif ex isa Union{Primitive,Quote}
    ex isa Quote && (ex = ex.expr)
    return data(:Literal, ex)
  elseif ex isa Tuple
    data(:Data, data(:Literal, :Tuple), map(x -> _lowerpattern(x, as), ex.args)...)
  elseif ex isa Operator && ex.op == :(:)
    name, T = ex.args
    name in as || push!(as, name)
    bind(name, lowerisa(T, as))
  elseif ex isa Call && ex.func == :data
    data(:Data, map(x -> _lowerpattern(x, as), ex.args)...)
  else
    error("Invalid pattern syntax $(ex)")
  end
end

function lowerpattern(ex)
  as = []
  p = _lowerpattern(ex, as)
  return p, as
end

# Pattern Matching

function match(mod, bs, p, x)
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
      bs = match(mod, bs, part(p, i+1), part(x, i))
      bs == nothing && return
    end
    return bs
  elseif tag(p) == :Isa
    return Bool(vinvoke(mod, Symbol("matches?"), x, mod[part(p, 1)])) ? bs : nothing
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
