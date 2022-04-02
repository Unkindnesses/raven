# Pattern Lowering

const hole = data(:Hole)
bind(name, pattern) = data(:Bind, name, pattern)

function lowerisa(ex, as)
  if ex isa Symbol
    return data(:Isa, ex)
  elseif ex isa Operator && ex.op == :(|)
    data(:Or, map(x -> lowerisa(x, as), ex.args)...)
  elseif ex isa Operator && ex.op == :(&)
    data(:And, map(x -> lowerisa(x, as), ex.args)...)
  else
    _lowerpattern(ex, as)
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
  elseif ex isa Splat
    inner = _lowerpattern(ex.expr, as)
    if tag(inner) == :Bind
      data(:Bind, part(inner, 1), data(:Repeat, part(inner, 2)))
    else
      data(:Repeat, inner)
    end
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

# Expr -> IR lowering

struct Global
  name::Symbol
end

struct GlobalScope
  defs::Vector{Symbol}
end

GlobalScope() = GlobalScope([])

Base.getindex(g::GlobalScope, x::Symbol) = Global(x)
Base.haskey(sc::GlobalScope, x::Symbol) = x in sc.defs

variable!(sc::GlobalScope, name) = Global(name)

struct Scope
  parent::Any
  env::Dict{Symbol,Slot}
end

Scope(parent) = Scope(parent, Dict{Symbol,Any}())
Scope() = Scope(GlobalScope())

@forward Scope.env Base.setindex!

Base.getindex(sc::Scope, x::Symbol) = haskey(sc.env, x) ? sc.env[x] : sc.parent[x]
Base.haskey(sc::Scope, x::Symbol) = haskey(sc.env, x) || haskey(sc.parent, x)

variable!(sc::Scope, name::Symbol) =
  haskey(sc, name) ? sc[name] : (sc[name] = Slot(gensym(name)))

# don't continue lowering after return
# e.g. `f(return 1)`
_push!(ir::IR, x) = IRTools.canbranch(blocks(ir)[end]) && push!(ir, x)

# lower while ignoring return value (if applicable)
_lower!(sc, ir, x) = lower!(sc, ir, x)

isfn(x) = x isa Block && x.name == :fn

lower!(sc, ir::IR, x::Union{Integer,String,Quote,Data}) = x
lower!(sc, ir::IR, x::Symbol) = sc[x]
lower!(sc, ir::IR, x::Vector) =
  isempty(x) ? nothing : (foreach(x -> _lower!(sc, ir, x), x[1:end-1]); lower!(sc, ir, x[end]))

lower!(sc, ir::IR, x::Block) = lower!(sc, ir, x.args)

function lower!(sc, ir::IR, ex::Operator)
  if ex.op == :(=)
    x = variable!(sc, ex.args[1])
    _push!(ir, :($(x) = $(lower!(sc, ir, ex.args[2]))))
    return x
  else
    _push!(ir, Base.Expr(:call, ex.op, Base.Expr(:tuple, map(x -> lower!(sc, ir, x), ex.args)...)))
  end
end

# TODO: should possibly have a primitive `data(...)` expression
# rather than special-casing `tuple`.
function lower!(sc, ir::IR, ex::Call)
  args = collect(ex.args)
  parts = []
  while !isempty(args)
    if first(args) isa Splat
      push!(parts, lower!(sc, ir, popfirst!(args).expr))
    else
      as = []
      while !(isempty(args) || first(args) isa Splat)
        push!(as, lower!(sc, ir, popfirst!(args)))
      end
      push!(parts, _push!(ir, Base.Expr(:tuple, as...)))
    end
  end
  args =
    isempty(parts) ? Base.Expr(:tuple) :
    length(parts) == 1 ? parts[1] :
    _push!(ir, Base.Expr(:call, :datacat, Base.Expr(:tuple, parts...)))
  _push!(ir, Base.Expr(:call, lower!(sc, ir, ex.func), args))
end

function lower!(sc, ir::IR, ex::Tuple)
  _push!(ir, Base.Expr(:call, :tuple, Base.Expr(:tuple, lower!.((sc,), (ir,), ex.args)...)))
end

function lower!(sc, ir::IR, ex::Return)
  IRTools.return!(ir, lower!(sc, ir, ex.val))
  return
end

function lower!(sc, ir::IR, ex::Break)
  IRTools.branch!(ir, -1)
  return
end

function lowerwhile!(sc, ir::IR, ex)
  sc = Scope(sc)
  header = IRTools.block!(ir)
  cond = lower!(sc, ir, ex.args[1])
  IRTools.block!(ir)
  lower!(sc, ir, ex.args[2].args)
  body = blocks(ir)[end]
  after = IRTools.block!(ir)
  # Rewrite continue/break to the right block number
  for i = header.id:body.id
    bl = block(ir, i)
    for j = 1:length(branches(bl))
      br = branches(bl)[j]
      br.block == -1 && (branches(bl)[j] = IRTools.Branch(br, block = after.id))
    end
  end
  IRTools.branch!(header, after, unless = cond)
  IRTools.branch!(body, header)
  return rnothing
end

struct If
  cond::Vector{Any}
  body::Vector{Any}
end

function If(b::Syntax)
  cond = []
  body = []
  push!(cond, b.args[1])
  push!(body, b.args[2])
  args = b.args[3:end]
  while !isempty(args)
    @assert popfirst!(args) == :else "Broken if block"
    if args[1] == :if
      popfirst!(args)
      push!(cond, popfirst!(args))
    else
      push!(cond, true)
    end
    push!(body, popfirst!(args))
  end
  if cond[end] !== true
    push!(cond, true)
    push!(body, Block([rnothing]))
  end
  return If(cond, body)
end

function lowerif!(sc, ir::IR, ex::If, value = true)
  sc = Scope(sc)
  b = blocks(ir)[end]
  ts = []
  vs = []
  body!(ir, ex) =
    value ?
      push!(vs, lower!(sc, ir, ex)) :
      _lower!(sc, ir, ex)
  for (cond, body) in zip(ex.cond, ex.body)
    if cond === true
      body!(ir, body.args)
      push!(ts, b)
      b = IRTools.block!(ir)
      break
    end
    cond = lower!(sc, ir, cond)
    t = IRTools.block!(ir)
    push!(ts, t)
    body!(ir, body.args)
    f = IRTools.block!(ir)
    IRTools.branch!(b, f, unless = cond)
    b = f
  end
  for i = 1:length(ts)
    IRTools.canbranch(ts[i]) &&
      (value ?
        IRTools.branch!(ts[i], b, vs[i]) :
        IRTools.branch!(ts[i], b))
  end
  value && IRTools.argument!(b, insert = false)
end

function lower!(sc, ir::IR, ex::Syntax, value = true)
  if ex.name == :while
    lowerwhile!(sc, ir, ex)
  elseif ex.name == :if
    lowerif!(sc, ir, If(ex), value)
  elseif ex.name == :wasm
    ex = ex.args[1].args[1]
    op = intrinsic(ex)
    args = lower!.((sc,), (ir,), intrinsic_args(ex))
    push!(ir, Base.Expr(:call, op, args...))
  elseif ex.name == :import
    push!(ir, Base.Expr(:import, importpath(ex)))
  else
    error("unrecognised block $(ex.name)")
  end
end

_lower!(sc, ir::IR, ex::Syntax) = lower!(sc, ir, ex, false)

function lowerfn(ex, args)
  sc = Scope()
  ir = IR()
  for arg in args
    sc[arg] = Slot(arg)
    push!(ir, :($(Slot(arg)) = $(argument!(ir))))
  end
  out = lower!(sc, ir, ex.args[2])
  out == nothing || IRTools.return!(ir, out)
  return ir |> IRTools.ssa! |> IRTools.prune! |> IRTools.renumber
end

function lowerexpr(ex)
  sc = Scope()
  ir = IR()
  out = lower!(sc, ir, ex)
  out == nothing || IRTools.return!(ir, out)
  return ir |> IRTools.ssa! |> IRTools.prune!
end

function rewrite_globals(ir::IR)
  gs = []
  for (v, st) in ir
    if isexpr(st.expr, :(=)) && (g = st.expr.args[1]) isa Global
      g in gs || push!(gs, g)
    end
  end
  slots = Dict(g => Slot(g.name) for g in gs)
  ir = IRTools.prewalk(x -> get(slots, x, x), ir)
  for g in reverse(gs)
    pushfirst!(ir, :($(slots[g]) = $g))
  end
  for g in gs
    push!(ir, :($g = $(slots[g])))
  end
  return ir
end

function lower_toplevel(ex, defs = [])
  sc = GlobalScope(defs)
  ir = IR()
  out = lower!(sc, ir, ex)
  IRTools.return!(ir, out)
  ir = rewrite_globals(ir)
  return ir |> IRTools.ssa! |> IRTools.prune!
end
