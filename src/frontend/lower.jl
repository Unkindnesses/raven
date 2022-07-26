# Pattern Lowering

struct Hole end

const hole = Hole()

struct Literal
  value
end

struct Repeat
  pattern
end

struct Bind
  name::Symbol
  pattern
end

struct Isa
  pattern
end

struct Or
  patterns::Vector{Any}
end

struct And
  patterns::Vector{Any}
end

struct Swap
  pattern
end

function lowerisa(ex, as)
  if ex isa Symbol
    return Isa(ex)
  elseif ex isa AST.Operator && ex.op == :(|)
    Or(map(x -> lowerisa(x, as), ex.args))
  elseif ex isa Operator && ex.op == :(&)
    And(map(x -> lowerisa(x, as), ex.args))
  else
    _lowerpattern(ex, as)
  end
end

function _lowerpattern(ex, as)
  if ex isa Symbol
    ex == :_ || ex in as || push!(as, ex)
    return ex == :_ ? hole : Bind(ex, hole)
  elseif ex isa Union{Primitive,AST.Quote}
    ex isa AST.Quote && (ex = ex.expr)
    return Literal(ex)
  elseif ex isa AST.Tuple
    data(Literal(:Tuple), map(x -> _lowerpattern(x, as), ex.args)...)
  elseif ex isa AST.Operator && ex.op == :(:)
    name, T = ex.args
    name in as || push!(as, name)
    Bind(name, lowerisa(T, as))
  elseif ex isa AST.Splat
    Repeat(_lowerpattern(ex.expr, as))
  elseif ex isa AST.Call && ex.func == :data
    data(map(x -> _lowerpattern(x, as), ex.args)...)
  else
    error("Invalid pattern syntax $(ex)")
  end
end

# At the top level, &x is allowed.
# TODO: swap should be part of the pattern, so we can reject swaps that mismatch
# the signature.
function _lowersig(ex, as, swaps)
  ex isa AST.Tuple || return _lowerpattern(ex, as)
  args = map(enumerate(ex.args)) do (i, x)
    if x isa AST.Swap
      swaps[i] = x.op
      x.op in as || push!(as, x.op)
      Bind(x.op, Hole())
    else
      _lowerpattern(x, as)
    end
  end
  data(Literal(:Tuple), args...)
end

function lowerpattern(ex)
  as = []
  swaps = Dict{Int,Symbol}()
  p = _lowersig(ex, as, swaps)
  return Signature(p, as, swaps)
end

# Expr -> IR lowering

xcall(args...) = Expr(:call, args...)
xdata(args...) = Expr(:data, args...)

function IRTools.Inner.print_stmt(io::IO, ::Val{:data}, ex)
  if ex.args[1] == :Tuple
    print(io, "[")
    join(io, [sprint(vprint, x) for x in ex.args[2:end]], ", ")
    print(io, "]")
  else
    print(io, "data[")
    join(io, [sprint(vprint, x) for x in ex.args], ", ")
    print(io, "]")
  end
end

struct Global
  name::Symbol
end

Base.show(io::IO, g::Global) = print(io, g.name)

struct GlobalScope
  defs::Vector{Symbol}
end

GlobalScope() = GlobalScope([])

Base.getindex(g::GlobalScope, x::Symbol) = Global(x)
Base.haskey(sc::GlobalScope, x::Symbol) = x in sc.defs

variable!(sc::GlobalScope, name) = Global(name)

swaps(sc::GlobalScope) = nothing

struct Scope
  parent::Any
  env::Dict{Symbol,Slot}
  swap::Union{Dict{Int,Symbol},Nothing} # slight hack; store for `return` lowering
end

Scope(parent; swap = nothing) = Scope(parent, Dict{Symbol,Any}(), swap)
Scope(; swap = nothing) = Scope(GlobalScope(); swap)

@forward Scope.env Base.setindex!

Base.getindex(sc::Scope, x::Symbol) = haskey(sc.env, x) ? sc.env[x] : sc.parent[x]
Base.haskey(sc::Scope, x::Symbol) = haskey(sc.env, x) || haskey(sc.parent, x)

variable!(sc::Scope, name::Symbol) =
  haskey(sc, name) ? sc[name] : (sc[name] = Slot(gensym(name)))

swaps(sc::Scope) = sc.swap == nothing ? swaps(sc.parent) : sc.swap

# don't continue lowering after return
# e.g. `f(return 1)`
_push!(ir::IR, x) = IRTools.canbranch(blocks(ir)[end]) && push!(ir, x)

# lower while ignoring return value (if applicable)
_lower!(sc, ir, x) = lower!(sc, ir, x)

isfn(x) = x isa Block && x.name == :fn

lower!(sc, ir::IR, x::Union{Integer,String,AST.Quote,Data}) = x
lower!(sc, ir::IR, x::Symbol) = sc[x]
lower!(sc, ir::IR, x::Vector) =
  isempty(x) ? nothing : (foreach(x -> _lower!(sc, ir, x), x[1:end-1]); lower!(sc, ir, x[end]))

lower!(sc, ir::IR, x::AST.Block) = lower!(sc, ir, x.args)

function lower!(sc, ir::IR, ex::AST.Operator, value = true)
  if ex.op == :(=)
    x = variable!(sc, ex.args[1])
    _push!(ir, :($(x) = $(lower!(sc, ir, ex.args[2]))))
    return x
  elseif ex.op in (:(&&), :(||))
    clauses = ex.op == :(&&) ? [ex.args[2], Int32(false)] : [true, ex.args[2]]
    lowerif!(sc, ir, If([ex.args[1], true], clauses), value)
  else
    r = _push!(ir, xcall(ex.op, xdata(:Tuple, map(x -> lower!(sc, ir, x), ex.args)...)))
    _push!(ir, xcall(part_method, r, 1))
  end
end

_lower!(sc, ir::IR, ex::AST.Operator) = lower!(sc, ir, ex, false)

function argtuple!(sc, ir::IR, args)
  args = collect(args)
  swaps = []
  parts = []
  idx = 1
  splat = false
  while !isempty(args)
    if first(args) isa AST.Splat
      push!(parts, lower!(sc, ir, popfirst!(args).expr))
      splat = true
    else
      as = []
      while !(isempty(args) || first(args) isa AST.Splat)
        arg = popfirst!(args)
        if arg isa AST.Swap && !splat
          arg = arg.op
          push!(swaps, arg => idx)
        end
        push!(as, lower!(sc, ir, arg))
        idx += 1
      end
      push!(parts, _push!(ir, xdata(:Tuple, as...)))
    end
  end
  args =
    isempty(parts) ? xdata(:Tuple) :
    length(parts) == 1 ? parts[1] :
    _push!(ir, xcall(datacat_method, xdata(:Tuple, parts...)))
  return args, swaps
end

function lower!(sc, ir::IR, ex::AST.Call)
  args, swaps = argtuple!(sc, ir, ex.args)
  result = _push!(ir, xcall(lower!(sc, ir, ex.func), args))
  val = _push!(ir, xcall(part_method, result, 1))
  for (x, i) in swaps
    _push!(ir, Expr(:(=), variable!(sc, x), xcall(part_method, result, i+1)))
  end
  return val
end

function lower!(sc, ir::IR, ex::AST.Tuple)
  # TODO: should use the `tuple` function.
  # But this puts off the need for special argument inference.
  argtuple!(sc, ir, ex.args)[1]
end

function swapreturn!(ir::IR, val, swaps)
  if swaps != nothing && !isempty(swaps)
    args = maximum(keys(swaps))
    ret = push!(ir, xdata(:Tuple, val, map(i -> haskey(swaps, i) ? Slot(swaps[i]) : Global(:nil), 1:args)...))
    return!(ir, ret)
  else
    return!(ir, val)
  end
end

function lower!(sc, ir::IR, ex::AST.Return)
  result = lower!(sc, ir, ex.val)
  swapreturn!(ir, result, swaps(sc))
  return
end

function lower!(sc, ir::IR, ex::AST.Break)
  IRTools.branch!(ir, -1)
  return
end

function lowerwhile!(sc, ir::IR, ex, value = true)
  sc = Scope(sc)
  header = IRTools.block!(ir)
  cond = lower!(sc, ir, ex.args[1])
  cond = _push!(ir, xcall(:condition, xdata(:Tuple, cond)))
  cond = _push!(ir, xcall(part_method, cond, 1))
  IRTools.block!(ir)
  _lower!(sc, ir, ex.args[2].args)
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
  IRTools.canbranch(body) && IRTools.branch!(body, header)
  return value ? Global(:nil) : nothing
end

struct If
  cond::Vector{Any}
  body::Vector{Any}
end

function If(b::AST.Syntax)
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
    push!(body, AST.Call(:data, [AST.Quote(:Nil)]))
  end
  return If(cond, body)
end

function lowerif!(sc, ir::IR, ex::If, value = true)
  sc = Scope(sc)
  ts = []
  vs = []
  body!(ir, ex) =
    value ?
      push!(vs, lower!(sc, ir, ex)) :
      _lower!(sc, ir, ex)
  for (cond, body) in zip(ex.cond, ex.body)
    if cond === true
      body!(ir, body)
      push!(ts, blocks(ir)[end])
      IRTools.block!(ir)
      break
    end
    cond = lower!(sc, ir, cond)
    cond = _push!(ir, xcall(:condition, xdata(:Tuple, cond)))
    cond = _push!(ir, xcall(part_method, cond, 1))
    c = blocks(ir)[end]
    t = IRTools.block!(ir)
    body!(ir, body)
    push!(ts, blocks(ir)[end])
    f = IRTools.block!(ir)
    IRTools.branch!(c, f, unless = cond)
  end
  b = blocks(ir)[end]
  for i = 1:length(ts)
    IRTools.canbranch(ts[i]) &&
      (value ?
        IRTools.branch!(ts[i], b, vs[i]) :
        IRTools.branch!(ts[i], b))
  end
  value && IRTools.argument!(b, insert = false)
end

function lowerlet!(sc, ir::IR, ex, value = true)
  sc = Scope(sc)
  @assert length(ex.args) == 1
  (value ? lower! : _lower!)(sc, ir, ex.args[1])
end

function lower!(sc, ir::IR, ex::AST.Syntax, value = true)
  if ex.name == :while
    lowerwhile!(sc, ir, ex, value)
  elseif ex.name == :if
    lowerif!(sc, ir, If(ex), value)
  elseif ex.name == :wasm
    ex = ex.args[1].args[1]
    op = intrinsic(ex)
    args = lower!.((sc,), (ir,), intrinsic_args(ex))
    push!(ir, xcall(op, args...))
  elseif ex.name == :import
    push!(ir, Expr(:import, importpath(ex)))
  elseif ex.name == :let
    lowerlet!(sc, ir, ex, value)
  else
    error("unrecognised block $(ex.name)")
  end
end

_lower!(sc, ir::IR, ex::AST.Syntax) = lower!(sc, ir, ex, false)

function lowerfn(ex, sig)
  sc = Scope(swap = sig.swap)
  ir = IR()
  for arg in sig.args
    sc[arg] = Slot(arg)
    push!(ir, :($(Slot(arg)) = $(argument!(ir))))
  end
  out = lower!(sc, ir, ex.args[2])
  out == nothing || swapreturn!(ir, out, sig.swap)
  return ir |> IRTools.ssa! |> IRTools.prune! |> IRTools.renumber
end

function lowerexpr(ex)
  sc = Scope()
  ir = IR()
  out = lower!(sc, ir, ex)
  out == nothing || IRTools.return!(ir, out)
  # return ir |> IRTools.ssa! |> IRTools.prune!
  return ir
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
  _lower!(sc, ir, ex)
  IRTools.return!(ir, Global(:nil))
  ir = rewrite_globals(ir)
  return ir |> IRTools.ssa! |> IRTools.prune!
end
