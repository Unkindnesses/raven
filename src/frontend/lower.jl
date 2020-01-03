struct GlobalScope end

Base.getindex(g::GlobalScope, x::Symbol) = x

struct Scope
  parent::Any
  env::Dict{Symbol,Any}
end

Scope(parent) = Scope(parent, Dict{Symbol,Any}())
Scope() = Scope(GlobalScope())

@forward Scope.env Base.setindex!

Base.getindex(sc::Scope, x::Symbol) = haskey(sc.env, x) ? sc.env[x] : sc.parent[x]

# don't continue lowering after return
# e.g. `f(return 1)`
_push!(ir::IR, x) = IRTools.canbranch(blocks(ir)[end]) && push!(ir, x)

# lower while ignoring return value (if applicable)
_lower!(sc, ir, x) = lower!(sc, ir, x)

isfn(x) = x isa Block && x.name == :fn

lower!(sc, ir::IR, x::Union{Integer,String,Quote}) = x
lower!(sc, ir::IR, x::Symbol) = sc[x]
lower!(sc, ir::IR, x::Vector) =
  isempty(x) ? nothing : (foreach(x -> _lower!(sc, ir, x), x[1:end-1]); lower!(sc, ir, x[end]))

function lower!(sc, ir::IR, ex::Operator)
  if ex.op == :(=)
    x = Slot(ex.args[1])
    sc[ex.args[1]] = x
    _push!(ir, :($(x) = $(lower!(sc, ir, ex.args[2]))))
    return x
  else
    _push!(ir, Base.Expr(:call, ex.op, map(x -> lower!(sc, ir, x), ex.args)...))
  end
end

function lower!(sc, ir::IR, ex::Call)
  _push!(ir, Base.Expr(:call, lower!(sc, ir, ex.func), lower!.((sc,), (ir,), ex.args)...))
end

function lower!(sc, ir::IR, ex::Tuple)
  _push!(ir, Base.Expr(:call, :tuple, lower!.((sc,), (ir,), ex.args)...))
end

function lower!(sc, ir::IR, ex::Return)
  IRTools.return!(ir, lower!(sc, ir, ex.val))
  return
end

struct If
  cond::Vector{Any}
  body::Vector{Any}
end

function If(b::Syntax)
  cond = []
  body = []
  while true
    if b.name == :if && isempty(cond)
      push!(cond, b.args[1])
      push!(body, b.args[2])
      get(b.args, 3, nothing) isa Syntax ? (b = b.args[3]) : break
    elseif b.name == :else && b.args[1] == :if
      push!(cond, b.args[2])
      push!(body, b.args[3])
      get(b.args, 4, nothing) isa Syntax ? (b = b.args[4]) : break
    elseif b.name == :else
      push!(cond, true)
      push!(body, b.args[1])
      break
    else error("broken if block")
    end
  end
  return If(cond, body)
end

function lowerif!(sc, ir::IR, ex::If, value = true)
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
      value ?
        IRTools.branch!(ts[i], b, vs[i]) :
        IRTools.branch!(ts[i], b)
  end
  value && IRTools.argument!(b, insert = false)
end

function lower!(sc, ir::IR, ex::Syntax, value = true)
  if ex.name == :while
    header = IRTools.block!(ir)
    cond = lower!(sc, ir, ex.args[1])
    body = IRTools.block!(ir)
    lower!(sc, ir, ex.args[2].args)
    after = IRTools.block!(ir)
    IRTools.branch!(header, after, unless = cond)
    IRTools.branch!(body, header)
    return rnothing
  elseif ex.name == :if
    lowerif!(sc, ir, If(ex), value)
  elseif ex.name == :wasm
    ex = ex.args[1].args[1]
    op = intrinsic(ex)
    args = lower!.((sc,), (ir,), intrinsic_args(ex))
    push!(ir, Base.Expr(:call, op, args...))
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
  out = lower!(sc, ir, ex.args[2].args)
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
