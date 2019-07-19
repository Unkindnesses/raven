using IRTools
using IRTools: IR, Slot, argument!, blocks

# don't continue lowering after return
# e.g. `f(return 1)`
_push!(ir::IR, x) = IRTools.canbranch(blocks(ir)[end]) && push!(ir, x)

# lower while ignoring return value (if applicable)
_lower!(ir, x) = lower!(ir, x)

isfn(x) = x isa Block && x.name == :fn

lower!(ir::IR, x::Integer) = x
lower!(ir::IR, x::Symbol) = Slot(x)
lower!(ir::IR, x::Vector) = (foreach(x -> _lower!(ir, x), x[1:end-1]); lower!(ir, x[end]))

function lower!(ir::IR, ex::Operator)
  if ex.op == :(:=)
    x = Slot(ex.args[1])
    _push!(ir, :($(x) = $(lower!(ir, ex.args[2]))))
    return x
  else
    _push!(ir, Base.Expr(:call, ex.op, map(x -> lower!(ir, x), ex.args)...))
  end
end

function lower!(ir::IR, ex::Return)
  IRTools.return!(ir, lower!(ir, ex.val))
  return
end

function lower!(ir::IR, ex::If, value = true)
  b = blocks(ir)[end]
  ts = []
  vs = []
  body!(ir, ex) =
    value ?
      push!(vs, lower!(ir, ex)) :
      _lower!(ir, ex)
  for (cond, body) in zip(ex.cond, ex.body)
    if cond === true
      body!(ir, body)
      push!(ts, b)
      b = IRTools.block!(ir)
      break
    end
    cond = lower!(ir, cond)
    t = IRTools.block!(ir)
    push!(ts, t)
    body!(ir, body)
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

_lower!(ir::IR, ex::If) = lower!(ir, ex, false)

function lower!(ir::IR, ex::Block)
  if ex.name == :while
    header = IRTools.block!(ir)
    cond = lower!(ir, ex.args[1])
    body = IRTools.block!(ir)
    lower!(ir, ex.block)
    after = IRTools.block!(ir)
    IRTools.branch!(header, after, unless = cond)
    IRTools.branch!(body, header)
  else
    error("unrecognised block $(b.name)")
  end
end

function lowerfn(ex)
  ir = IR()
  for arg in ex.args[1].args
    push!(ir, :($(Slot(arg)) = $(argument!(ir))))
  end
  out = lower!(ir, ex.block)
  out == nothing || IRTools.return!(ir, out)
  return ir |> IRTools.ssa! |> IRTools.prune!
end

# lowerfn(vs"""
#   fn pow(x, n):
#     r := 1
#     while n > 0:
#       n := n - 1
#       r := r * x
#     return r
#   """)

# lowerfn(vs"""
#   fn relu(x):
#     if x > 0:
#       x := x + 1
#     else:
#       x := x - 1
#     return x
#   """)
