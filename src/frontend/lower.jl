using IRTools
using IRTools: IR, Slot, argument!

isfn(x) = x isa Block && x.name == :fn

lower!(ir::IR, x::Integer) = x
lower!(ir::IR, x::Symbol) = Slot(x)
lower!(ir::IR, x::Vector) = foreach(x -> lower!(ir, x), x)

function lower!(ir::IR, ex::Operator)
  if ex.op == :(:=)
    push!(ir, :($(Slot(ex.args[1])) = $(lower!(ir, ex.args[2]))))
  else
    push!(ir, Base.Expr(:call, ex.op, map(x -> lower!(ir, x), ex.args)...))
  end
end

function lower!(ir::IR, ex::Return)
  IRTools.return!(ir, lower!(ir, ex.val))
end

function lower!(ir::IR, ex::If)
  b = IRTools.blocks(ir)[end]
  ts = []
  for (cond, body) in zip(ex.cond, ex.body)
    if cond === true
      lower!(ir, body)
      b = IRTools.block!(ir)
      break
    end
    cond = lower!(ir, cond)
    t = IRTools.block!(ir)
    push!(ts, t)
    lower!(ir, body)
    f = IRTools.block!(ir)
    IRTools.branch!(b, f, unless = cond)
    b = f
  end
  for t in ts
    IRTools.canbranch(t) && IRTools.branch!(t, b)
  end
end

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
  for x in ex.block
    lower!(ir, x)
  end
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
