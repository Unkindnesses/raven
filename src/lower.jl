using IRTools
using IRTools: IR, Slot, argument!

isfn(x) = x isa Block && x.name == :fn

lower!(ir::IR, x::Integer) = x
lower!(ir::IR, x::Symbol) = Slot(x)

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

function lower!(ir::IR, ex::Block)
  if ex.name == :while
    header = IRTools.block!(ir)
    cond = lower!(ir, ex.args[1])
    body = IRTools.block!(ir)
    foreach(x -> lower!(ir, x), ex.block)
    after = IRTools.block!(ir)
    IRTools.branch!(header, after.id, unless = cond)
    IRTools.branch!(body, header.id)
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
