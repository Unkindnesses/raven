import Base: map, map!
using Core: Typeof
import MacroTools: walk, prewalk, postwalk

xcall(f, args...) = Expr(:call, f, args...)

function map(f, b::BasicBlock)
  stmts = map(x -> Statement(x, expr = f(x.expr)), b.stmts)
  BasicBlock(stmts, b.args, b.argtypes)
end

function map!(f, b::BasicBlock)
  map!(x -> Statement(x, expr = f(x.expr)), b.stmts, b.stmts)
  return b
end

function map!(f, b::Block)
  map!(f, BasicBlock(b))
end

function map(f, ir::IR)
  IR(ir.defs, map.(f, ir.blocks), ir.meta)
end

function map!(f, ir::IR)
  for b in blocks(ir)
    map!(f, b)
  end
  return ir
end

walk(st::Statement, inner, outer) = Statement(st, expr = inner(st.expr))
walk(bb::BasicBlock, inner, outer) = map(inner, bb)
walk(b::Block, inner, outer) = walk(BasicBlock(b), inner, outer)

walk(ir::IR, inner, outer) = outer(map(inner, ir))

# Avoid recursing into lambdas
prewalk(f, ir::Union{IR,Block})  = walk(f(ir), x -> x isa IR ? x : prewalk(f, x), identity)
postwalk(f, ir::Union{IR,Block}) = walk(ir, x -> x isa IR ? x : postwalk(f, x), f)

prewalk!(f, ir::Union{IR,Block})  = map!(x -> x isa IR ? x :  prewalk(f, x), ir)
postwalk!(f, ir::Union{IR,Block}) = map!(x -> x isa IR ? x : postwalk(f, x), ir)

varmap(f, x) = prewalk(x -> x isa Variable ? f(x) : x, x)

exprtype(ir::IR, x::Expr; typeof = Typeof) = error(x)
exprtype(ir::IR, x; typeof = Typeof) = typeof(x)

function exprtype(ir::IR, x::Variable; typeof = Typeof)
  b, i = get(ir.defs, x.id, (-1, -1))
  b == -1 && error("No such variable $x")
  if i > 0
    ir[x].type
  else
    ir.blocks[b].argtypes[-i]
  end
end
