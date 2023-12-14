import Base: push!, insert!, getindex, setindex!, iterate, length

struct Undefined end
const undef = Undefined()

struct Variable
  id::Int
end

var(id::Integer) = Variable(id)

isvariable(x) = false
isvariable(::Variable) = true

Base.copy(x::Variable) = x

struct Slot
  id::Symbol
  type
end

Slot(id) = Slot(id, Any)

function Base.show(io::IO, s::Slot)
  print(io, "@", s.id)
  s.type != Any && print(io, "::", s.type)
end

branch(block::Integer, args...; when = nothing) =
  Expr(:branch, block, when, args...)

function arguments(ex::Expr)
  @assert isexpr(ex, :branch)
  @view ex.args[3:end]
end

function isreturn(ex::Expr)
  @assert isexpr(ex, :branch)
  ex.args[1] == 0 && length(arguments(ex)) == 1
end

function isunreachable(ex::Expr)
  @assert isexpr(ex, :branch)
  ex.args[1] == 0 && length(arguments(ex)) == 0
end

function isconditional(ex::Expr)
  @assert isexpr(ex, :branch)
  ex.args[2] != nothing
end

const unreachable = branch(0)

struct Source
  file::String
  line::Int
  col::Int
end

struct Statement
  expr::Any
  type::Any
  src::Union{Source,Nothing}
  bp::Bool
end

Statement(expr; type = Any, src = nothing, bp = false) =
  Statement(expr, type, src, bp)

Statement(x::Statement; expr = x.expr, type = x.type, src = x.src, bp = x.bp) =
  Statement(expr, type, src, bp)

MacroTools.isexpr(st::Statement, ts...) = isexpr(st.expr, ts...)

Base.copy(::Nothing) = nothing
Base.copy(st::Statement) = Statement(copy(st.expr), st.type, st.src, st.bp)

const stmt = Statement

arguments(ex::Statement) = arguments(ex.expr)
isreturn(ex::Statement) = isreturn(ex.expr)

struct BasicBlock
    stmts::Vector{Tuple{Variable,Statement}}
    args::Vector{Variable}
    argtypes::Vector{Any}
end

BasicBlock(stmts = []) = BasicBlock(stmts, [], [])

Base.copy(bb::BasicBlock) = BasicBlock(map(x -> copy.(x), bb.stmts), copy(bb.args), copy(bb.argtypes))

# TODO filter deleted
branches(bb::BasicBlock) = filter(x -> isexpr(x, :branch), map(x -> x[2].expr, bb.stmts))

arguments(bb::BasicBlock) = bb.args

argtypes(bb::BasicBlock) = bb.argtypes

struct IR
  defs::Vector{Tuple{Int,Int}}
  blocks::Vector{BasicBlock}
  meta::Any
end

IR(; meta = nothing) = IR([],[BasicBlock()],meta)

Base.copy(ir::IR) = IR(copy(ir.defs), copy.(ir.blocks), ir.meta)

length(ir::IR) = count(x -> x[2] > 0, ir.defs)

function block!(ir::IR)
  push!(ir.blocks, BasicBlock())
  return block(ir, length(ir.blocks))
end

function deleteblock!(ir::IR, i::Integer)
  deleteat!(ir.blocks, i)
  if i != length(ir.blocks)+1
    for b in blocks(ir), bi = 1:length(branches(b))
      br = branches(b)[bi]
      br.args[1] >= i && (br.args[1] = br.args[1]-1)
    end
  end
  for (ii, (b, j)) = enumerate(ir.defs)
    b == i && (ir.defs[ii] = (-1, -1))
    b > i && (ir.defs[ii] = (b-1, j))
  end
  return
end

struct Block
  ir::IR
  id::Int
end

BasicBlock(b::Block) = b.ir.blocks[b.id]
branches(b::Block) = branches(BasicBlock(b))

arguments(b::Block) = arguments(BasicBlock(b))
arguments(ir::IR) = arguments(block(ir, 1))

argtypes(b::Block) = argtypes(BasicBlock(b))
argtypes(ir::IR) = argtypes(block(ir, 1))

canbranch(b::Block) = length(branches(b)) == 0 || isconditional(branches(b)[end])

isreturn(b::Block) = any(isreturn, branches(b))

function branches(b::Block, c::Block)
  filter(br -> br.args[1] == c.id, branches(b))
end

branches(b::Block, c::Integer) = branches(b, block(b.ir, c))

function returnvalue(b::Block)
  isreturn(b[end]) || error("Block does not return")
  return only(arguments(b[end]))
end

returntype(b::Block) = exprtype(b.ir, returnvalue(b))

function argument!(b::Block, value = nothing, t = Any;
                   insert = true, at = length(arguments(b))+1, type = t)
  if at < length(arguments(b))
    for i = 1:length(b.ir.defs)
      (c, j) = b.ir.defs[i]
      c == b.id && -j >= at && (b.ir.defs[i] = (c, j-1))
    end
  end
  push!(b.ir.defs, (b.id, -at))
  arg = var(length(b.ir.defs))
  insert!(arguments(b), at, arg)
  insert!(BasicBlock(b).argtypes, at, type)
  if insert
    for c in blocks(b.ir), br in branches(c)
      br.args[1] == b.id && insert!(br.args, at+2, value)
    end
  end
  return arg
end

argument!(ir::IR, a...; kw...) =
  argument!(block(ir, 1), nothing, a...; kw..., insert = false)

function deletearg!(b::Block, i::Integer)
  arg = arguments(b)[i]
  deleteat!(arguments(b), i)
  deleteat!(argtypes(b), i)
  for c in blocks(b.ir), br in branches(c)
    br.args[1] == b.id && deleteat!(br.args, i+2)
  end
  b.ir.defs[arg.id] = (-1, -1)
  for arg in arguments(b)[i:end]
    (bl, pos) = b.ir.defs[arg.id]
    b.ir.defs[arg.id] = (bl, pos+1)
  end
  return
end

function deletearg!(b::Block, i::AbstractVector)
  for i in sort(i, rev = true)
    deletearg!(b, i)
  end
end

deletearg!(ir::IR, i) = deletearg!(block(ir, 1), i)

block(ir::IR, i) = Block(ir, i)
block(ir::IR, v::Variable) = block(ir, blockidx(ir, v)[1])

blocks(ir::IR) = [block(ir, i) for i = 1:length(ir.blocks)]

function blockidx(ir::IR, x::Variable)
  b, i = get(ir.defs, x.id, (-1, -1))
  i > 0 || error("No such variable $x")
  b, i
end

function Base.haskey(ir::IR, x::Variable)
  b, i = get(ir.defs, x.id, (-1, -1))
  return i > 0
end

Base.haskey(b::Block, x::Variable) = haskey(b.ir, x)

branch(block::Block, args...; kw...) = branch(block.id, args...; kw...)

function branch!(b::Block, block, args...; when = nothing, src = nothing, bp = false)
  brs = branches(b)
  args = map(a -> a isa Expr ? push!(b, a) : a, args)
  push!(b, stmt(branch(block, args...; when); src, bp))
  return b
end

function branch!(ir::IR, args...; kw...)
  branch!(blocks(ir)[end], args...; kw...)
  return ir
end

return!(ir, x; kw...) = branch!(ir, 0, x; kw...)

unreachable!(ir; kw...) = branch!(ir, 0; kw...)

function getindex(ir::IR, v::Variable)
  b, i = blockidx(ir, v)
  return ir.blocks[b].stmts[i][2]
end

Base.get(ir::IR, i::Variable, default) = haskey(ir, i) ? ir[i] : default

function setindex!(ir::IR, x::Statement, v::Variable)
  b, i = blockidx(ir, v)
  ir.blocks[b].stmts[i] = (v, x)
  return x
end

function setindex!(ir::IR, x, v::Variable)
  ir[v] = stmt(ir[v], expr = x)
  return x
end

getindex(b::Block, i::Variable) = b.ir[i]
setindex!(b::Block, x, i::Variable) = setindex!(b.ir, x, i)

function Base.delete!(ir::IR, i::Variable)
  ir[i] = nothing
  ir.defs[i.id] = (-1, -1)
  return ir
end

Base.delete!(b::Block, i::Variable) = delete!(b.ir, i)

length(b::Block) = count(x -> x[1] == b.id && x[2] > 0, b.ir.defs)

function successors(b::Block)
  brs = branches(b)
  succs = Int[br.args[1] for br in brs if br.args[1] > 0]
  return [block(b.ir, succ) for succ in succs]
end

predecessors(b::Block) = [c for c in blocks(b.ir) if b in successors(c)]

Base.keys(b::Block) = filter(v -> haskey(b.ir, v), [v for (v, _) in BasicBlock(b).stmts])

Base.lastindex(b::Block) = last(keys(b))

function iterate(b::Block, i = 1)
  stmts = BasicBlock(b).stmts
  i > length(stmts) && return
  haskey(b, stmts[i][1]) || return iterate(b, i+1)
  return stmts[i], i+1
end

Base.keys(ir::IR) = filter(v -> haskey(ir, v), [v for bl in ir.blocks for (v, _) in bl.stmts])

function iterate(ir::IR, (b, i) = (1, 1))
  b > length(ir.blocks) && return
  (st, i) = @something iterate(block(ir, b), i) (return iterate(ir, (b+1, 1)))
  return st, (b, i)
end

applyex(f, x) = x
applyex(f, x::Expr) =
  Expr(x.head, [x isa Expr ? f(x) : x for x in x.args]...)
applyex(f, x::Statement) = Statement(x, expr = applyex(f, x.expr))

function branch_start(b::BasicBlock)
  i = length(b.stmts)+1
  while i > 1 && isexpr(b.stmts[i-1][2], :branch, Nothing)
    i -= 1
  end
  return i
end

function push!(b::Block, x::Statement)
  if !isexpr(x, :branch) && (bs = branch_start(BasicBlock(b))) <= length(BasicBlock(b).stmts)
    return insert!(b, bs, x)
  end
  x = applyex(a -> push!(b, Statement(a, src = x.src)), x)
  v = var(length(b.ir.defs)+1)
  push!(BasicBlock(b).stmts, (v, x))
  push!(b.ir.defs, (b.id, length(BasicBlock(b).stmts)))
  return v
end

push!(b::Block, x) = push!(b, Statement(x))

push!(b::Block, x::Variable) = x

# TODO make this work on nested Exprs.
function insert!(b::Block, idx::Integer, x)
  v = var(length(b.ir.defs)+1)
  insert!(BasicBlock(b).stmts, idx, (v, Statement(x)))
  for i = 1:length(b.ir.defs)
    c, j = b.ir.defs[i]
    if c == b.id && j >= idx
      b.ir.defs[i] = (c, j+1)
    end
  end
  push!(b.ir.defs, (b.id, idx))
  return v
end

push!(ir::IR, x) = push!(block(ir, length(ir.blocks)), x)

Base.empty(ir::IR) = IR(meta = ir.meta)

# Pipe

mutable struct Pipe
  from::IR
  to::IR
  map::Dict{Variable,Any}
  var::Int
end

var!(p::Pipe) = var(p.var -= 1)

substitute!(p::Pipe, x, y) = (p.map[x] = y; x)
substitute(p::Pipe, x::Variable) = p.map[x]
substitute(p::Pipe, x) = get(p.map, x, x)
substitute(p::Pipe, x::Statement) = stmt(x, expr = substitute(p, x.expr))
substitute(p::Pipe, x::Expr) = Expr(x.head, substitute.((p,), x.args)...)
substitute(p::Pipe) = x -> substitute(p, x)

Pipe(ir) = Pipe(ir, IR(meta = ir.meta), Dict(), 0)

struct PipeBlock
  ir::Pipe
  id::Int
end

function init!(bl::PipeBlock)
  bl.id == 1 || block!(bl.ir.to)
  for (x, T) in zip(bl.ir.from.blocks[bl.id].args, bl.ir.from.blocks[bl.id].argtypes)
    y = argument!(blocks(bl.ir.to)[end], nothing, T, insert = false)
    substitute!(bl.ir, x, y)
  end
  return bl
end

blocks(pr::Pipe) = (init!(PipeBlock(pr, i)) for i = 1:length(pr.from.blocks))

function iterate(bl::PipeBlock, i = 1)
  p = bl.ir
  stmts = p.from.blocks[bl.id].stmts
  i > length(stmts) && return
  v, st = stmts[i]
  haskey(p.from, v) || return iterate(bl, i+1)
  substitute!(p, v, push!(p.to, substitute(p, st)))
  return (v, st), i+1
end

function iterate(p::Pipe, (b, i) = (1, 1))
  b > length(p.from.blocks) && return
  i == 1 && init!(PipeBlock(p, b))
  (st, i) = @something iterate(PipeBlock(p, b), i) (return iterate(p, (b+1, 1)))
  return st, (b, i)
end

finish(p::Pipe) = p.to

islastdef(ir::IR, v::Variable) =
  v.id == length(ir.defs) &&
  ir.defs[v.id] == (length(ir.blocks), length(ir.blocks[end].stmts))

setindex!(p::Pipe, x, v) = p.to[substitute(p, v)] = substitute(p, x)

function setindex!(p::Pipe, x::Variable, v)
  v′ = substitute(p, v)
  if islastdef(p.to, v′)
    delete!(p, v)
    substitute!(p, v, substitute(p, x))
  else
    p.to[v′] = substitute(p, x)
  end
end

function Base.replace!(pr::Pipe, x, y)
  substitute!(pr, x, substitute(pr, y))
end

function Base.push!(p::Pipe, x)
  tmp = var!(p)
  substitute!(p, tmp, push!(p.to, substitute(p, x)))
  return tmp
end

function Base.delete!(p::Pipe, v)
  v′ = substitute(p, v)
  p.map[v] = Variable(0)
  if islastdef(p.to, v′)
    pop!(p.to.defs)
    pop!(p.to.blocks[end].stmts)
  else
    delete!(p.to, v′)
  end
end

function insert!(p::Pipe, v, x; after = false)
  v′ = substitute(p, v)
  x = substitute(p, x)
  tmp = var!(p)
  if islastdef(p.to, v′) # we can make this case efficient by renumbering
    if after
      substitute!(p, tmp, push!(p.to, x))
    else
      substitute!(p, v, push!(p.to, p.to[v′]))
      p.to[v′] = Statement(x)
      substitute!(p, tmp, v′)
    end
  else
    error("not supported")
  end
  return tmp
end
