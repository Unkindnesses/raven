import Base: push!, insert!, getindex, setindex!, iterate, length

# We have our own versions of these in order to
# (1) be more robust to Base IR changes, and
# (2) make sure that mistakes/bugs do not cause bad LLVM IR.

struct Undefined end
const undef = Undefined()

struct Variable
  id::Int
end

var(id::Integer) = Variable(id)

isvariable(x) = false
isvariable(::Variable) = true

Base.copy(x::IRTools.Variable) = x

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

function isconditional(ex::Expr)
  @assert isexpr(ex, :branch)
  ex.args[2] != nothing
end

const unreachable = Expr(:branch, 0, nothing)

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

Statement(x; expr = x, type = Any, src = nothing, bp = false) =
  Statement(expr, type, src, bp)

Statement(x::Statement; expr = x.expr, type = x.type, src = x.src, bp = x.bp) =
  Statement(expr, type, src, bp)

MacroTools.isexpr(st::Statement, ts...) = isexpr(st.expr, ts...)

Base.copy(::Nothing) = nothing
Base.copy(st::Statement) = Statement(copy(st.expr), st.type, st.src, st.bp)

const stmt = Statement

struct BasicBlock
    stmts::Vector{Statement}
    args::Vector{Any}
    argtypes::Vector{Any}
end

BasicBlock(stmts = []) = BasicBlock(stmts, [], [])

Base.copy(bb::BasicBlock) = BasicBlock(copy.(bb.stmts), copy(bb.args), copy(bb.argtypes))

branches(bb::BasicBlock) = filter(x -> isexpr(x, :branch), map(x -> x.expr, bb.stmts))

arguments(bb::BasicBlock) = bb.args

argtypes(bb::BasicBlock) = bb.argtypes

struct IR
  defs::Vector{Tuple{Int,Int}}
  blocks::Vector{BasicBlock}
  meta::Any
end

IR(; meta = nothing) = IR([],[BasicBlock()],meta)

Base.copy(ir::IR) = IR(copy(ir.defs), copy.(ir.blocks), ir.meta)

length(ir::IR) = sum(x -> x[2] > 0, ir.defs, init = 0)

function block!(ir::IR, i = length(blocks(ir))+1)
  insert!(ir.blocks, i, BasicBlock())
  if i != length(blocks(ir))
    for b in blocks(ir), bi = 1:length(branches(b))
      br = branches(b)[bi]
      br.block >= i && (branches(b)[bi] = Branch(br, block = br.block+1))
    end
    for (ii, (b, j)) = enumerate(ir.defs)
      b >= i && (ir.defs[ii] = (b+1, j))
    end
  end
  return block(ir, i)
end

function deleteblock!(ir::IR, i::Integer)
  deleteat!(ir.blocks, i)
  if i != length(blocks(ir))+1
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

branches(ir::IR) = length(blocks(ir)) == 1 ? branches(block(ir, 1)) :
  error("IR has multiple blocks, so `branches(ir)` is ambiguous.")

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
  isreturn(branches(b)[end]) || error("Block does not return")
  return branches(b)[end].args[3]
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

function emptyargs!(b::Block)
  empty!(arguments(b))
  for c in blocks(b.ir), br in branches(c)
    br.args[1] == b.id && empty!(arguments(br))
  end
  return
end

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
  for i in sort(i, lt = >)
    deletearg!(b, i)
  end
end

deletearg!(ir::IR, i) = deletearg!(block(ir, 1), i)

block(ir::IR, i) = Block(ir, i)
block(ir::IR, v::Variable) = blockidx(ir, v)[1]

blocks(ir::IR) = [block(ir, i) for i = 1:length(ir.blocks)]

function blockidx(ir::IR, x::Variable)
  b, i = get(ir.defs, x.id, (-1, -1))
  i > 0 || error("No such variable $x")
  block(ir, b), i
end

function Base.haskey(ir::IR, x::Variable)
  b, i = get(ir.defs, x.id, (-1, -1))
  return i > 0
end

getindex(b::Block, i::Integer) = BasicBlock(b).stmts[i]
getindex(b::Block, i::Variable) = b.ir[i]
setindex!(b::Block, x::Statement, i::Integer) = (BasicBlock(b).stmts[i] = x)
setindex!(b::Block, x, i::Integer) = (b[i] = Statement(b[i], expr = x))

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

return!(ir, x; src = nothing, bp = false) = branch!(ir, 0, x; src, bp)

function getindex(ir::IR, i::Variable)
  b, i = blockidx(ir, i)
  return b[i]
end

Base.get(ir::IR, i::Variable, default) = haskey(ir, i) ? ir[i] : default

function setindex!(ir::IR, x, i::Variable)
  b, i = blockidx(ir, i)
  b[i] = x
end

setindex!(b::Block, x, i::Variable) = setindex!(b.ir, x, i)

function Base.delete!(ir::IR, i::Variable)
  ir[i] = nothing
  ir.defs[i.id] = (-1, -1)
  return ir
end

Base.delete!(b::Block, i::Variable) = delete!(b.ir, i)

length(b::Block) = count(x -> x[1] == b.id, b.ir.defs)

function successors(b::Block)
  brs = branches(b)
  succs = Int[br.args[1] for br in brs if br.args[1] > 0]
  return [block(b.ir, succ) for succ in succs]
end

predecessors(b::Block) = [c for c in blocks(b.ir) if b in successors(c)]

Base.keys(b::Block) = first.(sort([Variable(i) => v for (i, v) in enumerate(b.ir.defs) if v[1] == b.id && v[2] > 0], by = x->x[2]))

function iterate(b::Block, (ks, i) = (keys(b), 1))
  i > length(ks) && return
  return (ks[i]=>b.ir[ks[i]], (ks, i+1))
end

Base.keys(ir::IR) = first.(sort([Variable(i) => v for (i, v) in enumerate(ir.defs) if v[2] > 0], by = x->x[2]))

function iterate(ir::IR, (ks, i) = (keys(ir), 1))
  i > length(ks) && return
  return (ks[i]=>ir[ks[i]], (ks, i+1))
end

applyex(f, x) = x
applyex(f, x::Expr) =
  Expr(x.head, [x isa Expr ? f(x) : x for x in x.args]...)
applyex(f, x::Statement) = Statement(x, expr = applyex(f, x.expr))

function branch_start(b::BasicBlock)
  i = length(b.stmts)+1
  while i > 1 && isexpr(b.stmts[i-1], :branch, Nothing)
    i -= 1
  end
  return i
end

function push!(b::Block, x::Statement)
  bs = branch_start(BasicBlock(b))
  if !isexpr(x, :branch) && bs <= length(BasicBlock(b).stmts)
    return insert!(b, bs, x)
  end
  x = applyex(a -> push!(b, Statement(a, src = x.src)), x)
  push!(BasicBlock(b).stmts, x)
  push!(b.ir.defs, (b.id, length(BasicBlock(b).stmts)))
  return Variable(length(b.ir.defs))
end

push!(b::Block, x) = push!(b, Statement(x))

push!(b::Block, x::Variable) = x

# TODO make this work on nested Exprs.
function insert!(b::Block, idx::Integer, x)
  insert!(BasicBlock(b).stmts, idx, Statement(x))
  for i = 1:length(b.ir.defs)
    c, j = b.ir.defs[i]
    if c == b.id && j >= idx
      b.ir.defs[i] = (c, j+1)
    end
  end
  push!(b.ir.defs, (b.id, idx))
  return Variable(length(b.ir.defs))
end

Base.pushfirst!(b::Block, x) = insert!(b, 1, x)

push!(ir::IR, x) = push!(block(ir, length(ir.blocks)), x)

Base.pushfirst!(ir::IR, x) = pushfirst!(block(ir, 1), x)

function insert!(ir::IR, i::Variable, x; after = false)
  if after && ir.defs[i.id][2] < 0
    pushfirst!(block(ir, ir.defs[i.id][1]), x)
  else
    b, i = blockidx(ir, i)
    insert!(b, i+after, x)
  end
end

insert!(b::Block, i::Variable, x; after = false) =
  insert!(b.ir, i, x; after = after)

insertafter!(ir, i, x) = insert!(ir, i, x, after=true)

Base.empty(ir::IR) = IR(meta = ir.meta)

function Base.permute!(ir::IR, perm::AbstractVector)
  permute!(ir.blocks, perm)
  iperm = invperm(perm)
  for v = 1:length(ir.defs)
    b, i = ir.defs[v]
    b == -1 && continue
    ir.defs[v] = (iperm[b], i)
  end
  for b in blocks(ir), i = 1:length(branches(b))
    branches(b)[i].block > 0 || continue
    br = branches(b)[i]
    branches(b)[i] = Branch(br, block = iperm[br.block])
  end
  return ir
end

function IR(b::Block)
  ir = IR(copy(b.ir.defs), [copy(BasicBlock(b))], b.ir.meta)
  for i in 1:length(ir.defs)
    if ir.defs[i][1] == b.id
      ir.defs[i] = (1, ir.defs[i][2])
    else
      ir.defs[i] = (-1, -1)
    end
  end
  return ir
end

# Pipe

struct NewVariable
  id::Int
end

isvariable(::IRTools.NewVariable) = true

"""
    Pipe(ir)

In general, it is not efficient to insert statements into IR; only appending is
fast, for the same reason as with `Vector`s.

For this reason, the `Pipe` construct makes it convenient to incrementally build
an new IR fragment from an old one, making efficient modifications as you go.

The general pattern looks like:

    pr = IRTools.Pipe(ir)
    for (v, st) in pr
      # do stuff
    end
    ir = IRTools.finish(pr)

Iterating over `pr` is just like iterating over `ir`, except that within the
loop, inserting and deleting statements in `pr` around `v` is efficient. Later,
`finish(pr)` converts it back to a normal IR fragment (in this case just a plain
copy of the original).
"""
mutable struct Pipe
  from::IR
  to::IR
  map::Dict{Any,Any}
  var::Int
end

var!(p::Pipe) = NewVariable(p.var += 1)

substitute!(p::Pipe, x, y) = (p.map[x] = y; x)
substitute(p::Pipe, x::Union{Variable,NewVariable}) = p.map[x]
substitute(p::Pipe, x) = get(p.map, x, x)
substitute(p::Pipe, x::Statement) = stmt(x, expr = substitute(p, x.expr))
substitute(p::Pipe, x::Expr) = Expr(x.head, substitute.((p,), x.args)...)
substitute(p::Pipe) = x -> substitute(p, x)

function Pipe(ir)
  p = Pipe(ir, IR(meta = ir.meta), Dict(), 0)
  for (x, T) in zip(p.from.blocks[1].args, p.from.blocks[1].argtypes)
    y = argument!(blocks(p.to)[end], nothing, T, insert = false)
    substitute!(p, x, y)
  end
  return p
end

function pipestate(ir::IR)
  ks = sort([Variable(i) => v for (i, v) in enumerate(ir.defs) if v[2] > 0], by = x->x[2])
  [first.(filter(x -> x[2][1] == b, ks)) for b = 1:length(ir.blocks)]
end

function iterate(p::Pipe, (ks, b, i) = (pipestate(p.from), 1, 1))
  if i == 1 && b != 1
    for (x, T) in zip(p.from.blocks[b].args, p.from.blocks[b].argtypes)
      y = argument!(blocks(p.to)[end], nothing, T, insert = false)
      substitute!(p, x, y)
    end
  end
  if i > length(ks[b])
    b == length(ks) && return
    block!(p.to)
    return iterate(p, (ks, b+1, 1))
  end
  v = ks[b][i]
  st = p.from[v]
  substitute!(p, v, push!(p.to, substitute(p, st)))
  ((v, st), (ks, b, i+1))
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

function Base.replace!(pr::IRTools.Pipe, x, y)
  IRTools.substitute!(pr, x, IRTools.substitute(pr, y))
end

function Base.push!(p::Pipe, x)
  tmp = var!(p)
  substitute!(p, tmp, push!(p.to, substitute(p, x)))
  return tmp
end

function Base.pushfirst!(p::Pipe, x)
  tmp = var!(p)
  substitute!(p, tmp, pushfirst!(p.to, substitute(p, x)))
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
    substitute!(p, tmp, insert!(p.to, v′, x, after = after))
  end
  return tmp
end

argument!(p::Pipe, a...; kw...) =
  substitute!(p, var!(p), argument!(p.to, a...; kw...))

function block!(p::Pipe)
  block!(p.to)
  return
end

function blockargument!(p::Pipe, type)
  x = argument!(blocks(p.to)[end], nothing, type, insert = false)
  substitute!(p, var!(p), x)
end
