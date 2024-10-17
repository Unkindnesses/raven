using .IRTools: CFG, Block, Component, components, entry

rename(env, ex) =
  IRTools.prewalk(x -> x isa Variable ? env[x] : x, ex)

function copyblock!(c, b)
  env = Dict()
  for (arg, T) in zip(arguments(b), argtypes(b))
    env[arg] = argument!(c, nothing, T, insert = false)
  end
  for (v, st) in b
    env[v] = push!(c, rename(env, st))
  end
  return c
end

mutable struct LoopIR
  ir::IR
  bls::Vector{Int}
  body::Vector{IR}
  max::Int
end

LoopIR(ir::IR, bls::Vector{Int}) = LoopIR(ir, bls, [copy(ir)], 8)

IRTools.argtypes(ir::LoopIR) = argtypes(ir.body[1])

function Base.show(io::IO, ir::LoopIR)
  indent = get(io, :indent, 0)
  io = IOContext(io, :indent => indent+1, :blockmap => i -> ir.bls[i])
  print(io, "loop:")
  for (i, b) in enumerate(ir.body)
    println(io, "\n", IRTools.tab^indent, "#$i:")
    print(IOContext(io, :indent=>indent+1), b)
  end
end

function IRTools.print_stmt(io::IO, ::Val{:loop}, l)
  print(io, l.args[1])
end

function loop(bl::Block)
  if !isempty(bl) && isexpr(first(bl)[2].expr, :loop)
    return first(bl)[2].expr.args[1]
  else
    return nothing
  end
end

function looped(ir::IR, cs = components(CFG(ir)))
  out = IR(meta = ir.meta)
  blocks = Int[]
  for (i, ch) in enumerate(cs)
    bl = i == 1 ? block(out, 1) : block!(out)
    if ch isa Integer
      push!(blocks, ch)
      copyblock!(bl, block(ir, ch))
    else
      push!(blocks, entry(ch))
      args = [argument!(bl, type = T, insert = false) for T in argtypes(block(ir, entry(ch)))]
      push!(bl, Expr(:loop, looped(ir, ch), args...))
    end
  end
  return LoopIR(out, blocks)
end

nblocks(b::Block) = (l = loop(b)) == nothing ? 1 : nblocks(l)
nblocks(b::IR) = sum(nblocks, blocks(b))
nblocks(b::LoopIR) = sum(nblocks, b.body)

function blockmap(l::LoopIR, offset = 1)
  map = Dict{Int,Int}()
  for b in blocks(l.ir)
    map[l.bls[b.id]] = offset
    l′ = loop(b)
    offset += l′ == nothing ? 1 : nblocks(l′)
  end
  return map
end

function unloop!(ir::IR, l::LoopIR, _bs)
  for (iter, lir) in enumerate(l.body)
    bs = merge(blockmap(l, length(blocks(ir))+1), _bs)
    entry = length(blocks(ir))+1
    nextEntry = entry + nblocks(lir)
    bs[l.bls[1]] = iter == length(l.body) ? entry : nextEntry
    for b in blocks(lir)
      if (l′ = loop(b)) != nothing
        unloop!(ir, l′, bs)
      else
        c = block!(ir)
        copyblock!(c, b)
        for i = 1:length(branches(c))
          br = branches(c)[i]
          (isreturn(br) || isunreachable(br)) && continue
          branches(c)[i].args[1] = bs[br.args[1]]
        end
      end
    end
  end
  return ir
end

function unloop(l::LoopIR)
  ir = IR(meta = l.ir.meta)
  IRTools.deleteblock!(ir, 1)
  bs = Dict{Int,Int}()
  unloop!(ir, l, bs)
  return ir
end

function reroll!(ir::LoopIR)
  ir.max = 1
  length(ir.body) == 1 && return false
  changed = false
  first = ir.body[1]
  for ir in ir.body[2:end]
    changed |= blockargs!(first, IRTools.argtypes(ir))
  end
  resize!(ir.body, 1)
  return changed
end

# Navigation during inference

struct Path
  parts::Vector{Tuple{Int,Int}} # iter, block
end

Path() = Path([(1,1)])

tail(p::Path) = Path(p.parts[2:end])

(a::Path == b::Path) = (a.parts == b.parts)
hash(x::Path, h::UInt64) = hash(x.parts, h ⊻ 0x7fcfb6faea593cfb)
Base.isless(a::Path, b::Path) = isless(a.parts, b.parts)

@forward Path.parts Base.length, Base.lastindex, Base.getindex, Base.iterate

function IRTools.block(ir::LoopIR, path::Path)
  for (itr, b) in path[1:end-1]
    ir = loop(block(ir.body[itr], b))
  end
  itr, b = path[end]
  return block(ir.body[itr], b)
end

function nextpath(ir::LoopIR, p::Path)
  itr, bl = p[1]
  next = length(p) == 1 ? nothing :
    nextpath(loop(block(ir.body[itr], bl)), tail(p))
  next == nothing || return Path([p[1], next...])
  return (
    bl < length(ir.bls) ? Path([(itr, bl+1)]) :
    itr < length(ir.body) ? Path([(itr+1, 1)]) :
    nothing
  )
end

function nextpath(ir::LoopIR, p::Path, target::Int)
  q = Tuple{Int,Int}[]
  for (itr, b) in p
    c = findfirst(==(target), ir.bls)
    if isnothing(c) || c == b # back edge
      ir = loop(block(ir.body[itr], b))
      push!(q, (itr, b))
      continue
    end
    if c == 1
      if itr >= ir.max
        push!(q, (1, 1))
        return Path(q), reroll!(ir)
      else
        itr == length(ir.body) && push!(ir.body, copy(ir.ir))
        push!(q, (itr+1, 1))
        return Path(q), false
      end
    else
      push!(q, (itr, c))
      return Path(q), false
    end
  end
  error("Invalid block target $target")
end

# If a loop iteration breaks out, don't unroll it further.
function pin!(ir::LoopIR, p::Path, depth::Int)
  rr = false
  for (i, (itr, b)) in enumerate(p)
    if i > depth
      ir.max = itr
      if length(ir.body) > ir.max
        rr |= reroll!(ir)
        itr == 1 || return rr
      end
    end
    ir = loop(block(ir.body[itr], b))
  end
  return rr
end
