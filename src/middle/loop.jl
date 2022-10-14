using IRTools.Inner: CFG, Block, Component, components, entries

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
  for br in IRTools.branches(b)
    push!(branches(c), rename(env, br))
  end
  return c
end

mutable struct LoopIR
  ir::IR
  bls::Vector{Int}
  body::Vector{IR}
end

function loop(bl::Block)
  if !isempty(bl) && isexpr(first(bl)[2].expr, :loop)
    return first(bl)[2].expr.args[1]
  else
    return nothing
  end
end

function looped(ir::IR, cs::Component = components(CFG(ir)))
  out = IR()
  blocks = Int[]
  for (i, ch) in enumerate(cs.children)
    bl = i == 1 ? block(out, 1) : block!(out)
    if ch isa Integer
      push!(blocks, ch)
      copyblock!(bl, block(ir, ch))
    else
      es = entries(ch)
      @assert length(es) == 1
      push!(blocks, es[1])
      args = [argument!(bl, insert = false) for _ in arguments(block(ir, es[1]))]
      push!(bl, Expr(:loop, looped(ir, ch), args...))
    end
  end
  return LoopIR(out, blocks, IR[copy(out)])
end

function unrollall!(ir::IR, n)
  for b in blocks(ir)
    l = loop(b)
    l == nothing && continue
    unrollall!(l, n)
    for _ = 1:n
      push!(l.body, copy(l.ir))
    end
  end
end

function unrollall!(l::LoopIR, n = 1)
  unrollall!(l.ir, n)
  return l
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
    for b in blocks(l.ir)
      if (l′ = loop(b)) != nothing
        unloop!(ir, l′, bs)
      else
        c = block!(ir)
        copyblock!(c, b)
        for i = 1:length(branches(c))
          br = branches(c)[i]
          isreturn(br) && continue
          branches(c)[i] = IRTools.Branch(br, block = bs[br.block])
        end
      end
    end
  end
  return ir
end

function unloop(l::LoopIR)
  ir = IR()
  IRTools.deleteblock!(ir, 1)
  bs = Dict{Int,Int}()
  unloop!(ir, l, bs)
  return ir
end
