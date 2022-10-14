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

nblocks(cs::Integer) = 1
nblocks(cs::Component) = sum(nblocks, cs.children)

mutable struct LoopIR
  ir::IR
  bls::Vector{Int}
  nblocks::Int
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
  return LoopIR(out, blocks, nblocks(cs))
end

function blockmap(l::LoopIR, offset = 1)
  map = Dict{Int,Int}()
  for b in blocks(l.ir)
    map[l.bls[b.id]] = offset
    l′ = loop(b)
    offset += l′ == nothing ? 1 : l′.nblocks
  end
  return map
end

function unloop!(ir::IR, l::LoopIR, bs)
  bs = merge(blockmap(l, length(blocks(ir))), bs)
  for b in blocks(l.ir)
    c = b.id == 1 ? last(blocks(ir)) : block!(ir)
    if (l′ = loop(b)) != nothing
      unloop!(ir, l′, bs)
    else
      copyblock!(c, b)
      for i = 1:length(branches(c))
        br = branches(c)[i]
        isreturn(br) && continue
        branches(c)[i] = IRTools.Branch(br, block = bs[br.block])
      end
    end
  end
  return ir
end

function unloop(l::LoopIR)
  ir = IR()
  bs = Dict{Int,Int}()
  unloop!(ir, l, bs)
  return ir
end
