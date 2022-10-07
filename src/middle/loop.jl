using IRTools.Inner: CFG, Component, components, entries

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

struct LoopIR
  ir::IR
  bls::Vector{Int}
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
  return LoopIR(out, blocks)
end

function unloop!(ir::IR, l::LoopIR, bs)
  for b in blocks(l.ir)
    c = b.id == 1 ? last(blocks(ir)) : block!(ir)
    bs[l.bls[b.id]] = c.id
    if !isempty(b) && isexpr(first(b)[2].expr, :loop)
      unloop!(ir, first(b)[2].expr.args[1], bs)
    else
      copyblock!(c, b)
    end
  end
  return ir
end

function unloop(l::LoopIR)
  ir = IR()
  bs = Dict{Int,Int}()
  unloop!(ir, l, bs)
  for b in blocks(ir)
    for i = 1:length(branches(b))
      br = branches(b)[i]
      branches(b)[i] = IRTools.Branch(br, block = get(bs, br.block, br.block))
    end
  end
  return ir
end
