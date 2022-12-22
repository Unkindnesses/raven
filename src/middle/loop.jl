using .IRTools: CFG, Block, Component, components, entries

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
end

IRTools.argtypes(ir::LoopIR) = argtypes(ir.body[1])

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
      args = [argument!(bl, type = T, insert = false) for T in argtypes(block(ir, es[1]))]
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

function reroll!(l::LoopIR)
  # TODO: combine entry block types and re-infer
  @assert length(l.body) == 1
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
          isreturn(br) && continue
          branches(c)[i].args[1] = bs[br.args[1]]
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

# Unrolling eligibility

function merge_branchtypes!(a, b)
  for (k, v) in b
    a[k] = union.(get(a, k, [⊥ for _ in v]), v)
  end
  return a
end

function exitBranches(inf, l::LoopIR, b::Block)
  l′ = loop(b)
  l′ == nothing || error("unimplemented")
  brs = Dict()
  internal = l.bls[2:end]
  any(((v, st),) -> !isexpr(st, :branch) && st.type == ⊥, b) && return brs
  for br in openbranches(inf.mod, b)
    if !(br.args[1] in internal)
      merge_branchtypes!(brs, Dict(br.args[1] => exprtype(inf.mod, b.ir, arguments(br))))
    end
    br.args[1] in internal && br.args[1] > l.bls[b.id] &&
      merge_branchtypes!(brs, exitBranches(inf, l, block(b.ir, findfirst(==(br.args[1]), l.bls))))
  end
  return brs
end

function exitBranches(inf, l::LoopIR, itr::Int)
  ir = l.body[itr]
  exitBranches(inf, l, block(ir, 1))
end

function checkUnroll(inf, l::LoopIR, itr)
  ir = l.body[itr]
  inputs = argtypes(ir)
  brs = exitBranches(inf, l, itr)
  !haskey(brs, l.bls[1]) ||
    (length(brs) == 1 && !all(issubset.(inputs, brs[l.bls[1]])))
end

function checkExit(inf, l::LoopIR, path)
  for (itr, bl) in path.parts
    if length(l.body) > 1 && !checkUnroll(inf, l, itr)
      reroll!(l)
      break
    end
    l = loop(block(l.body[itr], bl))
  end
end

function nextItr!(inf, l::LoopIR, itr)
  if checkUnroll(inf, l, itr)
    itr += 1
    length(l.body) < itr && push!(l.body, copy(l.ir))
  else
    reroll!(l)
  end
  return itr
end

# Navigation during inference

struct Path
  parts::Vector{Tuple{Int,Int}} # iter, block
end

Path() = Path([(1,1)])

function IRTools.block(l::LoopIR, path::Path)
  for (itr, bl) in path.parts[1:end-1]
    l = loop(block(l.body[itr], bl))
  end
  itr, bl = path.parts[end]
  return block(l.body[itr], bl)
end

function nextpath(inf, l::LoopIR, p::Path, target::Int)
  p′ = Tuple{Int,Int}[]
  for (i, (itr, bl)) in enumerate(p.parts)
    bl′ = findfirst(==(target), l.bls)
    if bl′ == bl # back edge
      push!(p′, (itr, bl))
      l = loop(block(l.body[itr], bl))
      (itr, _) = p.parts[i+1]
      itr = nextItr!(inf, l, itr)
      push!(p′, (itr, 1))
      return Path(p′)
    elseif bl′ != nothing
      push!(p′, (itr, bl′))
      return Path(p′)
    else
      l = loop(block(l.body[itr], bl))
      push!(p′, (itr, bl))
    end
  end
  error("Invalid block target $target")
end
