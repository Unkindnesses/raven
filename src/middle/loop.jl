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
end

IRTools.argtypes(ir::LoopIR) = argtypes(ir.body[1])

function Base.show(io::IO, ir::LoopIR)
  indent = get(io, :indent, 0)
  io = IOContext(io, :indent => indent+1, :blockmap => i -> ir.bls[i])
  println(io, "loop:")
  for (i, b) in enumerate(ir.body)
    println(io, IRTools.tab^indent, "#$i:")
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

function looped(ir::IR, cs::Component = components(CFG(ir)))
  out = IR(meta = ir.meta)
  blocks = Int[]
  for (i, ch) in enumerate(cs.children)
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
  return LoopIR(out, blocks, [copy(out)])
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

# Unrolling eligibility

function merge_branchtypes!(a, b)
  for (k, v) in b
    a[k] = union.(get(a, k, [⊥ for _ in v]), v)
  end
  return a
end

function openbranches(bl)
  brs = []
  for br in IRTools.branches(bl)
    br.args[2] == nothing && (push!(brs, br); break)
    cond = exprtype(bl.ir, br.args[2])
    cond == false && continue
    cond == true && (push!(brs, br); break)
    push!(brs, br)
  end
  return brs
end

function exitBranches(l::LoopIR, b::Block)
  l′ = loop(b)
  l′ == nothing || error("unimplemented")
  brs = Dict()
  internal = l.bls[2:end]
  any(((v, st),) -> !isexpr(st, :branch) && st.type == ⊥, b) && return brs
  for br in openbranches(b)
    if !(br.args[1] in internal)
      merge_branchtypes!(brs, Dict(br.args[1] => exprtype(b.ir, arguments(br))))
    end
    br.args[1] in internal && br.args[1] > l.bls[b.id] &&
      merge_branchtypes!(brs, exitBranches(l, block(b.ir, findfirst(==(br.args[1]), l.bls))))
  end
  return brs
end

function exitBranches(l::LoopIR, itr::Int)
  ir = l.body[itr]
  exitBranches(l, block(ir, 1))
end

function reroll!(l::LoopIR)
  length(l.body) == 1 && return false
  first = l.body[1]
  for ir in l.body[2:end]
    blockargs!(first, IRTools.argtypes(ir))
  end
  resize!(l.body, 1)
  return true
end

function checkUnroll(l::LoopIR, itr)
  itr >= 8 && return false
  ir = l.body[itr]
  inputs = argtypes(ir)
  brs = exitBranches(l, itr)
  !haskey(brs, l.bls[1]) ||
    (length(brs) == 1 && !all(issubset.(inputs, brs[l.bls[1]])))
end

function checkExit(q, l::LoopIR, path)
  p′ = Tuple{Int,Int}[]
  for (itr, bl) in path.parts
    if length(l.body) > 1 && !checkUnroll(l, itr)
      reroll!(l)
      push!(p′, (1, 1))
      push!(q, Path(p′))
      break
    end
    push!(p′, (itr, bl))
    itr > length(l.body) && break
    l = loop(block(l.body[itr], bl))
  end
end

function nextItr!(l::LoopIR, itr)
  if checkUnroll(l, itr)
    itr += 1
    length(l.body) < itr && push!(l.body, copy(l.ir))
  else
    itr = 1
    reroll!(l)
  end
  return itr
end

# Navigation during inference

struct Path
  parts::Vector{Tuple{Int,Int}} # iter, block
end

Path() = Path([(1,1)])

(a::Path == b::Path) = (a.parts == b.parts)
hash(x::Path, h::UInt64) = hash(x.parts, h ⊻ 0x7fcfb6faea593cfb)

function IRTools.block(l::LoopIR, path::Path)
  for (itr, bl) in path.parts[1:end-1]
    itr > length(l.body) && return
    l = loop(block(l.body[itr], bl))
  end
  itr, bl = path.parts[end]
  itr > length(l.body) && return
  return block(l.body[itr], bl)
end

function nextpath(l::LoopIR, p::Path, target::Int)
  p′ = Tuple{Int,Int}[]
  for (i, (itr, bl)) in enumerate(p.parts)
    bl′ = findfirst(==(target), l.bls)
    if bl′ == bl # back edge
      push!(p′, (itr, bl))
      l = loop(block(l.body[itr], bl))
      (itr, _) = p.parts[i+1]
      itr′ = nextItr!(l, itr)
      reroll = itr > itr′ == 1
      push!(p′, (itr′, 1))
      return Path(p′), reroll
    elseif bl′ != nothing
      push!(p′, (itr, bl′))
      return Path(p′), false
    else
      l = loop(block(l.body[itr], bl))
      push!(p′, (itr, bl))
    end
  end
  error("Invalid block target $target")
end
