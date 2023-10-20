using ..IRTools
using ..IRTools: IR, CFG, Variable, isexpr, stmt, argument!, return!, xcall, block!,
  branch!, blocks, arguments, argtypes, isreturn, isconditional
using ..IRTools: Component, components, entries, successors

struct WTuple
  parts::Vector{WType}
end

WTuple(Ts::WType...) = WTuple([Ts...])

function Base.show(io::IO, t::WTuple)
  print(io, "(")
  join(io, t.parts, ", ")
  print(io, ")")
end

function locals(ir::IR)
  locals = WType[]
  ret = WType[]
  env = Dict()
  parts(x::Variable) = env[x]
  parts(x::Real) = [Const(x)]
  local!(T) = (push!(locals, T); Local(length(locals)-1))
  local!(v, T) = get!(() -> [local!(T)], env, v)
  local!(v, T::WTuple) = get!(() -> local!.(T.parts), env, v)
  for (arg, T) in zip(arguments(ir), argtypes(ir))
    local!(arg, T)
  end
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    ex = st.expr
    src = st.src
    if ex isa Variable
      delete!(pr, v)
      env[v] = env[ex]
    elseif ex == unreachable
      # leave it alone
    elseif !isexpr(ex)
      delete!(pr, v)
      env[v] = [Const(ex)]
    elseif isexpr(ex, :call)
      for arg in ex.args[2:end], i in 1:length(parts(arg))
        insert!(pr, v, stmt(parts(arg)[i]; src))
      end
      pr[v] = ex.args[1]::Instruction
      ls = local!(v, st.type)
      foreach(l -> push!(pr, stmt(SetLocal(false, l.id); src)), reverse(ls))
    elseif isexpr(ex, :tuple)
      env[v] = vcat([parts(x) for x in ex.args]...)
      delete!(pr, v)
    elseif isexpr(ex, :ref)
      env[v] = [parts(ex.args[1])[ex.args[2]]]
      delete!(pr, v)
    elseif isexpr(ex, :branch)
      if isreturn(ex)
        x = arguments(ex)[1]
        ret = [p isa Const ? WType(p) : locals[p.id+1] for p in parts(x)]
        for arg in parts(x)
          insert!(pr, v, stmt(arg; src))
        end
        pr[v] = Return()
      elseif ex == IRTools.unreachable
        pr[v] = unreachable
      else
        for (x, y) in zip(arguments(ex), arguments(IRTools.block(ir, ex.args[1])))
          ls = local!(y, IRTools.exprtype(ir, y))
          for (xl, yl) in zip(parts(x), ls)
            push!(pr, stmt(xl; src))
            push!(pr, stmt(SetLocal(false, yl.id); src))
          end
        end
        isconditional(ex) && push!(pr, stmt(only(parts(ex.args[2])); src))
        pr[v] = Branch(isconditional(ex), ex.args[1])
      end
    else
      error("Unrecognised wasm expression $ex")
    end
  end
  ir = IRTools.finish(pr)
  foreach(bl -> IRTools.emptyargs!(bl), blocks(ir))
  return ir, locals, ret
end

# When stepping over, debuggers will go to either the next line or the next
# is_stmt, whichever comes first. To avoid stepping to each call twice, we
# find contiguous sequences of instrs with the same source location, and move
# any breakpoint to the top of the sequence.
function shiftbps!(ir)
  for bl in IRTools.blocks(ir)
    ip, src = nothing, nothing
    for (v, st) in bl
      if st.src != src
        ip, src = v, st.src
      elseif src != nothing && st.bp
        ir[ip] = stmt(ir[ip], bp = true)
        ir[v]  = stmt(ir[v],  bp = false)
      end
    end
  end
  return ir
end

struct Relooping
  ir::IR
  cfg::CFG
  scopes::Vector{Any}
  targets::Vector{Int}
end

function pushscope!(rl::Relooping, bl, target)
  instr!(rl.scopes[end], bl)
  push!(rl.scopes, bl)
  push!(rl.targets, target)
  return rl
end

function popscope!(rl::Relooping)
  pop!(rl.scopes)
  pop!(rl.targets)
  return
end

lineinfo(st) = st.src == nothing ? nothing : LineInfo(st.src, st.bp)

function reloop!(rl::Relooping, i::Integer)
  b = blocks(rl.ir)[i]
  for (v, st) in b
    if st.expr isa Branch
      target = findfirst(b -> b == st.expr.level, reverse(rl.targets))-1
      instr!(rl.scopes[end], Branch(st.expr.cond, target), lineinfo(st))
    else
      instr!(rl.scopes[end], st.expr, lineinfo(st))
    end
  end
end

function reloop!(rl::Relooping, cs::IRTools.Component)
  # Insert blocks for forward jumps
  for i in length(cs.children):-1:2
    pushscope!(rl, Block([]), entries(cs.children[i])[1])
  end
  for i in 1:length(cs.children)
    # Pop forward jumps to this block
    i == 1 || popscope!(rl)
    cs.children[i] isa Component && pushscope!(rl, Loop([]), entries(cs.children[i])[1])
    # Block body
    reloop!(rl, cs.children[i])
    cs.children[i] isa Component && popscope!(rl)
  end
end

function reloop(ir, cfg)
  rl = Relooping(ir, cfg, Any[Block([],[])], [])
  reloop!(rl, components(cfg))
  @assert length(rl.scopes) == 1
  @assert isempty(rl.targets)
  return rl.scopes[1]
end

flattentype(Ts) = vcat(flattentype.(Ts)...)
flattentype(T::WType) = [T]
flattentype(T::WTuple) = T.parts

function irfunc(name, ir)
  cfg = CFG(ir)
  ir, ls, ret = locals(ir)
  ir = shiftbps!(ir)
  params = flattentype(argtypes(ir))
  ls = ls[length(params)+1:end]
  Func(name, params => ret, ls, reloop(ir, cfg), ir.meta)
end
