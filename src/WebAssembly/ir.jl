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

flattype(Ts) = vcat(flattype.(Ts)...)
flattype(T::WType) = [T]
flattype(T::WTuple) = T.parts

function stack(ir::IR)
  ret = WType[]
  env = Dict()
  parts(x::Variable) = env[x]
  parts(x::Real) = [Const(x)]
  parts(xs::AbstractVector) = [p for x in xs for p in parts(x)]
  parts!(x, T) = (env[x] = [(x, i) for (i, _) in enumerate(flattype(T))])
  lv = IRTools.liveness(ir)
  live(v) = Set([p for x in lv[v] for p in parts(x)])
  for bl in blocks(ir), (x, T) in zip(arguments(bl), argtypes(bl))
    parts!(x, T)
  end
  pr = IRTools.Pipe(ir)
  for bl in blocks(pr)
    stack = []
    for (v, st) in bl
      ex = st.expr
      src = st.src
      if ex isa Variable
        env[v] = env[ex]
        delete!(pr, v)
      elseif isexpr(st, :tuple)
        env[v] = vcat([parts(x) for x in ex.args]...)
        delete!(pr, v)
      elseif isexpr(st, :ref)
        env[v] = [parts(ex.args[1])[ex.args[2]]]
        delete!(pr, v)
      elseif ex isa Number
        env[v] = [Const(ex)]
        delete!(pr, v)
      elseif isexpr(st, :call)
        parts!(v, st.type)
        args = parts(ex.args[2:end])
        ops, state = stackshuffle(Locals(stack), Locals(args, intersect(live(v), stack)))
        foreach(op -> insert!(pr, v, stmt(op; src)), ops)
        pr[v] = ex.args[1]::Instruction
        stack = vcat(state.stack[1:end-length(args)], parts(v))
      elseif isexpr(st, :branch)
        if isreturn(ex)
          result = only(arguments(ex))
          parttype(x::Const) = WType(x)
          parttype((x, i)::Tuple) = flattype(IRTools.exprtype(ir, x))[i]
          ret = parttype.(parts(result))
          ops, _ = stackshuffle(Locals(stack), Locals(parts(result)))
          foreach(op -> insert!(pr, v, stmt(op; src)), ops)
          pr[v] = Return()
          stack = []
        elseif ex == IRTools.unreachable
          pr[v] = unreachable
        else
          args = parts(arguments(ex))
          # TODO in this case the args could be in any order
          ops, state = stackshuffle(Locals(stack), Locals(args, intersect(live(v), stack)))
          foreach(op -> insert!(pr, v, stmt(op; src)), ops)
          args = reverse(parts(arguments(IRTools.block(ir, ex.args[1]))))
          for x in args
            insert!(pr, v, stmt(Expr(:set, x); src))
          end
          stack = state.stack[1:end-length(args)]
          args = isconditional(ex) ? parts(ex.args[2]) : []
          # We are allowed to leave dead values on the stack when branching, but
          # anything live must be stored in a local.
          ops, state = stackshuffle(Locals(stack), Locals(args, intersect(live(v), stack)), store = true)
          foreach(op -> insert!(pr, v, stmt(op; src)), ops)
          pr[v] = Branch(isconditional(ex), ex.args[1])
          stack = state.stack[1:end-length(args)]
        end
      else
        error("Unrecognised expression $(ex)")
      end
    end
  end
  ir = IRTools.finish(pr)
  for (v, st) in ir
    isexpr(st, :get, :set, :tee) || continue
    (x, i) = only(st.expr.args)
    ir[v] = Expr(st.expr.head, (IRTools.substitute(pr, x), i))
  end
  return ir, ret
end

# TODO new liveness / interference analysis so we can reuse slots.
# Will also have to filter redundant moves (due to block args) in that case.
function locals!(ir::IR)
  locals = WType[]
  slots = Dict{Tuple{Variable,Int},Int}()
  for (x, T) in zip(arguments(ir), argtypes(ir)), (i, t) in enumerate(flattype(T))
    slots[(x, i)] = length(locals)
    push!(locals, t)
  end
  slot(v, i) = get!(() -> length(push!(locals, flattype(IRTools.exprtype(ir, v))[i]))-1, slots, (v, i))
  for (v, st) in ir
    isexpr(st, :get, :set, :tee) || continue
    (x, i) = only(st.expr.args)
    s = slot(x, i)
    ir[v] = st.expr.head == :get ? Local(s) : SetLocal(st.expr.head == :tee, s)
  end
  foreach(bl -> empty!(arguments(bl)), blocks(ir))
  return ir, locals
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

function irfunc(name, ir)
  cfg = CFG(ir)
  params = flattype(argtypes(ir))
  ir, ret = stack(ir)
  ir, ls = locals!(ir)
  ir = shiftbps!(ir)
  ls = ls[length(params)+1:end]
  Func(name, params => ret, ls, reloop(ir, cfg), ir.meta)
end
