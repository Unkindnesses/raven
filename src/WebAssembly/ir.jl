using ..IRTools
using ..IRTools: IR, CFG, Variable, isexpr, stmt, argument!, return!, xcall, block!,
  branch!, blocks, insertafter!, arguments, argtypes, isreturn, isconditional
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

# TODO: simpler to treat all variables as potential tuples
# Or better, strip out tuples before register allocation
function locals!(ir::IR)
  locals = WType[]
  ret = WType[]
  env = Dict()
  tuples = Dict()
  rename(x::Variable) = env[x]
  rename(x::Real) = Const(x)
  rename(x::Union{Const,Local,GetGlobal}) = x
  ltype(x) = rename(x) isa Const ? WType(rename(x)) : locals[rename(x).id+1]
  local!(T) = (push!(locals, T); Local(length(locals)-1))
  local!(v, T) = get!(() -> local!(T), env, v)
  local!(v, T::WTuple) = tuples[v] = local!.(T.parts)
  for (arg, T) in zip(arguments(ir), argtypes(ir))
    local!(arg, T)
  end
  for b in blocks(ir)
    for (v, st) in b
      ex = st.expr
      src = st.src
      if ex isa Variable
        delete!(ir, v)
        _env = haskey(tuples, ex) ? tuples : env
        _env[v] = _env[ex]
      elseif ex == unreachable
        # leave it alone
      elseif !isexpr(ex)
        delete!(ir, v)
        env[v] = Const(ex)
      elseif isexpr(ex, :call)
        for arg in ex.args[2:end]
          haskey(tuples, arg) ?
            [insert!(ir, v, stmt(tuples[arg][i]; src))
             for i in 1:length(tuples[arg])] :
            insert!(ir, v, stmt(rename(arg); src))
        end
        ir[v] = ex.args[1]::Instruction
        if st.type isa WTuple
          tuples[v] = [(l = local!(T); insertafter!(ir, v, stmt(SetLocal(false, l.id); src)); l)
                       for T in st.type.parts]
        else
          insertafter!(ir, v, stmt(SetLocal(false, local!(v, st.type).id); src))
        end
      elseif isexpr(ex, :tuple)
        ps = []
        for arg in ex.args
          if haskey(tuples, arg)
            append!(ps, tuples[arg])
          else
            push!(ps, rename(arg))
          end
        end
        tuples[v] = ps
        delete!(ir, v)
      elseif isexpr(ex, :ref)
        env[v] = tuples[ex.args[1]][ex.args[2]]
        delete!(ir, v)
      elseif isexpr(ex, :branch)
        br = ex
        if isreturn(br)
          args = haskey(tuples, arguments(br)[1]) ? tuples[arguments(br)[1]] : arguments(br)
          ret = ltype.(args)
          for arg in args
            insert!(ir, v, stmt(rename(arg); src))
          end
          ir[v] = Return()
        elseif br == IRTools.unreachable
          ir[v] = unreachable
        else
          for (x, y) in zip(arguments(br), arguments(IRTools.block(ir, br.args[1])))
            if haskey(tuples, x)
              ls = get!(tuples, y) do
                [local!(T) for T in IRTools.exprtype(ir, y).parts]
              end
              for (xl, yl) in zip(tuples[x], ls)
                push!(b, stmt(xl; src))
                push!(b, stmt(SetLocal(false, yl.id); src))
              end
            else
              push!(b, stmt(rename(x); src))
              push!(b, stmt(SetLocal(false, local!(y, ltype(x)).id); src))
            end
          end
          isconditional(br) && push!(b, stmt(rename(br.args[2]); src))
          ir[v] = Branch(isconditional(br), br.args[1])
        end
      else
        error("Unrecognised wasm expression $ex")
      end
    end
  end
  return ir, locals, ret
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
  for i in length(cs.children):-1:1
    pushscope!(rl, Block([]), entries(cs.children[i])[1])
  end
  for i in 1:length(cs.children)
    # Pop forward jumps to this block
    popscope!(rl)
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
  ir, locals, ret = locals!(ir)
  params = flattentype(argtypes(ir))
  locals = locals[length(params)+1:end]
  Func(name, params => ret, locals, reloop(ir, cfg), ir.meta)
end
