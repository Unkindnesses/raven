# After type inference, we lower to a form where reference and union types are
# represented with explicit pointers and tags, eg a `VData` becomes a (size,
# pointer) tuple. This allows us to compile methods like `part`, `datacat` and
# casts, whose implementation needs to access those internals.
#
# It makes sense for those methods to be represented as functions, to avoid
# code duplication and so that casting can be recursive. And they can still
# participate in optimisations (mainly inlining).
#
# After this lowering all code works with primitive values and does explicit
# memory management, so the job of the backend code generator is simple.

# TODO: check for specific methods here, not just a method of the right name.
ismethod(m, name) = m isa RMethod && m.name == name

struct Compilation
  mod::RModule
  frames::IdDict{Any,IR}
end

Compilation(mod::RModule) = Compilation(mod, IdDict{Any,IR}())

# Data primitives

function lowerdata(mod, ir)
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    if isexpr(st.expr, :data)
      # remove constants, which have zero width
      # TODO: better to do this based on type, even though it doesn't come up
      # yet
      args = filter(x -> x isa Union{Variable,Global}, st.expr.args)
      pr[v] = Expr(:tuple, args...)
    elseif isexpr(st.expr, :call)
      st.expr.args[1] isa WIntrinsic && continue
      F = exprtype(mod, ir, st.expr.args[1])
      if ismethod(F, :widen)
        T = exprtype(mod, ir, st.expr.args[2])
        val = T isa Integer ? T : st.expr.args[2]
        pr[v] = val
      elseif ismethod(F, :data)
        # Arguments are turned into a tuple when calling any function, so this
        # is just a cast.
        pr[v] = st.expr.args[2]
      elseif ismethod(F, :datacat)
        pr[v] = st.expr.args[2]
      elseif ismethod(F, :nparts)
        pr[v] = nparts(exprtype(mod, ir, st.expr.args[2]))
      end
    end
  end
  return IRTools.finish(pr)
end

# Casts

blockargtype(mod::RModule, bl, i) = exprtype(mod, bl.ir, arguments(bl)[i])

function isreachable(bl)
  for (v, st) in bl
    st.type == ⊥ && return false
  end
  return true
end

function cast!(ir, from, to)
  if from isa Number && to == typeof(from)
    from
  elseif from == rtuple() && to isa VData
    margs = push!(ir, IRTools.stmt(Expr(:tuple, Int32(0)), type = rtuple(Int32)))
    ptr = push!(ir, IRTools.stmt(xcall(Global(:malloc), margs), type = Int32))
    push!(ir, IRTools.stmt(Expr(:tuple, Int32(0), ptr), type = to))
  else
    error("unsupported cast: $from -> $to")
  end
end

function casts!(mod::RModule, ir)
  for bl in blocks(ir)
    if !isreachable(bl)
      empty!(branches(bl))
      continue
    end
    for br in branches(bl)
      isreturn(br) && continue # TODO: handle multiple returns
      for i = 1:length(arguments(br))
        S = exprtype(mod, ir, arguments(br)[i])
        T = blockargtype(mod, block(ir, br.block), i)
        S == T && continue
        arguments(br)[i] = cast!(bl, S, T)
      end
    end
  end
  return ir
end

function lowerir(mod, ir)
  # Inference expands block args, so prune them here
  casts!(mod, prune!(lowerdata(mod, ir)))
end

function lowerir(inf::Inference)
  comp = Compilation(inf.mod)
  for (k, fr) in inf.frames
    comp.frames[k] = lowerir(inf.mod, fr.ir)
  end
  return comp
end
