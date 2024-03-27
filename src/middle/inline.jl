function opcount(ir::IR)
  count = 0
  for (v, st) in ir
    if isexpr(st, :tuple, :ref, :branch, :cast, :retain, :release)
    elseif isexpr(st, :global, :(=))
      count += (nregisters(layout(st.type)) > 0)
    elseif isexpr(st, :call, :func, :call_indirect)
      count += 1
    elseif isexpr(st)
      error("unrecognised expr $(st.expr.head)")
    end
  end
  return count
end

function inlineable(ir::IR)
  length(blocks(ir)) == 1 && opcount(ir) <= 3
end

function inlineable(cache, T)
  T[1] == tag"common.hold" && return false
  fr = Caches.get(cache, T) # avoid self dependency in cycles
  fr == nothing && return false # hit a cycle
  fr isa Redirect && return inlineable(cache, cache[T].to)
  inlineable(cache[T])
end

function inline(ir::IR, inlined)
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    isexpr(st, :call) && !(st.expr.args[1] isa WIntrinsic) || continue
    T = (exprtype(ir, st.expr.args)...,)
    inlineable(inlined, T) || continue
    delete!(pr, v)
    v′ = IRTools.inlinehere!(pr, inlined[T], st.expr.args[2:end]...)
    replace!(pr, v, v′)
  end
  return IRTools.finish(pr)
end

function Inlined(cache)
  Cache{Any,Union{IR,Redirect,Nothing}}() do self, sig
    ir = cache[sig]
    options().inline || return ir
    ir isa Redirect && return ir
    Caches.set!(self, sig, nothing) # TODO Cache itself could handle cycles
    inline(ir, self)
  end
end
