# Reference Counting
#
# The unoptimised approach is:
# * Insert a `retain` whenever we pass a live variable as an argument.
# * Insert a `release` whenever a variable is dropped.
#
# Passing a variable as an argument means code like:
#
# ```
# x = ...
# y = f(x)
# ```
#
# Here `x` only needs to be retained if it is used again later on. If not,
# `f(x)` will free it. (Branch arguments are treated the same way.)
#
# Return values aren't dropped, so we don't need to `release` them before
# returning. The caller will take care of them. If `f(x) = x`, the code above
# doesn't actually need any counting instructions to be emitted. (If `f(x)`
# drops `x`, eg `f(x) = nothing`, we do need to release it before returning.)
#
# A variable can be dropped immediately (if it is never used) or after a branch
# (because the variable is not used in the current path). An edge case is when a
# block has multiple predecessors, one of which has multiple successors, such
# that the block only sometimes releases a preceding variable. For now this case
# is an error.

isrefobj(x::Pack) = tag(x) == tag"common.Ref"

isreftype(x) = false
isreftype(xs::Onion) = any(isreftype, xs.types)
isreftype(xs::Pack) = isrefobj(xs) || any(isreftype, xs.parts)
isreftype(x::VPack) = layout(x.parts) != ()
isreftype(x::Recursive) = true

isglobal(ir, v) = haskey(ir, v) && isexpr(ir[v], :global)

# Define refcount methods for different types
# ===========================================

@enum CountMode retain release

# Used as a key for generated methods
retain_method = RMethod(tag"common.core.retain", lowerpattern(rvx"args"), nothing, false)
release_method = RMethod(tag"common.core.release", lowerpattern(rvx"args"), nothing, false)

count!(ir, T, x, mode) =
  T isa Pack && !isrefobj(T) ? count_inline!(ir, T, x, mode) :
  push!(ir, stmt(xcall(mode == retain ? retain_method : release_method, x), type = nil))

retain!(ir, T, x) = count!(ir, T, x, retain)
release!(ir, T, x) = count!(ir, T, x, release)

function rcall!(ir, f, args...; type)
  args = [arg isa Number ? push!(ir, stmt(arg, type = typeof(arg))) : arg for arg in args]
  args = push!(ir, stmt(xtuple(args...), type = rlist(exprtype(ir, args)...)))
  push!(ir, stmt(xcall(f, args); type))
end

function countptr!(ir, ptr, mode)
  f = mode == retain ? tag"common.retain!" : tag"common.release!"
  @assert tag(exprtype(ir, ptr)) == tag"common.Ptr"
  rcall!(ir, f, ptr, type = nil)
end

function count_inline!(ir, T::Pack, x, mode)
  if isrefobj(T)
    P = partial_part(T, 1)
    ptr = indexer!(ir, T, 1, x, nothing)
    ptr = push!(ir, stmt(Expr(:tuple, ptr), type = rlist(P)))
    if mode == release
      cleanup = push!(ir, stmt(xcall(tag"common.i32load", ptr), type = Int32))
      args = push!(ir, stmt(Expr(:tuple, ptr, cleanup), type = rlist(P, Int32)))
      push!(ir, stmt(xcall(tag"common.release!", args), type = nil))
    else
      push!(ir, stmt(xcall(tag"common.retain!", ptr), type = nil))
    end
  else
    for i = 0:nparts(T)
      isreftype(part(T, i)) || continue
      p = indexer!(ir, T, i, x, nothing)
      count!(ir, part(T, i), p, mode)
    end
  end
end

function count_inline!(ir, T::VPack, x, mode)
  isreftype(T) || return
  len = push!(ir, stmt(Expr(:ref, x, 1), type = Int32))
  ptr = push!(ir, stmt(Expr(:ref, x, 2), type = pack(tag"common.Ptr", Int32)))
  pos = ptr
  if mode == release && isreftype(T.parts)
    test = blocks(ir)[end]
    header = block!(ir)
    body = block!(ir)
    after = block!(ir)

    unique = rcall!(test, tag"common.blockUnique", ptr, type = Int32)
    branch!(test, header, len, ptr, when = unique)
    branch!(test, after)

    len = argument!(header, type = Int32, insert = false)
    pos = argument!(header, type = pack(tag"common.Ptr", Int32), insert = false)
    done = rcall!(header, tag"common.==", len, Int32(0), type = Int32)
    branch!(header, after, when = done)
    branch!(header, body)

    el = load(body, T.parts, pos, count = false)
    release!(body, T.parts, el)
    len = rcall!(body, tag"common.-", len, Int32(1), type = Int32)
    pos = rcall!(body, tag"common.+", pos, Int32(sizeof(T.parts)), type = pack(tag"common.Ptr", Int32))
    branch!(body, header, len, pos)
  end
  countptr!(ir, ptr, mode)
end

function count_inline!(ir, T::Onion, x, mode)
  union_cases!(ir, T, x) do T, x
    if isreftype(T)
      count!(ir, T, x, mode)
    end
    push!(ir, stmt(Expr(:tuple), type = nil))
  end
end

function count_inline!(ir, T::Recursive, x, mode)
  ptr = push!(ir, stmt(Expr(:ref, x, 1), type = pack(tag"common.Ptr", Int32)))
  if mode == release
    unique = rcall!(ir, tag"common.blockUnique", ptr, type = Int32)
    branch!(ir, length(blocks(ir))+1, when = unique)
    branch!(ir, length(blocks(ir))+2)
    block!(ir)
    T = unroll(T)
    inner = unbox!(ir, T, x, count = false)
    release!(ir, T, inner)
    branch!(ir, length(blocks(ir))+1)
    block!(ir)
  end
  countptr!(ir, ptr, mode)
end

function count_ir(T, mode)
  ir = IR(meta = FuncInfo(Tag(tag"common.core", Symbol(mode))))
  x = argument!(ir, type = T)
  count_inline!(ir, T, x, mode)
  return!(ir, push!(ir, stmt(xtuple(), type = nil)))
end

# Insert counting instructions
# ============================

function refcounts(ir)
  lv = liveness(ir)
  pr = IRTools.Pipe(ir)
  for bl in blocks(pr)
    rel(x) = (T = exprtype(ir, x); isreftype(T) && release!(pr, T, x))
    # unused block arguments
    foreach(rel, filter(x -> !(x in lv[bl.id]), arguments(block(ir, bl.id))))
    # conditionally dropped variables
    dropped = [filter(x -> isreftype(exprtype(ir, x)), setdiff(liveness_after(c, lv), lv[bl.id])) for c in predecessors(block(ir, bl.id))]
    if !isempty(dropped)
      @assert all(xs -> xs == dropped[1], dropped) # condition mentioned above
      foreach(rel, dropped[1])
    end
    for (v, st) in bl
      if isexpr(st, :release)
        delete!(pr, v)
        x = st.expr.args[1]
        T = exprtype(ir, x)
        isreftype(T) && !(x in lv[v]) || continue
        release!(pr, T, x)
      elseif isexpr(st, :retain)
        delete!(pr, v)
        x = st.expr.args[1]
        T = exprtype(ir, x)
        isreftype(T) && x in lv[v] || continue
        retain!(pr, T, x)
      elseif isexpr(st, :call, :tuple, :branch)
        live = isexpr(st, :branch) ? liveness_after(block(ir, v), lv) : lv[v]
        delete!(pr, v)
        # reused argument
        for x in unique(st.expr.args)
          isnothing(x) && continue # branch condition
          T = exprtype(ir, x)
          isreftype(T) || continue
          ret = isglobal(ir, x) + count(==(x), st.expr.args) - !(x in live)
          for _ = 1:ret
            retain!(pr, T, x)
          end
        end
        v′ = push!(pr, st)
        replace!(pr, v, v′)
        # dropped variable
        isreftype(st.type) && (v in lv[v] || release!(pr, st.type, v′))
      end
    end
  end
  ir = IRTools.finish(pr)
  return ir
end

function aliases(ir)
  aliases = Dict{Variable,Vector{Union{Primitive,Tuple{Variable,Int}}}}()
  alias(v::Variable) = get!(() -> [(v, i) for i = 1:nregisters(layout(exprtype(ir, v)))], aliases, v)
  alias(x::Primitive) = [x]
  for (v, st) in ir
    if isexpr(st, :tuple)
      aliases[v] = vcat(alias.(st.expr.args)...)
    elseif isexpr(st, :ref) && st.expr.args[1] isa Variable
      aliases[v] = [alias(st.expr.args[1])[st.expr.args[2]]]
    end
  end
  return aliases
end

# ...and take them away again
function elide_counts!(ir)
  vs = aliases(ir)
  for bl in blocks(ir)
    retains = Dict{Vector{Tuple{Variable,Int}},Vector{Variable}}()
    for (v, st) in bl
      isexpr(st, :call) || continue
      if ismethod(st.expr.args[1], tag"common.core.retain")
        push!(get!(retains, vs[st.expr.args[2]], []), v)
      elseif ismethod(st.expr.args[1], tag"common.core.release")
        haskey(vs, st.expr.args[2]) || continue
        rets = get!(retains, vs[st.expr.args[2]], [])
        if !isempty(rets)
          delete!(ir, pop!(rets))
          delete!(ir, v)
        end
      end
    end
  end
  return ir
end

function refcounts(c::CycleCache)
  Cache{Any,Union{Redirect,IR}}() do self, sig
    if sig[1] in (retain_method, release_method)
      count_ir(sig[2], sig[1] == retain_method ? retain : release)
    else
      ir = c[sig]
      ir isa Redirect ? ir : elide_counts!(refcounts(ir))
    end
  end
end
