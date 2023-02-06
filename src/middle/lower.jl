# After type inference, we lower to a form where reference and union types are
# represented with explicit pointers and tags, eg a `VPack` becomes a (size,
# pointer) tuple. This allows us to compile methods like `part`, `packcat` and
# casts, whose implementation needs to access those internals.
#
# It makes sense for those methods to be represented as functions, to avoid
# code duplication and so that casting can be recursive. And they can still
# participate in optimisations (mainly inlining).
#
# After this lowering all code works with primitive values (or flat tuples of
# primitives) and does explicit memory management, so the job of the backend
# code generator is simple.
#
# We also insert primitive retain/release instructions here. The RC algorithm
# will only release just after a variable is created, and then only if there are
# no uses. The `release` expression tells it to try again, and is ignored if the
# variable is used later; it should be used liberally by any code that works
# with internals. `retain` is mainly used by indexing.

function sig(inf::Cache, T)
  fr = inf[T]
  fr isa Redirect ? sig(inf, fr.to) : T
end

frame(inf::Cache, T) = inf[sig(inf, T)]

# Global variables

# Turn global references into explicit load instructions
function globals(ir::IR)
  pr = IRTools.Pipe(ir)
  function transform(x, v)
    x isa Global || return x
    if x.type == ⊥
      insert!(pr, v, stmt(xcall(WIntrinsic(WebAssembly.Call(:panic), ⊥),
                          Expr(:ref, "$(x.name) is not defined")), type = ⊥))
    else
      insert!(pr, v, stmt(Expr(:global, x.name), type = x.type))
    end
  end
  for (v, st) in pr
    ex = st.expr
    if isexpr(ex, :(=))
      pr[v] = Expr(ex.head, ex.args[1], transform.(ex.args[2:end], (v,))...)
    elseif isexpr(ex)
      pr[v] = Expr(ex.head, transform.(ex.args, (v,))...)
    end
  end
  return IRTools.finish(pr)
end

# Pack primitives

function cat_layout(xs...)
  result = ()
  for x in xs
    x isa WTuple ? append!(result, x.parts) : push!(result, x)
  end
  return WTuple(result)
end

cat_layout() = ()
cat_layout(x) = (x,)
cat_layout(x::Tuple) = x
cat_layout(x, xs...) = (cat_layout(x)..., cat_layout(xs...)...)

layout(T::Type{<:Primitive}) = T
layout(::Type{String}) = layout(pack(:String, pack(:JSObject, Int32)))
layout(x::Union{Primitive,AST.Quote,Unreachable}) = ()
layout(x::Pack) = cat_layout(layout.(x.parts)...)
layout(x::VPack) = (Int32, Int32) # size, pointer
layout(x::Recursive) = (Int32,)
layout(x::Recur) = Int32
layout(xs::Or) = (Int32, cat_layout(layout.(xs.patterns)...)...)

nregisters(l::Type) = 1
nregisters(l::Tuple) = length(l)

function sublayout(T, i)
  before = pack(T.parts[1:i]...)
  offset = nregisters(layout(before))
  length = nregisters(layout(T.parts[i+1]))
  offset .+ (1:length)
end

inlinePrimitive[pack_method] = function (pr, ir, v)
  # Arguments are turned into a tuple when calling any function, so this
  # is just a cast.
  @assert layout(ir[v].type) == layout(exprtype(ir, ir[v].expr.args[2]))
  pr[v] = ir[v].expr.args[2]
end

# part
# ====

# Create a `part` method to dynamically index tuples allocated as registers.
# TODO: should make sure this comes out as a switch / branch table.
# TODO: we call WASM functions here, which is not backend-agnostic, but
# the functions we need might not have been inferred. Once the compiler is
# incremental, later stages can request work from earlier ones.
function partir(x, i)
  i <: Int64 || error("Only Int64 indexes are supported.")
  T = partial_part(x, i)
  ir = IR(meta = FuncInfo(:part))
  vx = argument!(ir, type = x)
  vi = argument!(ir, type = i)
  xlayout = layout(x)
  part(i) = xlayout isa Tuple ? push!(ir, Expr(:ref, vx, i)) : vx
  for i = 1:nparts(x)
    cond = push!(ir, stmt(xcall(WIntrinsic(i64.eq, i32), i, vi), type = Int32))
    branch!(ir, length(ir.blocks) + 1, when = cond)
    branch!(ir, length(ir.blocks) + 2)
    block!(ir)
    # TODO: recurse to `indexer` here, let casting happen later
    range = sublayout(x, i)
    T′ = partial_part(x, i)
    ex = layout(T′) isa Tuple ?
      Expr(:tuple, part.(range)...) :
      part(range[1])
    y = push!(ir, stmt(ex, type = T′))
    if T′ != T
      T′ isa Number && T == typeof(T′) || error("unsupported cast")
      y = push!(ir, stmt(T′, type = T))
    end
    return!(ir, y)
    block!(ir)
  end
  push!(ir, xcall(WIntrinsic(WebAssembly.Call(:panic), ⊥),
                  Expr(:ref, "Invalid index for $x")))
  return ir
end

function partir(x::Or, i)
  ir = IR(meta = FuncInfo(:part))
  retT = partial_part(x, i)
  vx = argument!(ir, type = x)
  vi = argument!(ir, type = i)
  union_cases!(ir, x, vx) do T, val
    # TODO possibly insert `part_method` calls and redo lowering
    ret = indexer!(ir, T, i, val, vi)
    isreftype(partial_part(T, i)) && push!(ir, Expr(:retain, ret))
    isreftype(T) && push!(ir, Expr(:release, val))
    ret = cast!(ir, partial_part(T, i), retT, ret)
    return ret
  end
  return ir
end

outlinePrimitive[part_method] = partir

function indexer!(ir, s::String, i::Int, _, _)
  @assert i == 1
  # Punt to the backend to decide how strings get IDd
  push!(ir, Expr(:ref, s))
end

function indexer!(ir, ::Type{String}, i::Int, s, _)
  @assert i == 1
  push!(ir, Expr(:ref, s, 1))
end

function indexer!(ir, T::Pack, i::Int, x, _)
  if 0 <= i <= nparts(T)
    _part(i) = push!(ir, Expr(:ref, x, i))
    range = sublayout(T, i)
    if layout(part(T, i)) isa Type
      push!(ir, stmt(Expr(:ref, x, range[1]), type = part(T, i)))
    else
      push!(ir, stmt(Expr(:tuple, _part.(range)...), type = part(T, i)))
    end
  else
    s = push!(ir, Expr(:ref, "Invalid index $i for $T"))
    push!(ir, xcall(WIntrinsic(WebAssembly.Call(:panic), ⊥), s))
  end
end

function indexer!(ir, T::VPack, I::Union{Int,Type{Int64}}, x, i)
  (I == 0 || layout(T.parts) == ()) && return push!(ir, Expr(:tuple))
  @assert T.parts == Int64
  if I isa Int
    i = Int32((I-1)*8)
  else
    i = push!(ir, xcall(WIntrinsic(i32.wrap_i64, i32), i))
    i = push!(ir, xcall(WIntrinsic(i32.sub, i32), i, Int32(1)))
    i = push!(ir, xcall(WIntrinsic(i32.mul, i32), i, Int32(8)))
  end
  # TODO bounds check
  p = push!(ir, Expr(:ref, x, 2))
  p = push!(ir, xcall(WIntrinsic(i32.add, i32), p, i))
  v = push!(ir, xcall(WIntrinsic(i64.load, i64), p))
  return v
end

inlinePrimitive[part_method] = function (pr, ir, v)
  x, i = ir[v].expr.args[2:end]
  T, I = exprtype(ir, [x, i])
  if T isa Pack && I isa Type{<:Integer}
  elseif T isa Or
  elseif T isa Recursive
    T = unroll(T)
    delete!(pr, v)
    x′ = unbox!(pr, T, x)
    y = push!(pr, stmt(xcall(part_method, x′, i), type = ir[v].type))
    replace!(pr, v, y)
    @assert T isa Or
    isreftype(ir[v].type) && push!(pr, Expr(:release, y))
  else
    delete!(pr, v)
    y = indexer!(pr, T, I, x, i)
    replace!(pr, v, y)
    isreftype(exprtype(ir, v)) && push!(pr, Expr(:retain, y))
    isreftype(T) && push!(pr, Expr(:release, x))
  end
end

# packcat
# =======

outlinePrimitive[packcat_method] = function (T::Pack)
  T′ = packcat(parts(T)...)
  @assert layout(T′.parts) == () || T′.parts == Int64
  ir = IR(meta = FuncInfo(:packcat))
  xs = argument!(ir, type = T)
  if layout(T′.parts) == ()
    push!(ir, xcall(WIntrinsic(WebAssembly.Call(:panic), ⊥),
                    Expr(:ref, "unsupported")))
    return ir
  end
  ps = [indexer!(ir, T, i, xs, i) for i in 1:nparts(T)]
  ls = [nparts!(ir, part(T, i), ps[i]) for i in 1:nparts(T)]
  size = popfirst!(ls)
  for l in ls
    size = push!(ir, xcall(WIntrinsic(i64.add, i64), size, l))
  end
  size = push!(ir, xcall(WIntrinsic(i32.wrap_i64, i32), size))
  bytes = push!(ir, xcall(WIntrinsic(i32.mul, i32), size, Int32(8)))
  margs = push!(ir, stmt(Expr(:tuple, bytes), type = rlist(Int32)))
  ptr = push!(ir, stmt(xcall(Global(:malloc!, :malloc!), margs), type = Int32))
  pos = ptr
  for i in 1:nparts(T)
    P = part(T, i)
    if P isa Pack
      for j in 1:nparts(P)
        @assert part(P, j) == Int64
        x = indexer!(ir, P, j, ps[i], j)
        push!(ir, xcall(WIntrinsic(i64.store, WTuple()), pos, x))
        pos = push!(ir, xcall(WIntrinsic(i32.add, i32), pos, Int32(8)))
      end
    elseif P isa VPack
      if P.parts != Int64
        push!(ir, xcall(WIntrinsic(WebAssembly.Call(:panic), ⊥),
                        Expr(:ref, "unsupported")))
      end
      sz = push!(ir, Expr(:ref, ps[i], 1))
      src = push!(ir, Expr(:ref, ps[i], 2))
      ln = push!(ir, xcall(WIntrinsic(i32.mul, i32), sz, Int32(8)))
      push!(ir, xcall(WIntrinsic(WebAssembly.Op(Symbol("memory.copy")), WTuple()), pos, src, ln))
      push!(ir, Expr(:release, ps[i]))
      pos = push!(ir, xcall(WIntrinsic(i32.add, i32), pos, ln))
    else
      error("unsupported")
    end
  end
  result = push!(ir, stmt(Expr(:tuple, size, ptr), type = packcat(parts(T)...)))
  return!(ir, result)
  return ir
end

inlinePrimitive[packcat_method] = function (pr, ir, v)
  x = ir[v].expr.args[2]
  S = exprtype(ir, x)
  T = ir[v].type
  if S isa Pack && T isa Pack
    @assert layout(S) == layout(T)
    pr[v] = Expr(:cast, x)
  end
end

# nparts
# ======

outlinePrimitive[nparts_method] = function (x::Or)
  ir = IR(meta = FuncInfo(:nparts))
  retT = partial_nparts(x)
  vx = argument!(ir, type = x)
  union_cases!(ir, x, vx) do T, val
    # TODO possibly insert `nparts_method` calls and redo lowering
    ret = nparts!(ir, T, val)
    ret = cast!(ir, partial_nparts(T), retT, ret)
    isreftype(T) && push!(ir, Expr(:release, val))
    return ret
  end
  return ir
end

function nparts!(ir, T::Pack, x)
  return nparts(T)
end

function nparts!(ir, T::VPack, x)
  sz = push!(ir, Expr(:ref, x, 1))
  push!(ir, xcall(WIntrinsic(i64.extend_i32_s, i64), sz))
end

inlinePrimitive[nparts_method] = function (pr, ir, v)
  x = ir[v].expr.args[2]
  T = exprtype(ir, x)
  if T isa Or
  elseif T isa Recursive
    T = unroll(T)
    delete!(pr, v)
    x′ = unbox!(pr, T, x)
    y = push!(pr, stmt(xcall(nparts_method, x′), type = Int64))
    replace!(pr, v, y)
    @assert T isa Or
    isreftype(ir[v].type) && push!(pr, Expr(:release, y))
  else
    delete!(pr, v)
    replace!(pr, v, nparts!(pr, T, x))
    isreftype(T) && push!(pr, Expr(:release, x))
  end
end

function lowerdata(ir)
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    if isexpr(st.expr, :pack)
      # remove constants, which have zero width
      args = filter(x -> x isa Variable, st.expr.args)
      pr[v] = Expr(:tuple, args...)
    elseif isexpr(st.expr, :call)
      st.expr.args[1] isa WIntrinsic && continue
      F = exprtype(ir, st.expr.args[1])
      if haskey(inlinePrimitive, F)
        inlinePrimitive[F](pr, ir, v)
      end
    end
  end
  return IRTools.finish(pr)
end

# Casts

blockargtype(bl, i) = exprtype(bl.ir, arguments(bl)[i])

function box!(ir, T, x)
  l = layout(T)
  bytes = sum(sizeof.(l))
  margs = push!(ir, stmt(Expr(:tuple, Int32(bytes)), type = rlist(Int32)))
  ptr = push!(ir, stmt(xcall(Global(:malloc!, :malloc!), margs), type = Int32))
  pos = ptr
  for (i, T) in enumerate(cat_layout(l))
    push!(ir, xcall(WIntrinsic(WType(T).store, WTuple()), pos, Expr(:ref, x, i)))
    # TODO could use constant offset here
    i == length(l) || (pos = push!(ir, xcall(WIntrinsic(i32.add, i32), pos, Int32(sizeof(T)))))
  end
  return ptr
end

function unbox!(ir, T, x; count = true)
  l = layout(T)
  bytes = sum(sizeof.(l))
  parts = []
  pos = push!(ir, stmt(Expr(:ref, x, 1), type = Int32))
  for (i, T) in enumerate(cat_layout(l))
    part = push!(ir, xcall(WIntrinsic(WType(T).load, WType(T)), pos))
    push!(parts, part)
    # TODO same as above
    i == length(l) || (pos = push!(ir, xcall(WIntrinsic(i32.add, i32), pos, Int32(sizeof(T)))))
  end
  result = push!(ir, stmt(Expr(:tuple, parts...), type = T))
  if count
    isreftype(T) && push!(ir, Expr(:retain, result))
    push!(ir, Expr(:release, x))
  end
  return result
end

# Used as a key for generated methods
cast_method = RMethod(:cast, lowerpattern(rvx"args"), nothing, false)

function cast!(ir, from, to, x)
  (to == ⊥ || from == ⊥ || from == to) && return x
  if from isa Number && to == typeof(from)
    from
  elseif from isa Or
    error("casting Or not implemented")
  elseif from isa Pack && to isa Pack
    @assert nparts(from) == nparts(to)
    parts = [indexer!(ir, from, i, x, nothing) for i = 0:nparts(from)]
    parts = [cast!(ir, part(from, i), part(to, i), parts[i+1]) for i = 0:nparts(from)]
    push!(ir, stmt(Expr(:tuple, parts...), type = to))
  elseif from == rlist() && to isa VPack
    margs = push!(ir, stmt(Expr(:tuple, Int32(0)), type = rlist(Int32)))
    ptr = push!(ir, stmt(xcall(Global(:malloc!, :malloc!), margs), type = Int32))
    push!(ir, stmt(Expr(:tuple, Int32(0), ptr), type = to))
  elseif from isa String && to == String
    indexer!(ir, from, 1, nothing, nothing)
  elseif to isa Or
    i = findfirst(==(from), to.patterns)
    @assert i != nothing
    x = (isvariable(x) ? [x] : [])
    return push!(ir, Expr(:tuple, Int32(i),
                          reduce(vcat, [j == i ? x : collect(zero.(layout(p)))
                                        for (j, p) in enumerate(to.patterns)])...))
  elseif from isa Pack && to isa Recursive
    to = unroll(to)
    x = cast!(ir, from, to, x)
    box!(ir, to, x)
  else
    error("unsupported cast: $(repr(from)) -> $(repr(to))")
  end
end

function casts!(inf::Cache, ir, ret)
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    # Cast arguments to wasm primitives
    if isexpr(st.expr, :call) && st.expr.args[1] isa WIntrinsic
      args = st.expr.args[2:end]
      Ts = exprtype(ir, args)
      pr[v] = xcall(st.expr.args[1], [T isa Integer ? T : x for (x, T) in zip(args, Ts)]...)
    elseif isexpr(st.expr, :call)
      S = (exprtype(ir, st.expr.args)...,)
      partial = S[1] isa RMethod && S[1].partial
      if !partial && !any(==(⊥), S) && inf[S] isa Redirect
        T = sig(inf, S)
        delete!(pr, v)
        args = [cast!(pr, s, t, x) for (x, s, t) in zip(st.expr.args, S, T)]
        v′ = push!(pr, stmt(xcall(args...), type = st.type))
        replace!(pr, v, v′)
      end
    elseif isexpr(st.expr, :branch)
      br = st.expr
      if isreturn(br)
        S = exprtype(ir, arguments(br)[1])
        if S != ret
          arguments(br)[1] = cast!(pr, S, ret, arguments(br)[1])
          pr[v] = br
        elseif !(arguments(br)[1] isa Variable)
          arguments(br)[1] = push!(pr, Expr(:tuple))
        end
      else
        for i = 1:length(arguments(br))
          S = exprtype(ir, arguments(br)[i])
          T = blockargtype(block(ir, br.args[1]), i)
          arguments(br)[i] isa Variable || (arguments(br)[i] = push!(pr, stmt(Expr(:tuple), type = S)))
          S == T && continue
          arguments(br)[i] = cast!(pr, S, T, arguments(br)[i])
          pr[v] = br
        end
      end
    end
  end
  return IRTools.finish(pr)
end

function lowerir(inf, ir, ret)
  # Inference expands block args, so prune them here
  ir = prune!(unloop(ir))
  ir = globals(ir)
  ir = trim_unreachable!(ir)
  ir = lowerdata(ir)
  ir = casts!(inf, ir, ret)
  return ir
end

function lowerir(inf::Cache)
  Cache{Any,Union{Redirect,IR}}() do cx, sig
    if haskey(outlinePrimitive, sig[1])
      outlinePrimitive[sig[1]](sig[2:end]...)
    elseif inf[sig] isa Redirect
      inf[sig]
    else
      lowerir(inf, inf[sig].ir, inf[sig].rettype)
    end
  end
end
