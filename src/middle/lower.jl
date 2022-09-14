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

struct Compilation
  mod::RModule
  frames::IdDict{Any,IR}
end

Compilation(mod::RModule) = Compilation(mod, IdDict{Any,IR}())

# Global variables

# Turn global references into explicit load instructions
# TODO this function is horrendously complex for what it does.
# Should be expressed as a prewalk in the new IRTools.
function globals(mod::RModule, ir::IR)
  pr = IRTools.Pipe(ir)
  transform(x, v = nothing) = x
  function transform(x::Global, v = nothing)
    T = get(mod, x.name, ⊥)
    insert = v == nothing ? (x -> push!(pr, x)) : (x -> insert!(pr, v, x))
    if T == ⊥
      insert(stmt(xcall(WIntrinsic(WebAssembly.Call(:panic), ⊥),
                        Expr(:ref, "$(x.name) is not defined")), type = ⊥))
    else
      insert(stmt(Expr(:global, x.name), type = T))
    end
  end
  IRTools.branches(pr) do b
    IRTools.Branch(b, args = [transform(x) for x in b.args],
                   condition = transform(b.condition))
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
layout(x::Recursive) = Int32
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

lowerPrimitive[pack_method] = function (cx, pr, ir, v)
  # Arguments are turned into a tuple when calling any function, so this
  # is just a cast.
  @assert layout(ir[v].type) == layout(exprtype(cx.mod, ir, ir[v].expr.args[2]))
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
  ir = IR()
  vx = argument!(ir, type = x)
  vi = argument!(ir, type = i)
  xlayout = layout(x)
  part(i) = xlayout isa Tuple ? push!(ir, Expr(:ref, vx, i)) : vx
  for i = 1:nparts(x)
    cond = push!(ir, stmt(xcall(WIntrinsic(i64.eq, i32), i, vi), type = Int32))
    branch!(ir, length(ir.blocks) + 2, unless = cond)
    branch!(ir, length(ir.blocks) + 1)
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

function partmethod!(cx::Compilation, x, i)
  T = (part_method, x, i)
  haskey(cx.frames, T) && return cx.frames[T][1]
  ir = partir(x, i)
  cx.frames[T] = ir
  return
end

function union_downcast!(ir, T::Or, i::Integer, x)
  offset = sum(length, layout.(T.patterns[1:i-1]), init = 0)+1
  parts = [push!(ir, Expr(:ref, x, j+offset)) for j = 1:length(layout(T.patterns[i]))]
  return layout(T.patterns[i]) isa Tuple ? push!(ir, Expr(:tuple, parts...)) : parts[1]
end

function union_partir(x::Or, i)
  ir = IR()
  T = partial_part(x, i)
  vx = argument!(ir, type = x)
  vi = argument!(ir, type = i)
  j = push!(ir, Expr(:ref, vx, 1))
  for (case, T) in enumerate(x.patterns)
    cond = push!(ir, xcall(WIntrinsic(i32.eq, i32), j, Int32(case)))
    branch!(ir, length(blocks(ir))+2, unless = cond)
    block!(ir)
    val = union_downcast!(ir, x, case, vx)
    # TODO possibly insert `part_method` calls and redo lowering
    ret = indexer!(ir, T, i, val, vi)
    return!(ir, ret)
    block!(ir)
  end
  push!(ir, xcall(WIntrinsic(WebAssembly.unreachable, ⊥)))
  return ir
end

function union_partmethod!(cx::Compilation, x, i)
  T = (part_method, x, i)
  haskey(cx.frames, T) && return cx.frames[T][1]
  ir = union_partir(x, i)
  cx.frames[T] = ir
  return
end

function indexer!(ir, s::String, i::Int, _, _)
  @assert i == 1
  # Punt to the backend to decide how strings get IDd
  push!(ir, Expr(:ref, s))
end

function indexer!(ir, ::Type{String}, i::Int, s, _)
  @assert i == 1
  push!(ir, Expr(:ref, s, 1))
end

function _indexer!(f, T::Pack, i::Int, x)
  if 0 <= i <= nparts(T)
    _part(i) = f(Expr(:ref, x, i))
    range = sublayout(T, i)
    if layout(part(T, i)) isa Type
      f(stmt(Expr(:ref, x, range[1]), type = part(T, i)))
    else
      f(stmt(Expr(:tuple, _part.(range)...), type = part(T, i)))
    end
  else
    s = f(Expr(:ref, "Invalid index $i for $T"))
    f(xcall(WIntrinsic(WebAssembly.Call(:panic), ⊥), s))
  end
end

function indexer!(ir, T::Pack, i::Int, x, _)
  _indexer!(ex -> push!(ir, ex), T, i, x)
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

lowerPrimitive[part_method] = function (cx, pr, ir, v)
  x, i = ir[v].expr.args[2:end]
  T, I = exprtype(cx.mod, ir, ir[v].expr.args[2:end])
  if T isa Pack && I isa Type{<:Integer}
    partmethod!(cx, T, I)
  elseif T isa Or
    union_partmethod!(cx, T, I)
  else
    delete!(pr, v)
    y = indexer!(pr, T, I, x, i)
    replace!(pr, v, y)
    if isreftype(exprtype(cx.mod, ir, v))
      push!(pr, Expr(:retain, y))
      push!(pr, Expr(:release, y))
    end
    isreftype(T) && push!(pr, Expr(:release, x))
  end
end

# packcat
# =======

function packcat_ir(T::Pack)
  T′ = packcat(parts(T)...)
  @assert layout(T′.parts) == () || T′.parts == Int64
  ir = IR()
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
  ptr = push!(ir, stmt(xcall(Global(:malloc!), margs), type = Int32))
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

function packcat_method!(cx::Compilation, T)
  S = (packcat_method, T)
  haskey(cx.frames, S) && return
  ir = packcat_ir(T)
  cx.frames[S] = ir
  return
end

lowerPrimitive[packcat_method] = function (cx, pr, ir, v)
  x = ir[v].expr.args[2]
  S = exprtype(cx.mod, ir, x)
  T = ir[v].type
  if S isa Pack && T isa Pack
    @assert layout(S) == layout(T)
    pr[v] = Expr(:cast, x)
  else
    packcat_method!(cx, S)
  end
end

# nparts
# ======

function nparts!(ir, T::Pack, x)
  return nparts(T)
end

function nparts!(ir, T::VPack, x)
  sz = push!(ir, Expr(:ref, x, 1))
  push!(ir, xcall(WIntrinsic(i64.extend_i32_s, i64), sz))
end

lowerPrimitive[nparts_method] = function (cx, pr, ir, v)
  x = ir[v].expr.args[2]
  T = exprtype(cx.mod, ir, x)
  delete!(pr, v)
  replace!(pr, v, nparts!(pr, T, x))
  isreftype(T) && push!(pr, Expr(:release, x))
end

function lowerdata(cx, ir)
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    if isexpr(st.expr, :pack)
      # remove constants, which have zero width
      # TODO: better to do this based on type, even though it doesn't come up
      # yet
      args = filter(x -> x isa Variable, st.expr.args)
      pr[v] = Expr(:tuple, args...)
    elseif isexpr(st.expr, :call)
      st.expr.args[1] isa WIntrinsic && continue
      F = exprtype(cx.mod, ir, st.expr.args[1])
      if haskey(lowerPrimitive, F)
        lowerPrimitive[F](cx, pr, ir, v)
      end
    end
  end
  return IRTools.finish(pr)
end

# Casts

blockargtype(mod::RModule, bl, i) = exprtype(mod, bl.ir, arguments(bl)[i])

storeinstr(::Type{Int64}) = i64.store
storeinstr(::Type{Int32}) = i32.store

function box!(ir, T, x)
  l = layout(T)
  bytes = sum(sizeof.(l))
  margs = push!(ir, stmt(Expr(:tuple, Int32(bytes)), type = rlist(Int32)))
  ptr = push!(ir, stmt(xcall(Global(:malloc!), margs), type = Int32))
  pos = ptr
  for (i, T) in enumerate(cat_layout(l))
    push!(ir, xcall(WIntrinsic(storeinstr(T), WTuple()), pos, Expr(:ref, x, i)))
    # TODO could use constant offset here
    i == length(l) || (pos = push!(ir, xcall(WIntrinsic(i32.add, i32), pos, Int32(sizeof(T)))))
  end
  return ptr
end

function cast!(ir, from, to, x)
  (to == ⊥ || from == to) && return x
  if from isa Number && to == typeof(from)
    from
  elseif from isa Pack && to isa Pack
    @assert nparts(from) == nparts(to)
    parts = [indexer!(ir, from, i, x, nothing) for i = 0:nparts(from)]
    parts = [cast!(ir, part(from, i), part(to, i), parts[i+1]) for i = 0:nparts(from)]
    push!(ir, Expr(:tuple, parts...))
  elseif from == rlist() && to isa VPack
    margs = push!(ir, stmt(Expr(:tuple, Int32(0)), type = rlist(Int32)))
    ptr = push!(ir, stmt(xcall(Global(:malloc!), margs), type = Int32))
    push!(ir, stmt(Expr(:tuple, Int32(0), ptr), type = to))
  elseif from isa String && to == String
    indexer!(ir, from, 1, nothing, nothing)
  elseif to isa Or
    i = findfirst(==(from), to.patterns)
    @assert i != nothing
    x = (x isa Variable ? [x] : [])
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

function casts!(mod::RModule, ir, ret)
  for bl in blocks(ir)
    for br in branches(bl)
      if isreturn(br)
        S = exprtype(mod, ir, arguments(br)[1])
        if S != ret
          arguments(br)[1] = cast!(bl, S, ret, arguments(br)[1])
        elseif !(arguments(br)[1] isa Variable)
          arguments(br)[1] = push!(bl, Expr(:tuple))
        end
      else
        for i = 1:length(arguments(br))
          S = exprtype(mod, ir, arguments(br)[i])
          T = blockargtype(mod, block(ir, br.block), i)
          S == T && continue
          arguments(br)[i] = cast!(bl, S, T, arguments(br)[i])
        end
      end
    end
  end
  return ir
end

function lowerir(cx, ir, ret)
  # Inference expands block args, so prune them here
  ir = prune!(copy(ir))
  ir = trim_unreachable!(ir)
  ir = globals(cx.mod, ir)
  ir = lowerdata(cx, ir)
  ir = casts!(cx.mod, ir, ret)
  return ir
end

function lowerir(inf::Inference)
  comp = Compilation(inf.mod)
  for (k, fr) in inf.frames
    comp.frames[k] = lowerir(comp, fr.ir, fr.rettype)
  end
  return refcounts(comp)
end
