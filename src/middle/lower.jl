# After type inference, we lower to a form where reference and union types are
# represented with explicit pointers and tags, eg a `VData` becomes a (size,
# pointer) tuple. This allows us to compile methods like `part`, `datacat` and
# casts, whose implementation needs to access those internals.
#
# It makes sense for those methods to be represented as functions, to avoid
# code duplication and so that casting can be recursive. And they can still
# participate in optimisations (mainly inlining).
#
# After this lowering all code works with primitive values (or flat tuples of
# primitives) and does explicit memory management, so the job of the backend
# code generator is simple.

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
      insert(xcall(WIntrinsic(WebAssembly.Call(:panic), ⊥),
                   Expr(:ref, "$(x.name) is not defined")))
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

# Data primitives

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

layout(::Type{T}) where T = T
layout(x::Union{Primitive,AST.Quote,Unreachable}) = ()
layout(x::Data) = cat_layout(layout.(x.parts)...)
layout(x::VData) = (Int32, Int32) # size, pointer

nregisters(l::Type) = 1
nregisters(l::Tuple) = length(l)

function sublayout(T, i)
  before = data(T.parts[1:i]...)
  offset = nregisters(layout(before))
  length = nregisters(layout(T.parts[i+1]))
  offset .+ (1:length)
end

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

function indexer!(ir, s::String, i::Int, _, _)
  @assert i == 1
  # Punt to the backend to decide how strings get IDd
  push!(ir, Expr(:ref, s))
end

function indexer!(ir, T::Data, i::Int, x, _)
  if 0 <= i <= nparts(T)
    _part(i) = push!(ir, Expr(:ref, x, i))
    range = sublayout(T, i)
    push!(ir, stmt(Expr(:tuple, _part.(range)...), type = part(T, i)))
  else
    s = push!(ir, Expr(:ref, "Invalid index $i for $T"))
    push!(ir, xcall(WIntrinsic(WebAssembly.Call(:panic), ⊥), s))
  end
end

function indexer!(ir, T::VData, I::Union{Int,Type{Int64}}, x, i)
  I == 0 && return push!(ir, Expr(:tuple))
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

function datacat_ir(T::Data)
  T′ = datacat(parts(T)...)
  @assert T′.parts == Int64
  ir = IR()
  xs = argument!(ir, type = T)
  ps = [indexer!(ir, T, i, xs, i) for i in 1:nparts(T)]
  ls = [nparts!(ir, part(T, i), ps[i]) for i in 1:nparts(T)]
  size = popfirst!(ls)
  for l in ls
    size = push!(ir, xcall(WIntrinsic(i64.add, i64), size, l))
  end
  size = push!(ir, xcall(WIntrinsic(i32.wrap_i64, i32), size))
  bytes = push!(ir, xcall(WIntrinsic(i32.mul, i32), size, Int32(8)))
  margs = push!(ir, stmt(Expr(:tuple, bytes), type = rtuple(Int32)))
  ptr = push!(ir, stmt(xcall(Global(:malloc), margs), type = Int32))
  pos = ptr
  for i in 1:nparts(T)
    P = part(T, i)
    if P isa Data
      for j in 1:nparts(P)
        @assert part(P, j) == Int64
        x = indexer!(ir, P, j, ps[i], j)
        push!(ir, xcall(WIntrinsic(i64.store, WTuple()), pos, x))
        pos = push!(ir, xcall(WIntrinsic(i32.add, i32), pos, Int32(8)))
      end
    elseif P isa VData
      if P.parts != Int64
        push!(ir, xcall(WIntrinsic(WebAssembly.Call(:panic), ⊥),
                        Expr(:ref, "unsupported")))
      end
      sz = push!(ir, Expr(:ref, ps[i], 1))
      src = push!(ir, Expr(:ref, ps[i], 2))
      ln = push!(ir, xcall(WIntrinsic(i32.mul, i32), sz, Int32(8)))
      push!(ir, xcall(WIntrinsic(WebAssembly.Op(Symbol("memory.copy")), WTuple()), pos, src, ln))
      pos = push!(ir, xcall(WIntrinsic(i32.add, i32), pos, ln))
    else
      error("unsupported")
    end
  end
  result = push!(ir, stmt(Expr(:tuple, size, ptr), type = datacat(parts(T))))
  return!(ir, result)
  return ir
end

function datacat_method!(cx::Compilation, T)
  S = (datacat_method, T)
  haskey(cx.frames, S) && return cx.frames[S][1]
  ir = datacat_ir(T)
  cx.frames[S] = ir
  return
end

function nparts!(ir, T::Data, x)
  return nparts(T)
end

function nparts!(ir, T::VData, x)
  sz = push!(ir, Expr(:ref, x, 1))
  push!(ir, xcall(WIntrinsic(i64.extend_i32_s, i64), sz))
end

function lowerdata(cx, ir)
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    if isexpr(st.expr, :data)
      # remove constants, which have zero width
      # TODO: better to do this based on type, even though it doesn't come up
      # yet
      args = filter(x -> x isa Variable, st.expr.args)
      pr[v] = Expr(:tuple, args...)
    elseif isexpr(st.expr, :call)
      st.expr.args[1] isa WIntrinsic && continue
      F = exprtype(cx.mod, ir, st.expr.args[1])
      if F == widen_method
        T = exprtype(cx.mod, ir, st.expr.args[2])
        val = T isa Integer ? T : st.expr.args[2]
        pr[v] = val
      elseif F == data_method
        # Arguments are turned into a tuple when calling any function, so this
        # is just a cast.
        @assert layout(st.type) == layout(exprtype(cx.mod, ir, st.expr.args[2]))
        pr[v] = st.expr.args[2]
      elseif F == datacat_method
        x = st.expr.args[2]
        S = exprtype(cx.mod, ir, x)
        T = st.type
        if S isa Data && T isa Data
          @assert layout(S) == layout(T)
          pr[v] = x
        else
          datacat_method!(cx, S)
        end
      elseif F == nparts_method
        x = st.expr.args[2]
        T = exprtype(cx.mod, ir, x)
        delete!(pr, v)
        replace!(pr, v, nparts!(pr, T, x))
      elseif F == part_method
        x, i = st.expr.args[2:end]
        T, I = exprtype(cx.mod, ir, st.expr.args[2:end])
        if T isa Data && I isa Type{<:Integer}
          partmethod!(cx, T, I)
        else
          delete!(pr, v)
          replace!(pr, v, indexer!(pr, T, I, x, i))
        end
      end
    end
  end
  return IRTools.finish(pr)
end

# Casts

blockargtype(mod::RModule, bl, i) = exprtype(mod, bl.ir, arguments(bl)[i])

# TODO should have a separate pass that prunes unreachable code.
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
    margs = push!(ir, stmt(Expr(:tuple, Int32(0)), type = rtuple(Int32)))
    ptr = push!(ir, stmt(xcall(Global(:malloc), margs), type = Int32))
    push!(ir, stmt(Expr(:tuple, Int32(0), ptr), type = to))
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

function lowerir(cx, ir)
  # Inference expands block args, so prune them here
  ir = prune!(copy(ir))
  ir = globals(cx.mod, ir)
  ir = lowerdata(cx, ir)
  ir = casts!(cx.mod, ir)
  return ir
end

function lowerir(inf::Inference)
  comp = Compilation(inf.mod)
  for (k, fr) in inf.frames
    comp.frames[k] = lowerir(comp, fr.ir)
  end
  return comp
end
