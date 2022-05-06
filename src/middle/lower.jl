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

# TODO: check for specific methods here, not just a method of the right name.
ismethod(m, name) = m isa RMethod && m.name == name

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
      insert(IRTools.stmt(Expr(:global, x.name), type = T))
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
    cond = push!(ir, IRTools.stmt(xcall(WIntrinsic(i64.eq, i32), i, vi), type = Int32))
    branch!(ir, length(ir.blocks) + 2, unless = cond)
    branch!(ir, length(ir.blocks) + 1)
    block!(ir)
    # TODO: recurse to `indexer` here, let casting happen later
    range = sublayout(x, i)
    T′ = partial_part(x, i)
    ex = layout(T′) isa Tuple ?
      Expr(:tuple, part.(range)...) :
      part(range[1])
    y = push!(ir, IRTools.stmt(ex, type = T′))
    if T′ != T
      T′ isa Number && T == typeof(T′) || error("unsupported cast")
      y = push!(ir, IRTools.stmt(T′, type = T))
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

function indexer(cx, ir, v, s::String, i::Int, _, _)
  @assert i == 1
  # Punt to the backend to decide how strings get IDd
  ir[v] = Expr(:ref, s)
end

function indexer(cx, ir, v, T::Data, i::Int, x, _)
  _part(i) = insert!(ir, v, Expr(:ref, x, i))
  range = sublayout(T, i)
  ir[v] = Expr(:tuple, _part.(range)...)
end

function indexer(cx, ir, v, T::Data, i::Type{Int}, _, _)
  partmethod!(cx, T, i)
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
      if ismethod(F, :widen)
        T = exprtype(cx.mod, ir, st.expr.args[2])
        val = T isa Integer ? T : st.expr.args[2]
        pr[v] = val
      elseif ismethod(F, :data) || ismethod(F, :datacat)
        # Arguments are turned into a tuple when calling any function, so this
        # is just a cast.
        @assert layout(st.type) == layout(exprtype(cx.mod, ir, st.expr.args[2]))
        pr[v] = st.expr.args[2]
      elseif ismethod(F, :nparts)
        pr[v] = nparts(exprtype(mod, ir, st.expr.args[2]))
      elseif F == part_method
        x, i = st.expr.args[2:end]
        T, I = exprtype(cx.mod, ir, st.expr.args[2:end])
        indexer(cx, pr, v, T, I, x, i)
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
