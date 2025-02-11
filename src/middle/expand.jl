# After type inference, we expand to a form where reference and union types are
# represented with explicit pointers and tags, eg a `VPack` becomes a (size,
# pointer) tuple. This allows us to compile methods like `part`, `packcat` and
# casts, whose implementation needs to access those internals.
#
# It makes sense for those methods to be represented as functions, to avoid
# code duplication and so that casting can be recursive. And they can still
# participate in optimisations (mainly inlining).
#
# After this expansion all code works with primitive values (or flat tuples of
# primitives) and does explicit memory management, so the job of the backend
# code generator is simple.
#
# We also insert primitive retain/release instructions here. The RC algorithm
# will only release just after a variable is created, and then only if there are
# no uses. The `release` expression tells it to try again, and is ignored if the
# variable is used later; it should be used liberally by any code that works
# with internals. `retain` is mainly used by indexing.

function sig(inf, T)
  fr = inf[T]
  fr isa Redirect ? sig(inf, fr.to) : T
end

frame(inf::Union{Inferred,Cache}, T) = inf[sig(inf, T)]

# Panic

function abort!(ir, s)
  s = push!(ir, stmt(Expr(:ref, s), type = Int32))
  push!(ir, stmt(xcall(WImport(:support, :abort), s), type = ⊥))
end

# We're a bit fast and loose with types here, because `T` and `[T]` have the
# same representation (for now).
function call!(ir, f, args...; type)
  Ts = [T isa Number ? typeof(T) : T for T in exprtype(ir, args)]
  args = push!(ir, stmt(xtuple(args...), type = rlist(Ts...)))
  push!(ir, stmt(xcall(f, args); type))
end

# Pack primitives

cat_layout() = ()
cat_layout(x) = (x,)
cat_layout(x::Tuple) = x
cat_layout(x, xs...) = (cat_layout(x)..., cat_layout(xs...)...)

layout(T::Type{<:Union{Float64,Float32,Int64,Int32}}) = T
layout(x::Union{Primitive,Unreachable}) = ()
layout(x::Pack) = cat_layout(layout.(x.parts)...)
layout(x::VPack) = cat_layout(layout(x.tag), Int32, layout(x.parts) == () ? () : Int32) # size, pointer
layout(x::Recursive) = (Int32,)
layout(x::Recur) = Int32
layout(xs::Onion) = (Int32, cat_layout(layout.(xs.types)...)...)

function layout(T::Type{<:Bits})
  if nbits(T) <= 32
    return Int32
  elseif nbits(T) <= 64
    return Int64
  else
    error("Unsupported bit size $(nbits(T))")
  end
end

data(x::Bits) = layout(typeof(x))(x.value)
data(x::Number) = x

function tlayout(x)
  rs = layout(x)
  rs isa Tuple ? rs : (rs,)
end

nregisters(l::Type) = 1
nregisters(l::Tuple) = length(l)

function sublayout(T, i)
  before = pack(T.parts[1:i]...)
  offset = nregisters(layout(before))
  length = nregisters(layout(T.parts[i+1]))
  offset .+ (1:length)
end

sizeof(T::Type) = Base.sizeof(T)
sizeof(T) = sum(sizeof, tlayout(T), init = 0)

inlinePrimitive[pack_method] = function (pr, ir, v)
  T = exprtype(ir, v)
  if T == Float64
    pr[v] = xcall(f64.reinterpret_i64, ir[v].expr.args[2])
  elseif T == Float32
    pr[v] = xcall(f32.reinterpret_i32, ir[v].expr.args[2])
  else
    # Arguments are turned into a tuple when calling any function, so this
    # is just a cast.
    @assert tlayout(T) == tlayout(exprtype(ir, ir[v].expr.args[2]))
    pr[v] = ir[v].expr.args[2]
  end
end

# part
# ====

# Create a `part` method to dynamically index tuples allocated as registers.
# TODO: should make sure this comes out as a switch / branch table.
function partir(x, i)
  i <: Int64 || error("Only Int64 indexes are supported.")
  T = partial_part(x, i)
  ir = IR(meta = FuncInfo(tag"common.core.part"))
  vx = argument!(ir, type = x)
  vi = argument!(ir, type = i)
  xlayout = layout(x)
  part(i) = xlayout isa Tuple ? push!(ir, stmt(Expr(:ref, vx, i), type = xlayout[i])) : vx
  for i = 1:nparts(x)
    cond = call!(ir, tag"common.==", i, vi, type = Int32)
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
  abort!(ir, "Invalid index for $x")
  return ir
end

function partir(x::Onion, i)
  ir = IR(meta = FuncInfo(tag"common.core.part"))
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

function partir(s::String, i)
  @assert i == 1
  ir = IR(meta = FuncInfo(tag"common.core.part"))
  argument!(ir, type = s)
  argument!(ir, type = i)
  # Punt to the backend to decide how strings get IDd
  id = push!(ir, stmt(Expr(:ref, s), type = Int32))
  o = call!(ir, tag"common.JSObject", id, type = JSObject())
  return!(ir, o)
  return ir
end

outlinePrimitive[part_method] = partir

function indexer!(ir, T::Union{Primitive,Type{<:Primitive}}, i::Int, x, _)
  if isvalue(part(T, i))
    push!(ir, xtuple())
  elseif i > 1
    abort!(ir, "Invalid index $i for $T")
  elseif T == Float64
    push!(ir, stmt(xcall(i64.reinterpret_f64, x), type = Int64))
  elseif T == Float32
    push!(ir, stmt(xcall(i32.reinterpret_f32, x), type = Int32))
  else
    push!(ir, x)
  end
end

function indexer!(ir, T::Pack, i::Int, x, _)
  if 0 <= i <= nparts(T)
    _part(i) = push!(ir, stmt(Expr(:ref, x, i), type = layout(T)[i]))
    range = sublayout(T, i)
    if layout(part(T, i)) isa Type
      push!(ir, stmt(Expr(:ref, x, range[1]), type = part(T, i)))
    else
      push!(ir, stmt(Expr(:tuple, _part.(range)...), type = part(T, i)))
    end
  else
    abort!(ir, "Invalid index $i for $T")
  end
end

function indexer!(ir, Ts::VPack, I::Union{Int64,Type{Int64}}, x, i)
  T = Ts.parts
  (I == 0 || layout(T) == ()) && return push!(ir, Expr(:tuple))
  if I isa Int
    i = Int32((I-1)*sizeof(T))
  else
    i = call!(ir, tag"common.core.Int32", i, type = Int32)
    i = call!(ir, tag"common.-", i, Int32(1), type = Int32)
    i = call!(ir, tag"common.*", i, Int32(sizeof(T)), type = Int32)
  end
  # TODO bounds check
  p = push!(ir, stmt(Expr(:ref, x, 2), type = Int32))
  p = call!(ir, tag"common.+", p, i, type = Int32)
  return load(ir, T, p, count = false)
end

inlinePrimitive[part_method] = function (pr, ir, v)
  x, i = ir[v].expr.args[2:end]
  T, I = exprtype(ir, [x, i])
  if T isa Pack && I isa Type{<:Integer}
  elseif T isa Onion
  elseif T isa String && I == 1
  elseif T isa Recursive
    T = unroll(T)
    delete!(pr, v)
    x′ = unbox!(pr, T, x)
    y = push!(pr, stmt(xcall(part_method, x′, i), type = ir[v].type))
    replace!(pr, v, y)
    @assert T isa Onion
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

outlinePrimitive[packcat_method] = function (Ts::Pack)
  T = packcat(parts(Ts)...)::VPack
  @assert tag(T) isa Tag
  ir = IR(meta = FuncInfo(tag"common.core.packcat"))
  xs = argument!(ir, type = Ts)
  ps = [indexer!(ir, Ts, i, xs, i) for i in 1:nparts(Ts)]
  ls = [nparts!(ir, part(Ts, i), ps[i]) for i in 1:nparts(Ts)]
  ls = [isvalue(exprtype(ir, l)) ? exprtype(ir, l) : l for l in ls]
  size = popfirst!(ls)
  for l in ls
    size = call!(ir, tag"common.+", size, l, type = Int64)
  end
  size = call!(ir, tag"common.core.Int32", size, type = Int32)
  if sizeof(T.parts) == 0
    return!(ir, push!(ir, stmt(Expr(:tuple, size), type = T)))
    return ir
  end
  bytes = call!(ir, tag"common.*", size, Int32(sizeof(T.parts)), type = Int32)
  ptr = call!(ir, tag"common.malloc!", bytes, type = Int32)
  pos = ptr
  for i in 1:nparts(Ts)
    P = part(Ts, i)
    if P isa Pack
      for j in 1:nparts(P)
        x = indexer!(ir, P, j, ps[i], j)
        x = cast!(ir, part(P, j), T.parts, x)
        store!(ir, T.parts, pos, x)
        pos = call!(ir, tag"common.+", pos, Int32(sizeof(T.parts)), type = Int32)
      end
    elseif P isa VPack
      # TODO memcpy when possible
      @assert sizeof(P.parts) > 0
      len = push!(ir, stmt(Expr(:ref, ps[i], 1), type = Int32))
      src = push!(ir, stmt(Expr(:ref, ps[i], 2), type = Int32))

      before = blocks(ir)[end]
      header = block!(ir)
      body = block!(ir)
      after = block!(ir)

      branch!(before, header, pos, src, len)
      pos = argument!(header, type = Int32, insert = false)
      src = argument!(header, type = Int32, insert = false)
      len = argument!(header, type = Int32, insert = false)
      done = call!(header, tag"common.==", len, Int32(0), type = Int32)
      branch!(header, after, when = done)
      branch!(header, body)

      x = load(body, P.parts, src)
      x = cast!(body, P.parts, T.parts, x)
      store!(body, T.parts, pos, x)
      pos2 = call!(body, tag"common.+", pos, Int32(sizeof(T.parts)), type = Int32)
      src2 = call!(body, tag"common.+", src, Int32(sizeof(T.parts)), type = Int32)
      len2 = call!(body, tag"common.-", len, Int32(1), type = Int32)
      branch!(body, header, pos2, src2, len2)

      push!(ir, Expr(:release, ps[i]))
    else
      error("unsupported")
    end
  end
  result = push!(ir, stmt(Expr(:tuple, size, ptr), type = T))
  return!(ir, result)
  return ir
end

inlinePrimitive[packcat_method] = function (pr, ir, v)
  x = ir[v].expr.args[2]
  S = exprtype(ir, x)
  T = ir[v].type
  if S isa Pack && T isa Union{Pack,PrimitiveNumber,Type{<:PrimitiveNumber}}
    @assert tlayout(S) == tlayout(T)
    pr[v] = Expr(:cast, x)
  end
end

# nparts
# ======

outlinePrimitive[nparts_method] = function (x::Onion)
  ir = IR(meta = FuncInfo(tag"common.core.nparts"))
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

function nparts!(ir, T::Union{Pack,Primitive,Type{<:Primitive}}, x)
  return push!(ir, stmt(Expr(:tuple), type = nparts(T)))
end

function nparts!(ir, T::VPack, x)
  sz = push!(ir, stmt(Expr(:ref, x, 1), type = Int32))
  call!(ir, tag"common.core.Int64", sz, type = Int64)
end

inlinePrimitive[nparts_method] = function (pr, ir, v)
  x = ir[v].expr.args[2]
  T = exprtype(ir, x)
  if T isa Onion
  elseif T isa Recursive
    T = unroll(T)
    delete!(pr, v)
    x′ = unbox!(pr, T, x)
    y = push!(pr, stmt(xcall(nparts_method, x′), type = Int64))
    replace!(pr, v, y)
    @assert T isa Onion
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
    if isexpr(st, :pack)
      # remove constants, which have zero width
      args = filter(x -> x isa Variable, st.expr.args)
      pr[v] = Expr(:tuple, args...)
    elseif isexpr(st, :call)
      if st.expr.args[1] isa WebAssembly.Instruction
        haskey(wasmPartials, st.expr.args[1]) && isvalue(st.type) && (pr[v] = Expr(:tuple))
      else
        F = exprtype(ir, st.expr.args[1])
        if haskey(inlinePrimitive, F)
          inlinePrimitive[F](pr, ir, v)
        end
      end
    elseif isexpr(st, :global) && st.type == ⊥
      delete!(pr, v)
      abort!(pr, "$(st.expr.args[1].name) is not defined")
    end
  end
  return IRTools.finish(pr)
end

# Casts

blockargtype(bl, i) = exprtype(bl.ir, arguments(bl)[i])

function store!(ir, T, ptr, x)
  l = tlayout(T)
  for (i, T) in enumerate(l)
    push!(ir, stmt(xcall(WType(T).store, ptr, Expr(:ref, x, i)), type = nil))
    # TODO could use constant offset here
    i == length(l) || (ptr = call!(ir, tag"common.+", ptr, Int32(sizeof(T)), type = Int32))
  end
end

function load(ir, T, ptr; count = true)
  l = tlayout(T)
  parts = []
  for (i, T) in enumerate(l)
    part = push!(ir, stmt(xcall(WType(T).load, ptr), type = T))
    push!(parts, part)
    # TODO same as above
    i == length(l) || (ptr = call!(ir, tag"common.+", ptr, Int32(sizeof(T)), type = Int32))
  end
  x = push!(ir, stmt(Expr(:tuple, parts...), type = T))
  count && isreftype(T) && push!(ir, Expr(:retain, x))
  return x
end

function box!(ir, T, x)
  ptr = call!(ir, tag"common.malloc!", Int32(sizeof(T)), type = Int32)
  store!(ir, T, ptr, x)
  return ptr
end

function unbox!(ir, T, x; count = true)
  ptr = push!(ir, stmt(Expr(:ref, x, 1), type = Int32))
  result = load(ir, T, ptr; count)
  count && push!(ir, Expr(:release, x))
  return result
end

# Used as a key for generated methods
cast_method = RMethod(tag"common.core.cast", lowerpattern(rvx"args"), nothing, false)

function cast!(ir, from, to, x)
  (to == ⊥ || from == ⊥ || from == to) && return x
  if from isa Number && to == typeof(from)
    from
  elseif from isa Onion
    error("casting union not implemented")
  elseif from isa Pack && to isa Pack
    @assert nparts(from) == nparts(to)
    parts = [indexer!(ir, from, i, x, nothing) for i = 0:nparts(from)]
    parts = [cast!(ir, part(from, i), part(to, i), parts[i+1]) for i = 0:nparts(from)]
    push!(ir, stmt(Expr(:tuple, parts...), type = to))
  elseif from isa Pack && to isa VPack
    @assert tag(to) isa Tag
    T = to.parts
    n = nparts(from)
    if sizeof(T) == 0
      push!(ir, stmt(xtuple(Int32(n)), type = to))
    else
      ptr = call!(ir, tag"common.malloc!", Int32(sizeof(T)*n), type = Int32)
      pos = ptr
      for i = 1:n
        el = indexer!(ir, from, i, x, nothing)
        el = cast!(ir, part(from, i), T, el)
        store!(ir, T, pos, el)
        i == n || (pos = call!(ir, tag"common.+", pos, Int32(sizeof(T)), type = Int32))
      end
      push!(ir, stmt(Expr(:tuple, Int32(n), ptr), type = to))
    end
  elseif from isa String && to == RString()
    string!(ir, from)
  elseif to isa Onion
    i = findfirst(==(from), to.types)
    @assert i != nothing
    x = (isvariable(x) ? [x] : [])
    return push!(ir, Expr(:tuple, Int32(i),
                          reduce(vcat, [j == i ? x : collect(zero.(layout(p)))
                                        for (j, p) in enumerate(to.types)])...))
  elseif from isa Pack && to isa Recursive
    to = unroll(to)
    x = cast!(ir, from, to, x)
    box!(ir, to, x)
  else
    error("unsupported cast: $(repr(from)) -> $(repr(to))")
  end
end

function casts!(inf::Inferred, ir, ret)
  pr = IRTools.Pipe(ir)
  for (v, st) in pr
    # Cast arguments to wasm primitives
    if isexpr(st, :call) && st.expr.args[1] isa WebAssembly.Instruction
      args = st.expr.args[2:end]
      Ts = exprtype(ir, args)
      pr[v] = xcall(st.expr.args[1], [T isa Union{Number,Bits} ? data(T) : x for (x, T) in zip(args, Ts)]...)
    elseif isexpr(st, :call)
      S = (exprtype(ir, st.expr.args)...,)
      partial = S[1] isa RMethod && S[1].partial
      if !partial && !any(==(⊥), S) && inf[S] isa Redirect
        T = sig(inf, S)
        delete!(pr, v)
        args = [cast!(pr, s, t, x) for (x, s, t) in zip(st.expr.args, S, T)]
        v′ = push!(pr, stmt(xcall(args...), type = st.type))
        replace!(pr, v, v′)
      end
    elseif isexpr(st, :branch)
      br = st.expr
      if isreturn(br)
        S = exprtype(ir, arguments(br)[1])
        if S != ret
          arguments(br)[1] = cast!(pr, S, ret, arguments(br)[1])
          pr[v] = br
        elseif !(arguments(br)[1] isa Variable)
          arguments(br)[1] = push!(pr, stmt(Expr(:tuple), type = S))
          pr[v] = br
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

function expand(inf, ir, ret)
  ir = trim_unreachable(ir)
  ir = fuseblocks(ir)
  ir = lowerdata(ir)
  ir = casts!(inf, ir, ret)
  return ir
end

function Expanded(inf::Inferred)
  Cache{Any,Union{Redirect,IR}}() do self, sig
    if haskey(outlinePrimitive, sig[1])
      outlinePrimitive[sig[1]](sig[2:end]...)
    elseif inf[sig] isa Redirect
      inf[sig]
    else
      expand(inf, inf[sig]...)
    end
  end
end
