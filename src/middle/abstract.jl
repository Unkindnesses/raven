using IRTools: WorkQueue

_typeof(mod, x) = error("invalid constant $x::$(typeof(x))")
_typeof(mod, x::Union{Number,String,Symbol,RMethod,Data}) = x
_typeof(mod, x::AST.Quote) = x.expr
_typeof(mod, x::Global) = get(mod, x.name, ⊥)

exprtype(mod, ir, x) = IRTools.exprtype(ir, x, typeof = x -> _typeof(mod, x))
exprtype(mod, ir, xs::AbstractVector) = map(x -> exprtype(mod, ir, x), xs)

union(x) = x
union(::Unreachable, x) = x
union(x, ::Unreachable) = x
union(::Unreachable, ::Unreachable) = ⊥

const NonSymbol = Union{Float32,Float64,Int32,Int64,String}

union(x::T, y::T) where T<:NonSymbol = x == y ? x : T
union(x::T, y::Type{T}) where T<:NonSymbol = T
union(x::Type{T}, y::T) where T<:NonSymbol = T
union(x::Type{T}, y::Type{T}) where T<:NonSymbol = T

union(x::Symbol, y::Symbol) = x == y ? x : Or([x, y])

partial_eltype(x::Data) = reduce(union, parts(x), init = ⊥)
partial_eltype(x::VData) = x.parts

function union(x::Data, y::Data)
  x == y && return x
  if tag(x) == tag(y)
    if nparts(x) == nparts(y)
      data(tag(x), [union(part(x, i), part(y, i)) for i = 1:nparts(x)]...)
    else
      return VData(tag(x), union(partial_eltype(x), partial_eltype(y)))
    end
  else
    return Or([x, y])
  end
end

function union(x::Data, y::VData)
  tag(x) == tag(y) || error("unimplemented union")
  VData(tag(x), union(partial_eltype(x), partial_eltype(y)))
end

function union(x::VData, y::VData)
  tag(x) == tag(y) || error("unimplemented union")
  return VData(tag(x), union(x.parts, y.parts))
end

function union(x::Union{Primitive,Type{<:Primitive},Data,VData}, y::Or)
  typedepth(y) > 10 && error("exploding type: $y")
  ps = y.patterns
  i = findfirst(y -> tag(x) == tag(y), ps)
  i == nothing && return Or([ps..., x])
  return Or([j == i ? union(x, ps[j]) : ps[j] for j = 1:length(ps)]) |> recursive
end

union(y::Or, x::Union{Primitive,Type{<:Primitive},Data,VData}) = union(x, y)

function union(x::Or, y::Or)
  reduce(union, y.patterns, init = x)
end

function union(x::Recursive, y::Union{Or,Data})
  @assert issubset(y, x)
  return x
end

union(y::Union{Or,Data}, x::Recursive) = union(x, y)

function prepare_ir!(ir)
  IRTools.expand!(ir)
  for b in ir.blocks
    b.argtypes .= (⊥,)
    for i in 1:length(b.stmts)
      b.stmts[i] = stmt(b.stmts[i], type = ⊥)
    end
  end
  return ir
end

function blockargs!(b, args)
  changed = false
  for i = 1:length(argtypes(b))
    issubset(args[i], argtypes(b)[i]) && continue
    argtypes(b)[i] = union(argtypes(b)[i], args[i])
    changed = true
  end
  return changed
end

struct Loc
  sig::Tuple
  block::Integer
  ip::Integer
end

mutable struct Frame
  ir::IR
  edges::Set{Loc}
  stmts::Vector{Vector{Variable}}
  seen::Set{Int}
  rettype
end

Base.show(io::IO, ::Frame) = print(io, "Frame(...)")

Frame(ir::IR) = Frame(ir, Set{Loc}(), keys.(blocks(ir)), Set(), ⊥)

function frame(ir::IR, args...)
  ir = prepare_ir!(copy(ir))
  @assert length(arguments(ir)) == length(args)
  argtypes(ir) .= args
  return Frame(ir)
end

struct Inference
  mod::RModule
  frames::IdDict{Any,Frame}
  globals::Dict{Symbol,Set{Loc}}
  main::Vector{Any}
  queue::WorkQueue{Loc}
end

Inference(mod::RModule) = Inference(mod, Dict(), Dict(), [], WorkQueue{Loc}())

global_edges(inf::Inference, name::Symbol) =
  get!(() -> Set{Loc}(), inf.globals, name)

function irframe!(inf, T, ir, args...)
  haskey(inf.frames, T) && return inf.frames[T]
  fr = frame(ir, args...)
  inf.frames[T] = fr
  push!(inf.queue, Loc(T, 1, 1))
  return fr
end

function frame!(inf, meth::RMethod, Ts...)
  meth.partial && return meth.func(Ts...)
  irframe!(inf, (meth, Ts...), meth.func, Ts...)
end

function frame!(inf, F, Ts)
  haskey(inf.frames, (F, Ts)) && return inf.frames[(F, Ts)]
  irframe!(inf, (F, Ts), dispatcher(inf, F, Ts), Ts)
end

function indexer!(ir::IR, arg, path)
  isempty(path) && return arg
  (p, rest...) = path
  if p isa AbstractVector
    arg = push!(ir, xlist([xpart(arg, i) for i in p]...))
  else
    arg = push!(ir, xpart(arg, p))
  end
  arg = indexer!(ir, arg, rest)
end

function dispatcher(inf, func::Symbol, Ts)
  ir = IR()
  args = argument!(ir)
  for meth in reverse(inf.mod.methods[func])
    m = partial_match(inf.mod, meth.sig.pattern, Ts)
    if m === nothing
      continue
    elseif m isa AbstractDict
      result = push!(ir, xcall(meth, [indexer!(ir, args, m[x][2]) for x in meth.sig.args]...))
      isempty(meth.sig.swap) && (result = push!(ir, xlist(result)))
      return!(ir, result)
      return ir
    else
      m = push!(ir, rcall(:match, args, rvpattern(meth.sig.pattern)))
      cond = push!(ir, xcall(isnil_method, m))
      cond = push!(ir, rcall(:not, cond))
      branch!(ir, length(blocks(ir))+2; unless = cond)
      block!(ir)
      m = push!(ir, xcall(notnil_method, m))
      as = []
      for arg in meth.sig.args
        push!(as, push!(ir, rcall(:getkey, m, arg)))
      end
      result = push!(ir, xcall(meth, as...))
      isempty(meth.sig.swap) && (result = push!(ir, xlist(result)))
      return!(ir, result)
      block!(ir)
    end
  end
  v = push!(ir, xcall(:panic, xlist("No matching method: $func: $Ts")))
  return!(ir, v)
  return ir
end

function infercall!(inf, loc, block, ex)
  if ex.args[1] isa RMethod
    F = ex.args[1]
    Ts = exprtype(inf.mod, block.ir, ex.args[2:end])
  else
    Ts = exprtype(inf.mod, block.ir, ex.args)
    F, Ts = Ts[1], (Ts[2],)
  end
  (F == ⊥ || any(==(⊥), Ts)) && return ⊥
  fr = frame!(inf, F, Ts...)
  fr isa Frame || return fr
  push!(fr.edges, loc)
  return fr.rettype
end

function openbranches(mod, bl)
  brs = []
  fallthrough = true
  for br in IRTools.branches(bl)
    br.condition == nothing && (fallthrough = false; push!(brs, br); break)
    cond = exprtype(mod, bl.ir, br.condition)
    cond == true && continue
    cond == false && (fallthrough = false; push!(brs, br); break)
    push!(brs, br)
  end
  fallthrough && push!(brs, IRTools.branch(bl.id+1))
  return brs
end

function step!(inf::Inference, loc)
  F, b, ip = loc.sig, loc.block, loc.ip
  frame = inf.frames[F]
  block, stmts = IRTools.block(frame.ir, b), frame.stmts[b]
  if ip <= length(stmts)
    var = stmts[ip]
    st = block[var]
    for g in (isexpr(st.expr, :(=)) ? st.expr.args[2:end] : st.expr.args)
      g isa Global && push!(global_edges(inf, g.name), loc)
    end
    if isexpr(st.expr, :call) && st.expr.args[1] isa WIntrinsic
      block.ir[var] = Statement(block[var], type = rvtype(st.expr.args[1].ret))
      push!(inf.queue, Loc(F, b, ip+1))
    elseif isexpr(st.expr, :call)
      T = infercall!(inf, Loc(F, b, ip), block, st.expr)
      if T != ⊥
        block.ir[var] = Statement(block[var], type = union(st.type, T))
        push!(inf.queue, Loc(F, b, ip+1))
      end
    elseif isexpr(st.expr, :data)
      Ts = exprtype(inf.mod, block.ir, st.expr.args)
      if !any(==(⊥), Ts)
        block.ir[var] = Statement(block[var], type = data(Ts...))
        push!(inf.queue, Loc(F, b, ip+1))
      end
    elseif isexpr(st.expr, :(=)) && st.expr.args[1] isa Global
      x = st.expr.args[1].name
      T = exprtype(inf.mod, block.ir, st.expr.args[2])
      T = union(get!(inf.mod.defs, x, ⊥), T)
      block.ir[var] = Statement(block[var], type = T)
      push!(inf.queue, Loc(F, b, ip+1))
      if inf.mod.defs[x] != T
        inf.mod.defs[x] = T
        foreach(loc -> push!(inf.queue, loc), global_edges(inf, x))
      end
    else
      error("Unknown expr type $(st.expr.head)")
    end
  else
    brs = openbranches(inf.mod, block)
    for br in brs
      if isreturn(br)
        T = exprtype(inf.mod, block.ir, IRTools.returnvalue(block))
        T = union(frame.rettype, T)
        T == frame.rettype && return
        frame.rettype = T
        foreach(loc -> push!(inf.queue, loc), frame.edges)
      else
        args = exprtype(inf.mod, block.ir, arguments(br))
        if (isempty(args) && !(br.block in frame.seen)) || blockargs!(IRTools.block(frame.ir, br.block), args)
          push!(frame.seen, br.block)
          push!(inf.queue, Loc(F, br.block, 1))
        end
      end
    end
  end
  return
end

# Virtual stack traces

struct Stack
  frames::Vector{Any}
end

Stack() = Stack([])

Base.length(st::Stack) = length(st.frames)

append(st::Stack, T) = Stack(Any[st.frames..., T])

function stack(inf::Inference, T; cache = Dict())
  haskey(cache, T) && return cache[T]
  cache[T] = nothing # stop cycles
  callers = unique([loc.sig for loc in inf.frames[T].edges])
  callers = [stack(inf, S) for S in callers]
  isempty(callers) && return (cache[T] = Stack([T]))
  all(==(nothing), callers) && return (cache[T] = nothing) # hit a cycle
  callers = filter(!=(nothing), callers)
  _, i = findmin(length, callers)
  return cache[T] = append(callers[i], T)
end

function Base.show(io::IO, stack::Stack)
  println(io, "Virtual stack trace:")
  for (f, Ts...) in stack.frames
    print(io, f, ": ")
    println(io, f isa RMethod ? Ts : Ts[1])
  end
end

struct CompileError <: Exception
  error
  stack::Stack
end

function Base.showerror(io::IO, err::CompileError)
  println(io, "Compiler error at")
  println(io, err.stack)
  Base.showerror(io, err.error)
end

# Inference Loop

function infer!(inf::Inference; partial = false)
  while !isempty(inf.queue)
    loc = pop!(inf.queue)
    try
      step!(inf, loc)
    catch e
      partial || rethrow(CompileError(e, stack(inf, loc.sig)))
    end
  end
  return inf
end
