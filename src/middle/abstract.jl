using IRTools: WorkQueue

_typeof(mod, x) = error("invalid constant $x::$(typeof(x))")
_typeof(mod, x::Union{Number,String,Symbol,RMethod}) = x
_typeof(mod, x::Quote) = x.expr
_typeof(mod, x::Global) = get(mod, x.name, ⊥)

exprtype(mod, ir, x) = IRTools.exprtype(ir, x, typeof = x -> _typeof(mod, x))
exprtype(mod, ir, xs::AbstractVector) = map(x -> exprtype(mod, ir, x), xs)

union(x) = x
union(::Unreachable, x) = x
union(x, ::Unreachable) = x
union(x::T, y::Type{T}) where T = T
union(x::Type{T}, y::Type{T}) where T = T
union(x::T, y::T) where T<:Primitive = x == y ? x : T
union(x::Type{T}, y::T) where T = T

partial_eltype(x::Data) = reduce(union, parts(x), init = ⊥)
partial_eltype(x::VData) = x.parts

function union(x::Data, y::Data)
  x == y && return x
  if nparts(x) == nparts(y)
    error("unimplemented union")
  else
    tag(x) == tag(y) || error("unimplemented union")
    return VData(tag(x), union(partial_eltype(x), partial_eltype(y)))
  end
end

function union(x::Data, y::VData)
  tag(x) == tag(y) || error("unimplemented union")
  VData(tag(x), union(partial_eltype(x), partial_eltype(y)))
end

issubtype(x::Union{T,Type{T}}, y::Type{T}) where T = true
issubtype(x, y) = x == y

function prepare_ir!(ir)
  IRTools.expand!(ir)
  for b in ir.blocks
    b.argtypes .= (⊥,)
    for i in 1:length(b.stmts)
      b.stmts[i] = IRTools.stmt(b.stmts[i], type = ⊥)
    end
  end
  return ir
end

function blockargs!(b, args)
  changed = false
  for i = 1:length(argtypes(b))
    issubtype(args[i], argtypes(b)[i]) && continue
    argtypes(b)[i] = union(argtypes(b)[i], args[i])
    changed = true
  end
  return changed
end

mutable struct Frame
  ir::IR
  edges::Vector{Any}
  stmts::Vector{Vector{Variable}}
  rettype
end

Base.show(io::IO, ::Frame) = print(io, "Frame(...)")

Frame(ir::IR) = Frame(ir, [], keys.(blocks(ir)), ⊥)

function frame(ir::IR, args...)
  ir = prepare_ir!(copy(ir))
  argtypes(ir) .= args
  return Frame(ir)
end

struct Inference
  mod::RModule
  frames::IdDict{Any,Frame}
  main::Vector{Any}
  queue::WorkQueue{Any}
end

Inference(mod::RModule) = Inference(mod, Dict(), [], WorkQueue{Any}())

function irframe!(inf, T, ir, args...)
  haskey(inf.frames, T) && return inf.frames[T]
  fr = frame(ir, args...)
  inf.frames[T] = fr
  for bl in reverse(blocks(ir))
    push!(inf.queue, (T, bl.id, 1))
  end
  return fr
end

function frame!(inf, meth::RMethod, Ts...)
  meth.partial && return meth.func(Ts...)
  irframe!(inf, (meth, Ts...), meth.func, Ts...)
end

function frame!(inf, F, Ts)
  irframe!(inf, (F, Ts), dispatcher(inf, F, Ts), Ts)
end

part_method = RMethod(:part, lowerpattern(rvx"[data, i]")..., part, true)

function indexer!(ir::IR, arg, path)
  isempty(path) && return arg
  (p, rest...) = path
  arg = indexer!(ir, arg, rest)
  if p isa AbstractVector
    push!(ir, Base.Expr(:tuple, [Base.Expr(:call, part_method, arg, i) for i in p]...))
  else
    push!(ir, Base.Expr(:call, part_method, arg, p))
  end
end

function dispatcher(inf, F, Ts)
  func::Symbol = F
  ir = IR()
  args = argument!(ir)
  for meth in reverse(inf.mod.methods[func])
    m = partial_match(inf.mod, meth.pattern, Ts)
    if m === nothing
      continue
    elseif m isa AbstractDict
      return!(ir, push!(ir, Base.Expr(:call, meth, [indexer!(ir, args, m[x][2]) for x in meth.args]...)))
      return ir
    else
      error("Runtime matching not yet supported")
    end
  end
  v = push!(ir, Base.Expr(:call, :panic, Base.Expr(:tuple, "No matching method: $F: $Ts")))
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
  any(==(⊥), Ts) && return ⊥
  fr = frame!(inf, F, Ts...)
  fr isa Frame || return fr
  push!(fr.edges, loc)
  return fr.rettype
end

function openbranches(inf, bl)
  brs = []
  for br in IRTools.branches(bl)
    br.condition == nothing && (push!(brs, br); break)
    cond = exprtype(inf.mod, bl.ir, br.condition)
    cond == true && continue
    cond == false && (push!(brs, br); break)
    push!(brs, br)
  end
  return brs
end

function step!(inf::Inference, loc)
  F, b, ip = loc
  frame = inf.frames[F]
  block, stmts = IRTools.block(frame.ir, b), frame.stmts[b]
  if ip <= length(stmts)
    var = stmts[ip]
    st = block[var]
    if isexpr(st.expr, :call) && st.expr.args[1] isa WIntrinsic
      block.ir[var] = Statement(block[var], type = rvtype(st.expr.args[1].ret))
      push!(inf.queue, (F, b, ip+1))
    elseif isexpr(st.expr, :call)
      T = infercall!(inf, (F, b, ip), block, st.expr)
      if T != ⊥
        block.ir[var] = Statement(block[var], type = union(st.type, T))
        push!(inf.queue, (F, b, ip+1))
      end
    elseif isexpr(st.expr, :tuple)
      Ts = exprtype(inf.mod, block.ir, st.expr.args)
      if !any(==(⊥), Ts)
        block.ir[var] = Statement(block[var], type = rtuple(Ts...))
        push!(inf.queue, (F, b, ip+1))
      end
    elseif isexpr(st.expr, :(=)) && st.expr.args[1] isa Global
      x = st.expr.args[1].name
      T = exprtype(inf.mod, block.ir, st.expr.args[2])
      block.ir[var] = Statement(block[var], type = T)
      inf.mod.defs[x] = T
      push!(inf.queue, (F, b, ip+1))
    else
      error("Unknown expr type $(st.expr.head)")
    end
  else
    brs = openbranches(inf, block)
    for br in brs
      if isreturn(br)
        T = exprtype(inf.mod, block.ir, IRTools.returnvalue(block))
        issubtype(T, frame.rettype) && return
        frame.rettype = union(frame.rettype, T)
        foreach(loc -> push!(inf.queue, loc), frame.edges)
      else
        args = exprtype(inf.mod, block.ir, arguments(br))
        if blockargs!(IRTools.block(frame.ir, br.block), args)
          push!(inf.queue, (F, br.block, 1))
        end
      end
    end
  end
  return
end

# Virtual stack traces

struct CompileError <: Exception
  error
  stack
end

function Base.showerror(io::IO, err::CompileError)
  println(io, "Compiler error at")
  println(io, err.stack)
  Base.showerror(io, err.error)
end

# Inference Loop

function infer!(inf::Inference)
  while !isempty(inf.queue)
    loc = pop!(inf.queue)
    try
      step!(inf, loc)
    catch e
      rethrow(CompileError(e, loc))
    end
  end
  return inf
end
