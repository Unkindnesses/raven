using IRTools: WorkQueue

_typeof(x) = x
_typeof(x::Quote) = x.expr
exprtype(ir, x) = IRTools.exprtype(ir, x, typeof = _typeof)

union(x) = x
union(x::Data, y::Data) = x == y ? x : error("Unions not supported")
union(::Bottom, xs...) = union(xs...)
union(x::T, y::Type{T}) where T = T
union(x::Type{T}, y::Type{T}) where T = T
union(x::T, y::T) where T<:Primitive = x == y ? x : T
union(x::Type{T}, y::T) where T = T

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

Frame(ir::IR) = Frame(ir, [], keys.(blocks(ir)), ⊥)

function frame(ir::IR, args...)
  ir = prepare_ir!(copy(ir))
  argtypes(ir) .= args
  return Frame(ir)
end

struct Inference
  mod::RModule
  frames::IdDict{Any,Frame}
  queue::WorkQueue{Any}
end

function frame!(inf, T, ir, args...)
  haskey(inf.frames, T) && return inf.frames[T]
  fr = frame(ir, args...)
  inf.frames[T] = fr
  for bl in reverse(blocks(ir))
    push!(inf.queue, (fr, bl.id, 1))
  end
  return fr
end

function dispatcher(inf, Ts)
  func::Symbol = Ts[1]
  args = rtuple(Ts[2:end]...)
  # TODO: just create the branch for each method, then elide later.
  applicable = [meth for meth in reverse(inf.mod.methods[func])
                if match(inf.mod, meth.pattern, args) != nothing]
  isempty(applicable) && error("No method matching $func($(join(args.parts[2:end], ", ")))")
  meth = first(applicable)
  ir = IR()
  if (is = simple_match(inf.mod, meth.pattern, args)) != nothing
    args = argument!(ir)
    args = is == (:) ? [args] :
      [push!(ir, Base.Expr(:call, part_method, args, i)) for i in is]
  else
    args = argument!(ir)
    args = push!(ir, Base.Expr(:call, :match, meth.pattern, args))
    args = [push!(ir, Base.Expr(:call, part_method, args, i)) for i = 1:length(meth.args)]
  end
  return!(ir, push!(ir, Base.Expr(:call, meth, args...)))
  return ir
end

function infercall!(inf, loc, block, ex)
  Ts = exprtype.((block.ir,), ex.args)
  if ex.args[1] isa RMethod
    meth = ex.args[1]
    Ts = Ts[2:end]
    meth.partial && return meth.func(Ts...)
    fr = frame!(inf, rtuple(meth, Ts...), meth.func, Ts...)
  else
    T = rtuple(Ts...)
    fr = frame!(inf, T, dispatcher(inf, Ts), rtuple(Ts[2:end]...))
  end
  push!(fr.edges, loc)
  return fr.rettype
end

function openbranches(bl)
  brs = []
  for br in IRTools.branches(bl)
    br.condition == nothing && (push!(brs, br); break)
    cond = exprtype(bl.ir, br.condition)
    cond == true && continue
    cond == false && (push!(brs, br); break)
    push!(brs, br)
  end
  return brs
end

function step!(inf::Inference)
  frame, b, ip = pop!(inf.queue)
  block, stmts = IRTools.block(frame.ir, b), frame.stmts[b]
  if ip <= length(stmts)
    var = stmts[ip]
    st = block[var]
    if isexpr(st.expr, :call) && st.expr.args[1] isa WIntrinsic
      block.ir[var] = Statement(block[var], type = rvtype(st.expr.args[1].ret))
      push!(inf.queue, (frame, b, ip+1))
    elseif isexpr(st.expr, :call)
      T = infercall!(inf, (frame, b, ip), block, st.expr)
      if T != ⊥
        block.ir[var] = Statement(block[var], type = union(st.type, T))
        push!(inf.queue, (frame, b, ip+1))
      end
    end
  else
    brs = openbranches(block)
    for br in brs
      if isreturn(br)
        T = exprtype(block.ir, IRTools.returnvalue(block))
        issubtype(T, frame.rettype) && return
        frame.rettype = union(frame.rettype, T)
        foreach(loc -> push!(inf.queue, loc), frame.edges)
      else
        args = exprtype.((block.ir,), arguments(br))
        if blockargs!(IRTools.block(frame.ir, br.block), args)
          push!(inf.queue, (frame, br.block, 1))
        end
      end
    end
  end
  return
end

function infer!(inf::Inference)
  while !isempty(inf.queue)
    step!(inf)
  end
  return inf
end

function Inference(mod::RModule)
  q = WorkQueue{Any}()
  inf = Inference(mod, Dict(), q)
  frame!(inf, rtuple(:_start), mod.methods[:_start][1].func)
  infer!(inf)
end
