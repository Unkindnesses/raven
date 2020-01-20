exprtype(ir, x) = IRTools.exprtype(ir, x, typeof = identity)

union(x) = x
union(x::Data, y::Data) = x == y ? x : error("Unions not supported")
union(::Bottom, xs...) = union(xs...)
union(x::T, y::PrimitiveHole{T}) where T = PrimitiveHole{T}()
union(x::PrimitiveHole{T}, y::PrimitiveHole{T}) where T = PrimitiveHole{T}()
union(x::T, y::T) where T<:Primitive = x == y ? x : PrimitiveHole{T}()
union(x::PrimitiveHole{T}, y::T) where T = PrimitiveHole{T}()

issubtype(x::Union{T,PrimitiveHole{T}}, y::PrimitiveHole{T}) where T = true
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

function infercall!(inf, loc, block, ex)
  Ts = exprtype.((block.ir,), ex.args)
  (m, bs) = select_method(inf.mod, Ts...)
  args = [bs[x] for x in m.args]
  if m.partial != nothing
    return m.partial(args...)
  else
    T = rtuple(Ts...)
    fr = frame!(inf, T, m.func, args...)
    push!(fr.edges, loc)
    return fr.rettype
  end
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
    if isexpr(st.expr, :call) && st.expr.args[1] isa WebAssembly.Op
      block.ir[var] = Statement(block[var], type = PrimitiveHole{WebAssembly.jltype(rettype(st.expr.args[1]))}())
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
