exprtype(ir, x::Variable) = IRTools.exprtype(ir, x)
exprtype(ir, x::Union{Number,String,Symbol}) = x

struct Hole end

struct PrimitiveHole{T} end

isprimitive(x::PrimitiveHole{T}, ::Type{T}) where T = true

struct Bottom end
const ⊥ = Bottom()

union(::Bottom, xs...) = union(xs...)
union(xs::Union{T,PrimitiveHole{T}}...) where T = PrimitiveHole{T}()
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
  mod::VModule
  frames::Dict{Any,Frame}
  queue::WorkQueue{Any}
end

function infercall!(inf, loc, block, ex)
  Ts = exprtype.((block.ir,), ex.args)
  (m, bs) = select_method(Ts...)
  args = [bs[x] for x in m.args]
  if m.partial != nothing
    return m.partial(args...)
  else
    error("Call not supported")
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
    if isexpr(st.expr, :call)
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

function Inference(mod::VModule)
  q = WorkQueue{Any}()
  init = frame(mod.methods[:_start][1].func)
  push!(q, (init, 1, 1))
  inf = Inference(mod, Dict(vtuple(:_start) => init), q)
  infer!(inf)
end
