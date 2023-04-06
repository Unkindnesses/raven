const recursionLimit = 10

_typeof(x) = error("invalid constant $x::$(typeof(x))")
_typeof(x::Union{Number,String,Id,RMethod,Pack}) = x
_typeof(x::Global) = x.type

exprtype(ir, x) = IRTools.exprtype(ir, x, typeof = _typeof)
exprtype(ir, xs::AbstractVector) = map(x -> exprtype(ir, x), xs)

function prepare_ir!(ir)
  ir = ir |> IRTools.expand!
  for b in ir.blocks
    b.argtypes .= (⊥,)
    for i in 1:length(b.stmts)
      b.stmts[i] = stmt(b.stmts[i], type = ⊥)
    end
  end
  ir = looped(ir)
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
  path::Path
  ip::Int
end

Loc(sig, path = Path()) = Loc(sig, path, 1)

next(l::Loc) = Loc(l.sig, l.path, l.ip+1)

struct Parent
  sig::Tuple
  depth::Int
end

mutable struct Frame
  parent::Parent
  ir::LoopIR
  edges::Set{Loc}
  seen::Set{Int}
  rettype
end

struct Redirect
  to::Tuple
end

Base.show(io::IO, ::Frame) = print(io, "Frame(...)")

Frame(P, ir::LoopIR) =
  Frame(P, ir, Set{Loc}(), Set(), ⊥)

function frame(P, ir::IR, args...)
  ir = prepare_ir!(copy(ir))
  @assert length(arguments(ir.ir)) == length(args)
  argtypes(ir.body[1]) .= args
  return Frame(P, ir)
end

struct Inference
  mod::RModule
  frames::IdDict{Any,Union{Frame,Redirect}}
  globals::Dict{Symbol,Set{Loc}}
  queue::WorkQueue{Loc}
end

Inference(mod::RModule) = Inference(mod, Dict(), Dict(), WorkQueue{Loc}())

global_edges(inf::Inference, name::Symbol) =
  get!(() -> Set{Loc}(), inf.globals, name)

function sig(inf::Inference, T)
  T == () && return ()
  fr = inf.frames[T]
  fr isa Redirect ? sig(inf, fr.to) : T
end

frame(inf::Inference, T) = inf.frames[sig(inf, T)]

parent(inf, T) = sig(inf, frame(inf, T).parent.sig)

function recursionDepth(inf, T, F)
  sigs = []
  while T != ()
    T[1] == F && return frame(inf, T).parent.depth+1
    T = parent(inf, T)
  end
  return 1
end

function irframe!(inf, P, T, ir, args...)
  haskey(inf.frames, T) && return frame(inf, T)
  fr = frame(P, ir, args...)
  inf.frames[T] = fr
  push!(inf.queue, Loc(T))
  return fr
end

function frame!(inf, P, meth::RMethod, Ts...)
  meth.partial && return meth.func(Ts...)
  if P.depth > recursionLimit
    mergeFrames(inf, P.sig, (meth, Ts...))
  else
    meth.func isa IR || error("No IR for $meth: $Ts")
    irframe!(inf, P, (meth, Ts...), meth.func, Ts...)
  end
end

function frame!(inf, P, F, Ts)
  haskey(inf.frames, (F, Ts)) && return frame(inf, (F, Ts))
  irframe!(inf, P, (F, Ts), dispatcher(inf.mod, F, Ts), Ts)
end

# TODO some methods become unreachable, remove them somewhere?
function mergeFrames(inf, T, F)
  sigs = filter(t -> t[1] == F[1], [stack(inf, T).frames..., F])
  @assert length(sigs) > 1
  sig = reduce((a, b) -> union.(a, b), sigs)
  P = inf.frames[sigs[1]].parent.sig
  fr = frame!(inf, Parent(P, recursionLimit), sig...)
  for s in sigs
    s === sig && continue
    if haskey(inf.frames, s)
      if inf.frames[s] isa Redirect
        @assert sig == inf.frames[s].to # TODO figure out whether to move backedges here
      else
        union!(fr.edges, inf.frames[s].edges)
      end
    end
    inf.frames[s] = Redirect(sig)
  end
  return fr
end

function infercall!(inf, loc, block, ex)
  Ts = exprtype(block.ir, ex.args)
  any(==(⊥), Ts) && return ⊥
  P = Parent(loc.sig, recursionDepth(inf, loc.sig, Ts[1]))
  fr = frame!(inf, P, Ts...)
  fr isa Frame || return fr
  push!(fr.edges, loc)
  return fr.rettype
end

function openbranches(bl)
  brs = []
  for br in IRTools.branches(bl)
    br.args[2] == nothing && (push!(brs, br); break)
    cond = exprtype(bl.ir, br.args[2])
    cond == false && continue
    cond == true && (push!(brs, br); break)
    push!(brs, br)
  end
  return brs
end

function step!(inf::Inference, loc)
  p, ip = loc.path, loc.ip
  frame = inf.frames[loc.sig]
  frame isa Redirect && return
  bl = block(frame.ir, p)
  bl == nothing && return
  stmts = keys(bl)
  var = stmts[ip]
  st = bl[var]
  for i in (isexpr(st, :(=)) ? 2 : 1):length(st.expr.args)
    g = st.expr.args[i]
    g isa Global || continue
    push!(global_edges(inf, g.name), loc)
    T = get(inf.mod, g.name, ⊥)
    T == ⊥ && return
    st.expr.args[i] = Global(g.name, T)
  end
  if isexpr(st, :call) && st.expr.args[1] isa WIntrinsic
    op = st.expr.args[1].op
    T = rvtype(st.expr.args[1].ret)
    Ts = exprtype(bl.ir, st.expr.args[2:end])
    if all(isvalue, Ts) && haskey(wasmPartials, op)
      T = wasmPartials[op](Ts...)
    end
    bl.ir[var] = Statement(bl[var], type = T)
    push!(inf.queue, next(loc))
  elseif isexpr(st, :call)
    T = infercall!(inf, loc, bl, st.expr)
    if T != ⊥
      bl.ir[var] = Statement(bl[var], type = T)
      push!(inf.queue, next(loc))
    end
  elseif isexpr(st, :pack)
    Ts = exprtype(bl.ir, st.expr.args)
    if !any(==(⊥), Ts)
      bl.ir[var] = Statement(bl[var], type = pack(Ts...))
      push!(inf.queue, next(loc))
    end
  elseif isexpr(st, :loop)
    l = loop(bl)
    if blockargs!(l.body[1], argtypes(bl))
      push!(inf.queue, Loc(loc.sig, Path([p.parts..., (1,1)])))
    end
  elseif isexpr(st, :(=)) && st.expr.args[1] isa Global
    x = st.expr.args[1].name
    T = exprtype(bl.ir, st.expr.args[2])
    T = union(get!(inf.mod.defs, x, ⊥), T)
    bl.ir[var] = Statement(bl[var], type = T)
    push!(inf.queue, next(loc))
    if inf.mod.defs[x] != T
      inf.mod.defs[x] = T
      foreach(loc -> push!(inf.queue, loc), global_edges(inf, x))
    end
  elseif isexpr(st, :branch)
    brs = openbranches(bl)
    reroll = false
    for br in brs
      if isreturn(br)
        T = exprtype(bl.ir, IRTools.returnvalue(bl))
        T = union(frame.rettype, T)
        T == frame.rettype && return
        frame.rettype = T
        foreach(loc -> push!(inf.queue, loc), frame.edges)
      elseif isunreachable(br)
      else
        args = exprtype(bl.ir, arguments(br))
        p′, reroll = nextpath(frame.ir, p, br.args[1])
        backedge = br.args[1] < bl.id
        if reroll || (isempty(args) && !(br.args[1] in frame.seen)) || blockargs!(block(frame.ir, p′), args)
          push!(frame.seen, br.args[1])
          # Unroll loops late; try to widen types in the body first, so as not
          # to unroll too eagerly.
          p! = (backedge && p′.parts[end][1] != 1) ? pushfirst! : push!
          p!(inf.queue, Loc(loc.sig, p′))
        end
      end
    end
    reroll || checkExit(inf.queue, frame.ir, loc)
  else
    error("Unknown expr type $(st.expr.head)")
  end
  return
end

# Virtual stack traces

struct Stack
  frames::Vector{Any}
end

Stack() = Stack([])

function stack(inf::Inference, T)
  st = Stack()
  while T != ()
    pushfirst!(st.frames, T)
    T = parent(inf, T)
  end
  return st
end

function Base.show(io::IO, stack::Stack)
  println(io, "Abstract stack trace:")
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
      partial && break
      rethrow(CompileError(e, stack(inf, loc.sig)))
    end
  end
  return inf
end

function infer(mod::RModule; partial = false)
  inf = Inference(mod)
  Cache() do ch, sig
    frame!(inf, Parent((), 1), sig...)
    haskey(inf.frames, sig) || error("Can't infer types for $sig")
    infer!(inf; partial)
    for (k, fr) in inf.frames
      haskey(ch, k) && continue
      if fr isa Redirect
        ch[k] = fr
      else
        ir = prune!(unloop(fr.ir))
        ch[k] = ir => fr.rettype
      end
    end
    return ch[sig]
  end
end
