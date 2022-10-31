_typeof(mod, x) = error("invalid constant $x::$(typeof(x))")
_typeof(mod, x::Union{Number,String,Symbol,RMethod,Pack}) = x
_typeof(mod, x::AST.Quote) = x.expr
_typeof(mod, x::Global) = get(mod, x.name, ⊥)

exprtype(mod, ir, x) = IRTools.exprtype(ir, x, typeof = x -> _typeof(mod, x))
exprtype(mod, ir, xs::AbstractVector) = map(x -> exprtype(mod, ir, x), xs)

function prepare_ir!(ir)
  ir = ir |> IRTools.expand! |> IRTools.explicitbranch!
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

mutable struct Frame
  ir::LoopIR
  edges::Set{Loc}
  seen::Set{Int}
  rettype
end

Base.show(io::IO, ::Frame) = print(io, "Frame(...)")

Frame(ir::LoopIR) = Frame(ir, Set{Loc}(), Set(), ⊥)

function frame(ir::IR, args...)
  ir = prepare_ir!(copy(ir))
  @assert length(arguments(ir.ir)) == length(args)
  argtypes(ir.body[1]) .= args
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
  push!(inf.queue, Loc(T))
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

function infercall!(inf, loc, block, ex)
  Ts = exprtype(inf.mod, block.ir, ex.args)
  any(==(⊥), Ts) && return ⊥
  fr = frame!(inf, Ts...)
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
  p, ip = loc.path, loc.ip
  frame = inf.frames[loc.sig]
  bl = block(inf, frame.ir, p)
  stmts = keys(bl)
  if ip <= length(stmts)
    var = stmts[ip]
    st = bl[var]
    for g in (isexpr(st.expr, :(=)) ? st.expr.args[2:end] : st.expr.args)
      g isa Global && push!(global_edges(inf, g.name), loc)
    end
    if isexpr(st.expr, :call) && st.expr.args[1] isa WIntrinsic
      op = st.expr.args[1].op
      T = rvtype(st.expr.args[1].ret)
      Ts = exprtype(inf.mod, bl.ir, st.expr.args[2:end])
      if all(isvalue, Ts) && haskey(wasmPartials, op)
        T = wasmPartials[op](Ts...)
      end
      bl.ir[var] = Statement(bl[var], type = T)
      push!(inf.queue, next(loc))
    elseif isexpr(st.expr, :call)
      T = infercall!(inf, loc, bl, st.expr)
      if T != ⊥
        bl.ir[var] = Statement(bl[var], type = union(st.type, T))
        push!(inf.queue, next(loc))
      end
    elseif isexpr(st.expr, :pack)
      Ts = exprtype(inf.mod, bl.ir, st.expr.args)
      if !any(==(⊥), Ts)
        bl.ir[var] = Statement(bl[var], type = pack(Ts...))
        push!(inf.queue, next(loc))
      end
    elseif isexpr(st.expr, :loop)
      l = loop(bl)
      if blockargs!(l.body[1], argtypes(bl))
        push!(inf.queue, Loc(loc.sig, Path([p.parts..., (1,1)])))
      end
    elseif isexpr(st.expr, :(=)) && st.expr.args[1] isa Global
      x = st.expr.args[1].name
      T = exprtype(inf.mod, bl.ir, st.expr.args[2])
      T = union(get!(inf.mod.defs, x, ⊥), T)
      bl.ir[var] = Statement(bl[var], type = T)
      push!(inf.queue, next(loc))
      if inf.mod.defs[x] != T
        inf.mod.defs[x] = T
        foreach(loc -> push!(inf.queue, loc), global_edges(inf, x))
      end
    else
      error("Unknown expr type $(st.expr.head)")
    end
  else
    brs = openbranches(inf.mod, bl)
    for br in brs
      if isreturn(br)
        T = exprtype(inf.mod, bl.ir, IRTools.returnvalue(bl))
        T = union(frame.rettype, T)
        T == frame.rettype && return
        frame.rettype = T
        foreach(loc -> push!(inf.queue, loc), frame.edges)
      else
        args = exprtype(inf.mod, bl.ir, arguments(br))
        p′ = nextpath(frame.ir, p, br.block)
        if (isempty(args) && !(br.block in frame.seen)) || blockargs!(block(inf, frame.ir, p′), args)
          push!(frame.seen, br.block)
          push!(inf.queue, Loc(loc.sig, p′))
        end
      end
    end
    checkExit(inf, frame.ir, p)
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
