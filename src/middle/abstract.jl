const recursionLimit = 10

_typeof(x) = error("invalid constant $x::$(typeof(x))")
_typeof(x::Union{Number,String,Tag,RMethod,Pack}) = x

exprtype(ir, x) = IRTools.exprtype(ir, x, typeof = _typeof)
exprtype(ir, xs::AbstractVector) = map(x -> exprtype(ir, x), xs)

function prepare_ir!(ir)
  ir = ir |> globals |> IRTools.expand!
  # TODO use the API
  for b in ir.blocks
    b.argtypes .= (⊥,)
    for i in 1:length(b.stmts)
      b.stmts[i] = (b.stmts[i][1], stmt(b.stmts[i][2], type = ⊥))
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

struct Parent
  sig::Tuple
  depth::Int
end

struct Redirect
  to::Tuple
end

const Sig = Union{Tuple,Binding}

mutable struct GlobalFrame
  type
  deps::Set{Sig}
  edges::Set{Sig}
end

GlobalFrame(T) = GlobalFrame(T, Set(), Set())

mutable struct Frame
  parent::Parent
  ir::LoopIR
  deps::Set{Sig}
  edges::Set{Sig}
  rettype
end

Frame(P, ir::LoopIR, T = ⊥) =
  Frame(P, ir, Set(), Set(), T)

Base.show(io::IO, ::Frame) = print(io, "Frame(...)")

function frame(P, ir::IR, args...)
  ir = prepare_ir!(copy(ir))
  @assert length(arguments(ir.ir)) == length(args)
  argtypes(ir.body[1]) .= args
  return Frame(P, ir)
end

function const_frame(P, T, F, sig...)
  name = F isa Tag ? F : F.name
  ir = IR(meta = FuncInfo(name, trampoline = true))
  foreach(T -> argument!(ir, T), sig)
  r = push!(ir, stmt(Expr(:tuple), type = T))
  return!(ir, r)
  return Frame(P, looped(ir), T)
end

struct Inference
  defs::Definitions
  int::Interpreter
  dispatchers::Dispatchers
  deps::IdDict{Any,Caches.NFT}
  frames::IdDict{Sig,Union{Frame,GlobalFrame,Redirect}}
  queue::WorkQueue{Tuple}
end

function Inference(defs::Definitions, int::Interpreter, ds::Dispatchers)
  Inference(defs, int, ds, IdDict(), Dict(), WorkQueue{Tuple}())
end

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

function frame!(inf::Inference, name::Binding)
  get!(inf.frames, name) do
    T = inf.defs[name]
    inf.deps[name] = Caches.id(inf.defs.globals, name)[2]
    if T isa Binding
      parent = frame!(inf, T)
      push!(parent.edges, name)
      T = parent.type
    end
    GlobalFrame(T)
  end
end

function irframe!(inf::Inference, P, ir, F, Ts...)
  inf.frames[(F, Ts...)] = frame(P, ir, Ts...)
  update!(inf, (F, Ts...))
  return frame(inf, (F, Ts...))
end

function interpframe!(inf::Inference, P, sig...)
  T = inf.int[sig]
  (isnothing(T) || !isvalue(T)) && return
  fr = inf.frames[sig] = const_frame(P, T, sig...)
  inf.deps[sig] = Caches.id(inf.int.results, sig)[2]
  return fr
end

function frame!(inf::Inference, P, meth::RMethod, Ts...)
  if meth.partial
    meth.func(Ts...)
  elseif haskey(inf.frames, (meth, Ts...))
    frame(inf, (meth, Ts...))
  elseif (fr = interpframe!(inf, P, meth, Ts...)) != nothing
    fr
  elseif P.depth > recursionLimit
    mergeFrames(inf, P.sig, (meth, Ts...))
  else
    meth.func isa IR || error("No IR for $meth: $Ts")
    irframe!(inf, P, meth.func, meth, Ts...)
  end
end

function frame!(inf::Inference, P, F, Ts)
  haskey(inf.frames, (F, Ts)) && return frame(inf, (F, Ts))
  (fr = interpframe!(inf, P, F, Ts)) == nothing || return fr
  ir = inf.dispatchers[(F, Ts)]
  inf.deps[(F, Ts)] = Caches.id(inf.dispatchers, (F, Ts))[2]
  irframe!(inf, P, ir, F, Ts)
end

# TODO some methods become unreachable, remove them somewhere?
function mergeFrames(inf::Inference, T, F)
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
        push!(fr.edges, s) # propagate deletions
      end
    end
    inf.frames[s] = Redirect(sig)
  end
  return fr
end

function infercall!(inf::Inference, sig, block, ex)
  Ts = exprtype(block.ir, ex.args)
  any(==(⊥), Ts) && return ⊥
  P = Parent(sig, recursionDepth(inf, sig, Ts[1]))
  fr = frame!(inf, P, Ts...)
  fr isa Frame || return fr
  pf = inf.frames[sig]
  pf isa Redirect && return
  push!(pf.deps, (Ts...,))
  push!(fr.edges, sig)
  return fr.rettype
end

function cleardeps!(inf::Inference, sig)
  fr = inf.frames[sig]
  for dep in fr.deps
    delete!(inf.frames[dep].edges, sig)
  end
  empty!(fr.deps)
  return
end

# TODO: rm work queue
function update!(inf::Inference, sig)
  cleardeps!(inf, sig)
  fr = inf.frames[sig]
  ret = ⊥
  path = Path()
  seen = Set{Path}()
  queue = WorkQueue{Path}()
  push!(queue, Path())
  while !isempty(queue)
    path = pop!(queue)
    push!(seen, path)
    bl = block(fr.ir, path)
    reroll = false
    for (v, st) in bl
      if isexpr(st, :call) && st.expr.args[1] isa WIntrinsic
        op = st.expr.args[1].op
        T = rvtype(st.expr.args[1].ret)
        Ts = exprtype(bl.ir, st.expr.args[2:end])
        if all(isvalue, Ts) && haskey(wasmPartials, op)
          T = wasmPartials[op](Ts...)
        end
        bl.ir[v] = stmt(bl[v], type = T)
      elseif isexpr(st, :call)
        T = infercall!(inf, sig, bl, st.expr)
        T == nothing && return ⊥
        T == ⊥ && break
        bl.ir[v] = stmt(bl[v], type = T)
      elseif isexpr(st, :pack)
        Ts = exprtype(bl.ir, st.expr.args)
        any(==(⊥), Ts) && break
        bl.ir[v] = stmt(bl[v], type = pack(Ts...))
      elseif isexpr(st, :global)
        g = frame!(inf, st.expr.args[1])
        push!(fr.deps, st.expr.args[1])
        push!(g.edges, sig)
        g.type == ⊥ && break
        bl.ir[v] = stmt(bl[v], type = g.type)
      elseif isexpr(st, :(=)) && st.expr.args[1] isa Binding
        T = exprtype(bl.ir, st.expr.args[2])
        T == ⊥ && break
        bl.ir[v] = stmt(st, type = T)
      elseif isexpr(st, :loop)
        l = loop(bl)
        if blockargs!(l.body[1], argtypes(bl))
          push!(queue, Path([path.parts..., (1,1)]))
        end
      elseif isexpr(st, :branch)
        br = st.expr
        if isreturn(br)
          ret = union(ret, exprtype(bl.ir, only(arguments(br))))
        elseif isunreachable(br)
          break
        else
          cond = exprtype(bl.ir, something(br.args[2], Int32(1)))
          cond == false && continue
          args = exprtype(bl.ir, arguments(br))
          p′, reroll = nextpath(fr.ir, path, br.args[1])
          backedge = br.args[1] < bl.id
          if reroll || blockargs!(block(fr.ir, p′), args) || !(p′ in seen)
            # Unroll loops late; try to widen types in the body first, so as not
            # to unroll too eagerly.
            p! = (backedge && p′.parts[end][1] != 1) ? pushfirst! : push!
            p!(queue, p′)
          end
          cond == true && break
        end
      else
        error("Unknown expr type $(st.expr.head)")
      end
      reroll || checkExit(inf.queue, fr.ir, path)
    end
  end
  if !(issubset(ret, fr.rettype))
    fr.rettype = ret
    foreach(s -> push!(inf.queue, s), fr.edges)
  end
  return
end

# TODO remove backedges, so we don't do redundant work
function Base.delete!(inf::Inference, sig::Union{Tuple,Binding})
  haskey(inf.frames, sig) || return
  fr = inf.frames[sig]
  # cleardeps!(inf, sig)
  delete!(inf.frames, sig)
  delete!(inf.deps, sig)
  fr isa Redirect && return
  foreach(loc -> delete!(inf, loc), fr.edges)
end

# Virtual stack traces

struct Stack
  frames::Vector{Any}
end

Stack() = Stack([])

function stack(inf::Inference, T)
  st = Stack()
  while haskey(inf.frames, T)
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
    sig = pop!(inf.queue)
    try
      update!(inf, sig)
    catch e
      partial && break
      rethrow(CompileError(e, stack(inf, sig)))
    end
  end
  return inf
end

# Results and caching

struct Inferred
  dispatchers::Dispatchers
  inf::Inference
  results::Caches.Dict{Any,Any}
end

function Inferred(defs::Definitions, int::Interpreter)
  ds = dispatchers(int)
  inf = Inference(defs, int, ds)
  return Inferred(ds, inf, Caches.Dict())
end

Caches.iscached(i::Inferred, k) = iscached(i.results, k)

function Base.getindex(i::Inferred, sig)
  iscached(i, sig) && return i.results[sig]
  # Don't let inference dependencies leak
  Caches.trackdeps() do
    frame!(i.inf, Parent((), 1), sig...)
    infer!(i.inf)
  end
  for (k, fr) in i.inf.frames
    (k isa Binding || iscached(i, k)) && continue
    i.results[k] = fr isa Redirect ? fr : (prune!(unloop(fr.ir)) => fr.rettype)
  end
  return i.results[sig]
end

Caches.fingerprint(i::Inferred) = fingerprint(i.results)

function Caches.reset!(i::Inferred; deps = [])
  print = fingerprint(deps)
  reset!(i.dispatchers, deps = print)
  print = Base.union(print, fingerprint(i.dispatchers))
  for (x, dep) in i.inf.deps
    dep in print || delete!(i.inf, x)
  end
  for k in keys(i.results)
    haskey(i.inf.frames, k) || delete!(i.results, k)
  end
end
