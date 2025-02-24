const recursionLimit = 10

_typeof(x) = error("invalid constant $x::$(typeof(x))")
_typeof(x::Union{Bits,Number,String,Tag,RMethod,Pack}) = x

# TODO constants should be converted early to empty tuples of constant type.
# If literals appear in the IR they are runtime values (eg args to foreign calls)
exprtype(ir, x) = IRTools.exprtype(ir isa Block ? ir.ir : ir, x, typeof = _typeof)
exprtype(ir, xs::Union{AbstractVector,Tuple}) = map(x -> exprtype(ir, x), xs)

function prepare_ir!(ir)
  ir = ir |> globals |> IRTools.expand!
  # TODO use the API
  for b in ir.blocks
    b.argtypes .= (⊥,)
    for i in 1:length(b.stmts)
      T = b.stmts[i][2].type
      T == Any && (T = ⊥)
      b.stmts[i] = (b.stmts[i][1], stmt(b.stmts[i][2], type = T))
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
  meths::EagerCache
  deps::IdDict{Any,Set{Caches.NFT}}
  frames::IdDict{Sig,Union{Frame,GlobalFrame,Redirect}}
  queue::WorkQueue{Tuple}
end

function Inference(defs::Definitions, meths::EagerCache)
  Inference(defs, meths, IdDict(), Dict(), WorkQueue{Tuple}())
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
    T, deps = Caches.trackdeps(() -> inf.defs[name])
    inf.deps[name] = deps
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

function frame!(inf::Inference, P, meth::RMethod, Ts...)
  if meth.partial
    meth.func(Ts...)
  elseif haskey(inf.frames, (meth, Ts...))
    frame(inf, (meth, Ts...))
  elseif P.depth > recursionLimit
    mergeFrames(inf, P.sig, (meth, Ts...))
  else
    meth.func isa IR || error("No IR for $meth: $Ts")
    irframe!(inf, P, meth.func, meth, Ts...)
  end
end

function frame!(inf::Inference, P, F, Ts)
  haskey(inf.frames, (F, Ts)) && return frame(inf, (F, Ts))
  inf.frames[(F, Ts)] = Frame(P, looped(IR()))
  update!(inf, (F, Ts))
  return frame(inf, (F, Ts))
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

function infercall!(inf::Inference, P, sig...)
  any(==(⊥), sig) && return ⊥
  P = Parent(P, recursionDepth(inf, P, sig[1]))
  fr = frame!(inf, P, sig...)
  fr isa Frame || return fr
  pf = inf.frames[P.sig]
  pf isa Redirect && return
  push!(pf.deps, sig)
  push!(fr.edges, P.sig)
  return fr.rettype
end

function infercall!(inf::Inference, P, ir, ex::Expr)
  infercall!(inf, P, exprtype(ir, ex.args)...)
end

function cleardeps!(inf::Inference, sig)
  fr = inf.frames[sig]
  for dep in fr.deps
    delete!(inf.frames[dep].edges, sig)
  end
  empty!(fr.deps)
  return
end

function update_dispatcher!(inf::Inference, sig)
  (ir, ret), deps = Caches.trackdeps(() -> dispatcher(inf, sig...))
  inf.deps[sig] = deps
  fr = inf.frames[sig]
  fr.ir = looped(IRTools.expand!(ir))
  if !issubset(ret, fr.rettype)
    fr.rettype = ret
    foreach(s -> push!(inf.queue, s), fr.edges)
  end
end

function update!(inf::Inference, sig)
  cleardeps!(inf, sig)
  sig[1] isa RMethod || return update_dispatcher!(inf, sig)
  fr = inf.frames[sig]
  ret = ⊥
  path = Path()
  reachable = Set([path])
  while path != nothing
    @label loop
    bl = block(fr.ir, path)
    for (v, st) in bl
      if isexpr(st, :call) && st.expr.args[1] isa WebAssembly.Instruction
        op = st.expr.args[1]
        Ts = exprtype(bl.ir, st.expr.args[2:end])
        if all(isvalue, Ts) && haskey(wasmPartials, op)
          T = wasmPartials[op](Ts...)
          bl.ir[v] = stmt(bl[v], type = T)
        end
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
        blockargs!(l.body[1], argtypes(bl))
        path = Path([path.parts..., (1,1)])
        @goto loop
      elseif isexpr(st, :branch)
        br = st.expr
        if isreturn(br)
          ret = union(ret, exprtype(bl.ir, only(arguments(br))))
        elseif isunreachable(br)
          break
        else
          cond = exprtype(bl.ir, something(br.args[2], RBool(true)))
          @assert tag(cond) == tag"common.Bool"
          cond == RBool(false) && continue
          p, rr = nextpath(fr.ir, path, br.args[1])
          rr = rr || pin!(fr.ir, path, length(p))
          @assert !rr || p < path "unimplemented"
          args = exprtype(bl.ir, arguments(br))
          push!(reachable, p)
          if (blockargs!(block(fr.ir, p), args) || rr) && p < path
            path = p
            @goto loop
          end
          cond == RBool(true) && break
        end
      else
        error("Unknown expr type $(st.expr.head)")
      end
    end
    while true
      path = nextpath(fr.ir, path)
      (isnothing(path) || path in reachable) && break
    end
  end
  if !issubset(ret, fr.rettype)
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
  inf::Inference
  results::Caches.Dict{Any,Any}
end

function Inferred(defs::Definitions, meths::EagerCache)
  inf = Inference(defs, meths)
  return Inferred(inf, Caches.Dict())
end

Caches.iscached(i::Inferred, k) = iscached(i.results, k)

function Base.getindex(i::Inferred, sig)
  iscached(i, sig) && return i.results[sig]
  # Don't let inference dependencies leak
  _, deps = Caches.trackdeps() do
    frame!(i.inf, Parent((), 1), sig...)
    infer!(i.inf)
  end
  @assert isempty(deps)
  for (k, fr) in i.inf.frames
    (k isa Binding || iscached(i, k)) && continue
    i.results[k] = fr isa Redirect ? fr : (prune!(unloop(fr.ir)) => fr.rettype)
  end
  return i.results[sig]
end

Caches.fingerprint(i::Inferred) = fingerprint(i.results)

function Caches.reset!(i::Inferred; deps = [])
  print = fingerprint(deps)
  for (x, deps) in i.inf.deps
    Base.issubset(deps, print) || delete!(i.inf, x)
  end
  for k in keys(i.results)
    haskey(i.inf.frames, k) || delete!(i.results, k)
  end
end
