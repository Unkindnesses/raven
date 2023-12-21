# Pattern types

struct Hole end

const hole = Hole()

struct Literal
  value
end

struct Repeat
  pattern
end

struct Bind
  name::Symbol
end

struct Trait
  pattern::Tag
end

# TODO we reuse the `Or` type as a pattern, don't

struct And
  patterns::Vector{Any}
end

struct Swap
  pattern
end

struct Constructor
  func::Tag
  args::Vector{Any}
end

function Base.show(io::IO, or::Or)
  for i = 1:length(or.patterns)
    i == 1 || print(io, " | ")
    show(io, or.patterns[i])
  end
end

Base.:(==)(a::Or, b::Or) = a.patterns == b.patterns

# Raven versions

rvpattern(::Hole) = pack(tag"common.Hole")
rvpattern(x::Primitive) = x
rvpattern(x::Literal) = pack(tag"common.Literal", x.value)
rvpattern(x::Bind) = pack(tag"common.Bind", Tag(x.name))
rvpattern(xs::Pack) = pack(tag"common.Pack", rvpattern.(xs.parts)...)
rvpattern(xs::And) = pack(tag"common.And", rvpattern.(xs.patterns)...)
rvpattern(x::Trait) = pack(tag"common.Trait", x.pattern)
rvpattern(xs::Constructor) = pack(tag"common.Constructor", xs.func, rvpattern.(xs.args)...)

# Pattern lowering

function resolvetags(ex::AST.Template, mod::Tag)
  @assert ex[1] == :tag
  AST.Token(modtag(mod, ex[2]), ex.meta)
end

resolvetags(ex::Union{AST.Token,AST.Atom}, mod::Tag) = ex
resolvetags(ex::AST.Expr, mod::Tag) = typeof(ex)(resolvetags.(ex.args, (mod,)), ex.meta)

function lowerisa(ex, as, resolve)
  if ex isa Symbol
    return Trait(resolve(ex)::Tag)
  elseif ex isa AST.Operator && ex[1] == :(|)
    Or(map(x -> lowerisa(x, as, resolve), ex[2:end]))
  elseif ex isa AST.Operator && ex[1] == :(&)
    And(map(x -> lowerisa(x, as, resolve), ex[2:end]))
  elseif ex isa AST.Group && length(ex) == 1
    lowerisa(ex[1], as, resolve)
  else
    _lowerpattern(ex, as, resolve)
  end
end

function _lowerpattern(ex, as, resolve)
  if ex isa Symbol
    ex == :_ || ex in as || push!(as, ex)
    return ex == :_ ? hole : Bind(ex)
  elseif ex isa Primitive
    return Literal(ex)
  elseif ex isa AST.List
    pack(Literal(tag"common.List"), map(x -> _lowerpattern(x, as, resolve), ex.args)...)
  elseif ex isa AST.Operator && ex[1] == :(:)
    name, T = ex[2:end]
    if name == :_
      lowerisa(T, as, resolve)
    else
      name in as || push!(as, name)
      And([Bind(name), lowerisa(T, as, resolve)])
    end
  elseif ex isa AST.Splat
    Repeat(_lowerpattern(ex[1], as, resolve))
  elseif ex isa AST.Call && ex[1] == :pack
    pack(map(x -> _lowerpattern(x, as, resolve), ex[2:end])...)
  elseif ex isa AST.Call
    Constructor(resolve(ex[1]::Symbol)::Tag, _lowerpattern.(ex[2:end], (as,), (resolve,)))
  elseif ex isa AST.Group && length(ex) == 1
    _lowerpattern(ex[1], as, resolve)
  else
    error("Invalid pattern syntax $(ex)")
  end
end

# At the top level, &x is allowed.
# TODO: swap should be part of the pattern, so we can reject swaps that mismatch
# the signature.
function _lowersig(ex, as, swaps, resolve)
  ex isa AST.List || return _lowerpattern(ex, as, resolve)
  args = map(enumerate(ex[:])) do (i, x)
    if x isa AST.Swap
      swaps[i] = x[1]
      _lowerpattern(x[1], as, resolve)
    elseif x isa AST.Operator && x[1] == :(:) && x[2] isa AST.Swap
      swaps[i] = x[2][1]
      _lowerpattern(AST.Operator(:(:), x[2][1], x[3:end]...), as, resolve)
    else
      _lowerpattern(x, as, resolve)
    end
  end
  pack(Literal(tag"common.List"), args...)
end

function lowerpattern(ex; mod = tag"", resolve = x -> error("Couldn't statically resolve $x"))
  ex = resolvetags(ex, mod)
  as = []
  swaps = Dict{Int,Symbol}()
  p = _lowersig(ex, as, swaps, resolve)
  return Signature(p, as, swaps)
end

# Built-in macros

namify(x::Symbol) = x
namify(ex::AST.Operator) = namify(ex[2])
namify(ex::AST.Splat) = AST.Splat(namify(ex[1]))

function bundlemacro(ex::AST.Syntax)
  super, specs = length(ex) == 2 ? (nothing, ex[2]) : (ex[2], ex[3])
  specs = specs[:]
  names = []
  body = []
  for spec in specs
    name = spec[1]
    push!(names, name)
    tag = AST.Template(:tag, string(".", name))
    args = spec[2:end]
    push!(body, AST.Syntax(:fn, spec,
                                AST.Block(
                                 AST.Call(:pack, tag, namify.(args)...))))
    push!(body, AST.Syntax(:fn, AST.Call(:isa, AST.Call(:pack, tag, args...),
                                               tag),
                                AST.Block(Symbol("true"))))
    push!(body, AST.Syntax(:fn, AST.Call(:constructorPattern, tag, namify.(args)...),
                                AST.Block(
                                  AST.Call(:And, AST.Call(:Trait, tag),
                                                 AST.Call(:Pack, AST.Call(:Literal, tag), namify.(args)...)))))
  end
  if super != nothing
    push!(body, AST.Operator(:(=), super, AST.Template(:tag, string(super))))
    push!(body, AST.Syntax(:fn, AST.Call(:isa, AST.Operator(:(:), :_, AST.Operator(:|, names...)),
                                               AST.Template(:tag, string(super))),
                                AST.Block(Symbol("true"))))
  end
  return AST.Group(body...)
end

function formacro(ex)
  x, xs, body = ex[2], ex[4], ex[5]
  itr, val = gensym.((:itr, :val))
  AST.Block(
    AST.Operator(:(=), itr, AST.Call(:iterator, xs)),
    AST.Syntax(:while, Symbol("true"), AST.Block(
      AST.Operator(:(=), val, AST.Call(:iterate, AST.Swap(itr))),
      AST.Syntax(:if, AST.Call(:isnil, val), AST.Block(:break)),
      AST.Operator(:(=), x, AST.Call(:part, val, 1)),
      body
    ))
  )
end

function showmacro(ex)
  ex = ex[2]
  name = gensym()
  AST.Block(
    AST.Call(:print, string(ex, " = ")),
    AST.Operator(:(=), name, ex),
    AST.Call(:println, name),
    name
  )
end

function testmacro(ex)
  ex = ex[2]
  AST.Syntax(:if, ex, AST.Block(
    AST.Call(:println, "pass: $ex")
  ), :else, AST.Block(
    AST.Call(:println, "fail: $ex")
  ))
end

macros = Dict(
  :bundle => bundlemacro,
  :for => formacro,
  :show => showmacro,
  :test => testmacro,
)

# Expr -> IR lowering

const nilx = AST.Call(:pack, AST.Template(:tag, "common.Nil"))

IRTools.Source(x::AST.Meta) = Source(x.file, x.loc.line, x.loc.column)
Base.convert(::Type{Source}, x::AST.Meta) = Source(x)

xcall(args...) = Expr(:call, args...)
xtuple(args...) = Expr(:tuple, args...)
xpack(args...) = Expr(:pack, args...)
xlist(args...) = xpack(tag"common.List", args...)
xpart(x, i) = xcall(part_method, x, i)

rcall(f, args...) = xpart(xcall(f, xlist(args...)), 1)

function IRTools.print_stmt(io::IO, ::Val{:pack}, ex)
  if ex.args[1] == tag"common.List"
    print(io, "[")
    join(io, [sprint(vprint, x) for x in ex.args[2:end]], ", ")
    print(io, "]")
  else
    print(io, "pack[")
    join(io, [sprint(vprint, x) for x in ex.args], ", ")
    print(io, "]")
  end
end

struct GlobalScope
  mod::Tag
end

Base.getindex(g::GlobalScope, x::Symbol) = Binding(g.mod, x)
Base.haskey(sc::GlobalScope, x::Symbol) = false
mod(sc::GlobalScope) = sc.mod

variable!(sc::GlobalScope, name) = sc[name]

swaps(sc::GlobalScope) = nothing

struct Scope
  parent::Union{Scope,GlobalScope}
  env::Dict{Symbol,Slot}
  swap::Union{Dict{Int,Symbol},Nothing} # slight hack; store for `return` lowering
end

Scope(parent; swap = nothing) = Scope(parent, Dict{Symbol,Any}(), swap)
Scope(mod::Tag; swap = nothing) = Scope(GlobalScope(mod); swap)

@forward Scope.env Base.setindex!

Base.getindex(sc::Scope, x::Symbol) = haskey(sc.env, x) ? sc.env[x] : sc.parent[x]
Base.haskey(sc::Scope, x::Symbol) = haskey(sc.env, x) || haskey(sc.parent, x)
mod(sc::Scope) = mod(sc.parent)

variable!(sc::Scope, name::Symbol) =
  haskey(sc, name) ? sc[name] : (sc[name] = Slot(name))

swaps(sc::Scope) = sc.swap == nothing ? swaps(sc.parent) : sc.swap

# don't continue lowering after return
# e.g. `f(return 1)`
_push!(ir::IR, x::Statement) = IRTools.canbranch(blocks(ir)[end]) && push!(ir, x)

_push!(ir::IR, x::Expr; src = nothing, bp = false) = _push!(ir, stmt(x; src, bp))

# lower while ignoring return value (if applicable)
_lower!(sc, ir, x) = lower!(sc, ir, x)
_lower!(sc, ir, x::Vector) = foreach(x -> _lower!(sc, ir, x), x)

lower!(sc, ir::IR, x::Union{Integer,String,Pack}) = x
lower!(sc, ir::IR, x::Vector) =
  isempty(x) ? nothing : (foreach(x -> _lower!(sc, ir, x), x[1:end-1]); lower!(sc, ir, x[end]))

lower!(sc, ir::IR, x::AST.Block) = lower!(Scope(sc), ir, x[:])
_lower!(sc, ir::IR, x::AST.Block) = _lower!(Scope(sc), ir, x[:])

lower!(sc, ir::IR, x::AST.Group) = lower!(sc, ir, x[:])
_lower!(sc, ir::IR, x::AST.Group) = _lower!(sc, ir, x[:])

function lower!(sc, ir::IR, x::Symbol)
  # TODO only active inside loop
  if x == :break
    branch!(ir, -1)
  elseif x == :return
    result = lower!(sc, ir, nilx)
    # TODO debug info
    swapreturn!(ir, result, swaps(sc), nothing, bp = true)
  else
    sc[x]
  end
end

function lower!(sc, ir::IR, ex::AST.Operator, value = true)
  if ex[1] == :(=) && ex[2] isa Symbol
    y = lower!(sc, ir, ex[3])
    x = variable!(sc, ex[2])
    _push!(ir, :($x = $y))
    return x
  elseif ex[1] == :(=)
    pat = ex[2]
    val = lower!(sc, ir, ex[3])
    lowermatch!(sc, ir, val, pat)
  elseif ex[1] in (:(&&), :(||))
    clauses = ex[1] == :(&&) ? [ex[3], Int32(false)] : [Int32(true), ex[3]]
    lowerif!(sc, ir, If([ex[2], true], clauses), value)
  else
    r = _push!(ir, xcall(lower!(sc, ir, ex[1]), xlist(map(x -> lower!(sc, ir, x), ex[2:end])...)), src = AST.meta(ex), bp = true)
    _push!(ir, xpart(r, 1), src = AST.meta(ex))
  end
end

_lower!(sc, ir::IR, ex::AST.Operator) = lower!(sc, ir, ex, false)

function argtuple!(sc, ir::IR, args, src)
  swaps = []
  parts = []
  idx = 1
  splat = false
  while !isempty(args)
    if first(args) isa AST.Splat
      push!(parts, lower!(sc, ir, popfirst!(args)[1]))
      splat = true
    else
      as = []
      while !(isempty(args) || first(args) isa AST.Splat)
        arg = popfirst!(args)
        if arg isa AST.Swap && !splat
          arg = arg[1]
          push!(swaps, arg => idx)
        end
        push!(as, lower!(sc, ir, arg))
        idx += 1
      end
      push!(parts, _push!(ir, xlist(as...); src))
    end
  end
  args =
    isempty(parts) ? _push!(ir, xpack(tag"common.List")) :
    length(parts) == 1 ? parts[1] :
    _push!(ir, xcall(packcat_method, xlist(parts...)))
  return args, swaps
end

function lower!(sc, ir::IR, ex::AST.Call)
  args, swaps = argtuple!(sc, ir, ex[2:end], AST.meta(ex))
  result = _push!(ir, xcall(lower!(sc, ir, ex[1]), args),
                  src = AST.meta(ex), bp = true)
  val = _push!(ir, xpart(result, 1), src = AST.meta(ex))
  for (x, i) in swaps
    _push!(ir, Expr(:(=), variable!(sc, x), xpart(result, i+1)))
  end
  return val
end

function lower!(sc, ir::IR, ex::AST.List)
  # TODO: should use the `tuple` function.
  # But this puts off the need for special argument inference.
  argtuple!(sc, ir, ex[:], AST.meta(ex))[1]
end

function swapreturn!(ir::IR, val, swaps, src; bp = false)
  if swaps != nothing && !isempty(swaps)
    args = maximum(keys(swaps))
    ret = push!(ir, xlist(val, map(i -> haskey(swaps, i) ? Slot(swaps[i]) : Binding(tag"common", :nil), 1:args)...))
    return!(ir, ret; src, bp)
  else
    return!(ir, val; src, bp)
  end
  return
end

function lower!(sc, ir::IR, ex::AST.Template)
  @assert ex[1] == :tag
  return modtag(mod(sc), ex[2])
end

function lowermatch!(sc, ir::IR, val, pat)
  sig = lowerpattern(pat, mod = mod(sc), resolve = resolve_static)
  pat = rvpattern(sig.pattern)
  m = push!(ir, rcall(tag"common.match", val, pat))
  isnil = push!(ir, xcall(isnil_method, m))
  branch!(ir, length(blocks(ir))+1, when = isnil)
  branch!(ir, length(blocks(ir))+2)
  block!(ir)
  push!(ir, rcall(tag"common.panic", "match failed: $(sig.pattern)"))
  block!(ir)
  m = push!(ir, xcall(notnil_method, m))
  for arg in sig.args
    push!(ir, Expr(:(=), variable!(sc, arg), rcall(tag"common.getkey", m, Tag(arg))))
  end
  return m
end

function lowerwhile!(sc, ir::IR, ex, value = true)
  sc = Scope(sc)
  header = block!(ir)
  branch!(blocks(ir)[end-1], header)
  cond = lower!(sc, ir, ex[2])
  cond = _push!(ir, rcall(tag"common.condition", cond), src = AST.meta(ex))
  bodyStart = block!(ir)
  _lower!(sc, ir, ex[3][:])
  bodyEnd = blocks(ir)[end]
  after = block!(ir)
  # Rewrite continue/break to the right block number
  for i = header.id:bodyEnd.id
    bl = block(ir, i)
    for j = 1:length(branches(bl))
      br = branches(bl)[j]
      br.args[1] == -1 && (br.args[1] = after.id)
    end
  end
  branch!(header, bodyStart, when = cond, src = AST.meta(ex))
  branch!(header, after, src = AST.meta(ex.args[1]))
  IRTools.canbranch(bodyEnd) && branch!(bodyEnd, header)
  return value ? Binding(tag"common", :nil) : nothing
end

struct If
  cond::Vector{Any}
  body::Vector{Any}
end

function If(b::AST.Syntax)
  cond = []
  body = []
  push!(cond, b[2])
  push!(body, b[3])
  args = b[4:end]
  while !isempty(args)
    @assert popfirst!(args) == :else "Broken if block"
    if args[1] == :if
      popfirst!(args)
      push!(cond, popfirst!(args))
    else
      push!(cond, true)
    end
    push!(body, popfirst!(args))
  end
  if cond[end] !== true
    push!(cond, true)
    push!(body, AST.Call(:pack, AST.Template(:tag, "common.Nil")))
  end
  return If(cond, body)
end

function lowerif!(sc, ir::IR, ex::If, value = true)
  sc = Scope(sc)
  ts = []
  vs = []
  body!(ir, ex) =
    value ?
      push!(vs, lower!(sc, ir, ex)) :
      _lower!(sc, ir, ex)
  for (cond, body) in zip(ex.cond, ex.body)
    if cond === true
      body!(ir, body)
      push!(ts, blocks(ir)[end])
      block!(ir)
      break
    end
    cond = lower!(sc, ir, cond)
    cond = _push!(ir, rcall(tag"common.condition", cond))
    c = blocks(ir)[end]
    t = block!(ir)
    body!(ir, body)
    push!(ts, blocks(ir)[end])
    f = block!(ir)
    branch!(c, t, when = cond)
    branch!(c, f)
  end
  b = blocks(ir)[end]
  for i = 1:length(ts)
    IRTools.canbranch(ts[i]) &&
      (value ?
        branch!(ts[i], b, vs[i]) :
        branch!(ts[i], b))
  end
  value && IRTools.argument!(b, insert = false)
end

# TODO support pattern matching
function lowerlet!(sc, ir::IR, ex, value = true)
  assignments = ex[2:end-1]
  @assert all(x -> x isa AST.Operator && x[1] == :(=), assignments)
  vars = map(x -> x[2], assignments)
  vals = map(x -> lower!(sc, ir, x[3]), assignments)
  sc = Scope(sc)
  foreach(x -> sc.env[x] = Slot(gensym(x)), vars)
  foreach((x, y) -> _push!(ir, :($(sc[x]) = $y)), vars, vals)
  (value ? lower! : _lower!)(sc, ir, ex[end])
end

function lower!(sc, ir::IR, ex::AST.Syntax, value = true)
  if ex[1] == :return
    result = lower!(sc, ir, ex[2])
    swapreturn!(ir, result, swaps(sc), AST.meta(ex), bp = true)
  elseif ex[1] == :while
    lowerwhile!(sc, ir, ex, value)
  elseif ex[1] == :if
    lowerif!(sc, ir, If(ex), value)
  elseif ex[1] == :wasm
    src = AST.meta(ex)
    ex = ex[2][1]
    op = intrinsic(ex)
    args = lower!.((sc,), (ir,), intrinsic_args(ex))
    _push!(ir, xcall(op, args...); src, bp = true)
  elseif ex[1] == :let
    lowerlet!(sc, ir, ex, value)
  elseif haskey(macros, ex[1])
    (value ? lower! : _lower!)(sc, ir, macros[ex[1]](ex))
  else
    error("unrecognised block $(ex[1])")
  end
end

_lower!(sc, ir::IR, ex::AST.Syntax) = lower!(sc, ir, ex, false)

fnsig(ex) = lowerpattern(AST.List(ex[2][:]...))

# This is only used (hackily) by `lowermatch!`, so we avoid cluttering the code.
# TODO: remove the need for `resolve` in lowering entirely
withresolve(f, r) = dynamic_bind(f, :resolve, r)
resolve_static(x) = dynamic_value(:resolve)(x)

function lowerfn(mod::Tag, sig::Signature, body::AST.Expr; meta = nothing, resolve)
  sc = Scope(mod, swap = sig.swap)
  ir = IR(; meta)
  for arg in sig.args
    sc[arg] = Slot(arg)
    push!(ir, :($(Slot(arg)) = $(argument!(ir))))
  end
  out = withresolve(resolve) do
    lower!(sc, ir, body)
  end
  out == nothing || swapreturn!(ir, out, sig.swap, nothing)
  ir = ir |> fuseblocks |> IRTools.ssa! |> IRTools.prune! |> IRTools.renumber
end

# Turn toplevel vars into global writes

function assignments(ir::IR)
  locals = Set{Symbol}()
  for (v, st) in ir
    isexpr(st, :(=)) && st.expr.args[1] isa Slot && push!(locals, st.expr.args[1].id)
  end
  return locals
end

# Assumes all globals are in the same module
function rewrite_globals(ir::IR, cx::RModule)
  globals = Set{Symbol}()
  locals = filter(x -> haskey(cx, x), assignments(ir))
  pr = IRTools.Pipe(ir)
  foreach(x -> push!(pr, :($(Slot(x)) = $(Binding(cx.name, x)))), locals)
  for (v, st) in pr
    # Global loads use the new slot
    ex = IRTools.prewalk(x -> x isa Binding && (x.name in locals || x.name in globals) ? Slot(x.name) : x, st.expr)
    # Global stores use the new slot
    if isexpr(ex, :(=)) && ex.args[1] isa Binding
      push!(globals, ex.args[1].name)
      ex = :($(Slot(ex.args[1].name)) = $(ex.args[2]))
    end
    pr[v] = ex
  end
  foreach(x -> push!(pr, :($(Binding(cx.name, x)) = $(Slot(x)))), Base.union(locals, globals))
  return IRTools.finish(pr), globals
end

function assigned_globals(ir::IR)
  globals = Dict{Binding,Any}()
  for (v, st) in ir
    isexpr(st, :(=)) && st.expr.args[1] isa Binding &&
      (globals[st.expr.args[1]] = st.type)
  end
  return globals
end

function lower_toplevel(cx::RModule, ex; meta = nothing, resolve)
  sc = GlobalScope(cx.name)
  ir = IR(; meta)
  withresolve(resolve) do
    _lower!(sc, ir, ex)
  end
  IRTools.return!(ir, lower!(sc, ir, nilx))
  ir, defs = rewrite_globals(ir, cx)
  ir |> fuseblocks |> IRTools.ssa! |> IRTools.prune! |> IRTools.renumber, defs
end

# Turn global references into explicit load instructions
function globals(ir::IR)
  pr = IRTools.Pipe(ir)
  function transform(x)
    x isa Binding || return x
    push!(pr, Expr(:global, x))
  end
  for (v, st) in pr
    ex = st.expr
    delete!(pr, v)
    ex = isexpr(ex, :(=)) ?
      Expr(ex.head, ex.args[1], transform.(ex.args[2:end])...) :
      Expr(ex.head, transform.(ex.args)...)
    v′ = push!(pr, stmt(ir[v], expr = ex))
    replace!(pr, v, v′)
  end
  return IRTools.finish(pr)
end
