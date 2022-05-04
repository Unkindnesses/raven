using WebAssembly: WType, WTuple, i32, i64, f32, f64

rvtype(x::WType) = WebAssembly.jltype(x)
rvtype(x::WTuple) = data(:Tuple, map(rvtype, x.parts)...)
rvtype(::typeof(⊥)) = ⊥

struct WIntrinsic
  op
  ret
end

function intrinsic(ex)
  if ex isa AST.Operator && ex.op == :(:)
    typ = ex.args[2] == :unreachable ? ⊥ : WType(ex.args[2])
    ex = ex.args[1]
  else
    typ = WTuple()
  end
  op = ex.func
  if op == :call
    WIntrinsic(WebAssembly.Call(ex.args[1].expr), typ)
  else
    namify(x::Symbol) = x
    namify(x::AST.Operator) = Symbol(join(namify.(x.args), x.op))
    WIntrinsic(WebAssembly.Op(namify(op)), typ)
  end
end

function intrinsic_args(ex)
  ex isa AST.Operator && ex.op == :(:) && return intrinsic_args(ex.args[1])
  args = filter(x -> x isa AST.Operator && x.op == :(:), ex.args)
  return map(x -> AST.Call(:widen, [x.args[1]]), args)
end

WNum = Union{Int32,Int64,Float32,Float64}

function cat_layout(xs...; result = [])
  for x in xs
    x isa WTuple ? cat_layout(x.parts..., result = result) : push!(result, x)
  end
  return length(result) == 1 ? result[1] : WTuple(result)
end

layout(::Type{T}) where T = WebAssembly.WType(T)
layout(x::Union{Primitive,AST.Quote,Unreachable}) = WTuple()
layout(x::Data) = cat_layout(layout.(x.parts)...)
layout(x::VData) = WTuple([i32, i32]) # size, pointer

function tlayout(x)
  l = layout(x)
  return l isa WTuple ? l : WTuple(l)
end

nregisters(l::WType) = 1
nregisters(l::WTuple) = length(l.parts)

function sublayout(T, i)
  before = data(T.parts[1:i]...)
  offset = nregisters(layout(before))
  length = nregisters(layout(T.parts[i+1]))
  offset .+ (1:length)
end

function wparts(x)
  ly = layout(x)
  return ly isa WTuple ? ly.parts : [ly]
end

# Create a `part` method to dynamically index tuples allocated as registers.
# TODO: should make sure this comes out as a switch / branch table.
function partir(x, i)
  i <: Int64 || error("Only i64 indexes are supported.")
  T = partial_part(x, i)
  ir = IR()
  _ = argument!(ir, type = WTuple())
  vx = argument!(ir, type = layout(x))
  vi = argument!(ir, type = layout(i))
  xlayout = layout(x)
  part(i) = xlayout isa WTuple ? push!(ir, Expr(:ref, vx, i)) : vx
  for i = 1:nparts(x)
    cond = push!(ir, IRTools.stmt(xcall(i64.eq, i, vi), type = i32))
    branch!(ir, length(ir.blocks) + 2, unless = cond)
    branch!(ir, length(ir.blocks) + 1)
    block!(ir)
    range = sublayout(x, i)
    T′ = partial_part(x, i)
    ex = layout(T′) isa WTuple ?
      Expr(:tuple, part.(range)...) :
      part(range[1])
    y = push!(ir, IRTools.stmt(ex, type = layout(T′)))
    if T′ != T
      T′ isa Number && T == typeof(T′) || error("unsupported cast")
      y = push!(ir, IRTools.stmt(T′, type = layout(T)))
    end
    return!(ir, y)
    block!(ir)
  end
  push!(ir, WebAssembly.unreachable)
  return ir
end

struct WModule
  inf::Inference
  symbols::Dict{Symbol,Int}
  strings::Vector{String}
  funcs::IdDict{Any,Tuple{Symbol,Union{IR,Nothing}}}
  globals::Dict{Global,Vector{Int}}
  gtypes::Vector{WType}
end

function name(mod::WModule, s::Symbol)
  s == :_start && return s
  c = mod.symbols[s] = get(mod.symbols, s, 0)+1
  return Symbol(s, ":", c)
end

function stringid!(mod::WModule, s::String)
  i = findfirst(==(s), mod.strings)
  i === nothing || return i-1
  push!(mod.strings, s)
  return Int32(length(mod.strings)-1)
end

function global!(mod::WModule, g::Global, T)
  get!(mod.globals, g) do
    start = sum([length(gs) for gs in values(mod.globals)])
    l = tlayout(T).parts
    append!(mod.gtypes, l)
    collect(start:start+length(l)-1)
  end
end

WModule(inf) = WModule(inf, Dict(), [], Dict(), Dict(), [])

function sigs!(mod::RModule, ir::IR)
  for (v, st) in ir
    if isexpr(st.expr, :call)
      if st.expr.args[1] isa WIntrinsic
      elseif st.expr.args[1] isa RMethod
        ir[v] = xcall((exprtype(mod, ir, st.expr.args)...,), st.expr.args...)
      else
        f, xs = exprtype(mod, ir, st.expr.args)
        # TODO should probably fold this into lowering
        ir[v] = xcall((f, xs), st.expr.args...)
      end
    elseif isexpr(st.expr, :cast)
      ir[v] = Expr(:cast, st.expr.args[1], exprtype(mod, ir, st.expr.args[2]), st.expr.args[2])
    end
  end
  return ir
end

function cast!(mod::WModule, ir, v)
  to, from, x = ir[v].expr.args
  if from isa Number && to == typeof(from)
    ir[v] = IRTools.stmt(from, type = layout(to))
  elseif from == rtuple() && to isa VData
    sz = insert!(ir, v, IRTools.stmt(0, type = i32))
    malloc = lowerwasm!(mod, (:malloc, rtuple(Int32)))
    ptr = insert!(ir, v, IRTools.stmt(xcall(WebAssembly.Call(malloc), Int32(0)), type = i32))
    ir[v] = IRTools.stmt(Expr(:tuple, sz, ptr), type = layout(to))
  else
    error("unsupported cast: $from -> $to")
  end
end

# Arguments are turned into a tuple when calling any function, so this is
# basically just a cast.
function lowerdata!(mod::WModule, ir, v)
  ir[v] = ir[v].expr.args[3]
end

function lowerdatacat!(mod::WModule, ir, v)
  ir[v] = ir[v].expr.args[3]
end

ismethod(m, name) = m isa RMethod && m.name == name

# Turn global references into explicit load instructions
# TODO this function is horrendously complex for what it does.
# Should be expressed as a prewalk in the new IRTools.
function globals(mod::RModule, ir::IR)
  pr = IRTools.Pipe(ir)
  transform(x, v = nothing) = x
  function transform(x::Global, v = nothing)
    T = get(mod, x.name, ⊥)
    insert = v == nothing ? (x -> push!(pr, x)) : (x -> insert!(pr, v, x))
    insert(IRTools.stmt(Expr(:global, x.name), type = T))
  end
  IRTools.branches(pr) do b
    IRTools.Branch(b, args = [transform(x) for x in b.args],
                   condition = transform(b.condition))
  end
  for (v, st) in pr
    ex = st.expr
    if isexpr(ex, :(=))
      pr[v] = Expr(ex.head, ex.args[1], transform.(ex.args[2:end], (v,))...)
    else
      pr[v] = Expr(ex.head, transform.(ex.args, (v,))...)
    end
  end
  return IRTools.finish(pr)
end

function lowerwasm!(mod::WModule, ir::IR)
  prune!(ir)
  casts!(mod.inf.mod, ir)
  sigs!(mod.inf.mod, ir)
  ir = globals(mod.inf.mod, ir)
  for b in blocks(ir)
    IRTools.argtypes(b) .= layout.(IRTools.argtypes(b))
    for (v, st) in b
      if isexpr(st.expr, :data)
        # remove constants, which have zero width
        args = filter(x -> x isa Variable, st.expr.args)
        ir[v] = length(args) == 1 ? args[1] : Expr(:tuple, args...)
        continue
      elseif isexpr(st.expr, :cast)
        cast!(mod, ir, v)
        continue
      elseif isexpr(st.expr, :global)
        g = Global(st.expr.args[1])
        if st.type == ⊥
          ir[v] = xcall(WebAssembly.Call(:panic), stringid!(mod, "$(g.name) is not defined"))
          ir[v] = IRTools.stmt(ir[v], type = WTuple())
        else
          l = global!(mod, g, st.type)
          ir[v] = Expr(:tuple, [WebAssembly.GetGlobal(id) for id in l]...)
        end
        continue
      elseif isexpr(st.expr, :(=)) && (g = st.expr.args[1]) isa Global
        l = global!(mod, g, st.type)
        for i in 1:length(l)
          p = st.expr.args[2]
          layout(st.type) isa WTuple &&
            (p = insert!(ir, v, Expr(:ref, p, i)))
          w = insert!(ir, v, xcall(WebAssembly.SetGlobal(l[i]), p))
          ir[w] = IRTools.stmt(ir[w], type = WTuple())
        end
        delete!(ir, v)
        continue
      elseif !isexpr(st.expr, :call)
        error("unrecognised $(st.expr.head) expression")
      end
      Ts, args = st.expr.args[1], st.expr.args[2:end]
      if Ts isa WIntrinsic
        ex = xcall(st.expr.args[1].op, st.expr.args[2:end]...)
        ir[v] = IRTools.stmt(st.expr, expr = ex, type = Ts.ret == ⊥ ? WTuple() : Ts.ret)
        if Ts.ret == ⊥
          IRTools.insertafter!(ir, v, IRTools.stmt(xcall(WebAssembly.unreachable), type = WTuple()))
        end
      elseif any(x -> x == ⊥, Ts)
        ir[v] = IRTools.stmt(xcall(WebAssembly.unreachable), type = WTuple())
      elseif ismethod(Ts[1], :widen)
        val = Ts[2] isa Integer ? Ts[2] : st.expr.args[3]
        ir[v] = IRTools.stmt(val, type = layout(st.type))
      elseif ismethod(Ts[1], :data) # TODO: should specifically check this is the fallback method
        lowerdata!(mod, ir, v)
      elseif ismethod(Ts[1], :datacat)
        lowerdatacat!(mod, ir, v)
      elseif ismethod(Ts[1], :part) # TODO: same
        x::Union{String,Data}, i = Ts[2:end]
        if x isa String && i == 1
          ir[v] = IRTools.stmt(stringid!(mod, x), type = layout(st.type))
          continue
        end
        if i isa Int
          xlayout = layout(x)
          part(i) = xlayout isa WTuple ? insert!(ir, v, Expr(:ref, args[2], i)) : args[2]
          range = sublayout(x, i)
          ex = layout(st.type) isa WTuple ?
            Expr(:tuple, part.(range)...) :
            part(range[1])
          ir[v] = IRTools.stmt(ex, type = layout(st.type))
        else
          func = partmethod!(mod, Ts[1], x, i)
          ir[v] = xcall(WebAssembly.Call(func), args[2:end]...)
          ir[v] = IRTools.stmt(ir[v], type = layout(ir[v].type))
        end
      elseif ismethod(Ts[1], :nparts)
        ir[v] = nparts(Ts[2])
      else
        func = lowerwasm!(mod, Ts)
        ir[v] = xcall(WebAssembly.Call(func), args[2:end]...)
        ir[v] = IRTools.stmt(ir[v], type = layout(ir[v].type))
      end
    end
  end
  return ir
end

function lowerwasm!(mod::WModule, T)
  haskey(mod.funcs, T) && return mod.funcs[T][1]
  f = T[1]::Union{Symbol,RMethod}
  id = name(mod, f isa Symbol ? f : Symbol(f.name, ":method"))
  mod.funcs[T] = (id, nothing)
  ir = lowerwasm!(mod, mod.inf.frames[T].ir)
  mod.funcs[T] = (id, ir)
  return id
end

function partmethod!(mod::WModule, m::RMethod, x, i)
  T = (m, x, i)
  haskey(mod.funcs, T) && return mod.funcs[T][1]
  id = name(mod, Symbol("part:method"))
  ir = partir(x, i)
  mod.funcs[(m, x, i)] = (id, ir)
  return id
end

default_imports = [
  WebAssembly.Import(:support, :global, :jsglobal, :func, [], [i32]),
  WebAssembly.Import(:support, :property, :jsproperty, :func, [i32, i32], [i32]),
  WebAssembly.Import(:support, :call, :jscall0, :func, [i32, i32], [i32]),
  WebAssembly.Import(:support, :call, :jscall1, :func, [i32, i32, i32], [i32]),
  WebAssembly.Import(:support, :panic, :panic, :func, [i32], []),
  WebAssembly.Import(:support, :createRef, :jsbox, :func, [f64], [i32]),
  WebAssembly.Import(:support, :fromRef, :jsunbox, :func, [i32], [f64])]

function wasm_ir(inf::Inference)
  mod = WModule(inf)
  lowerwasm!(mod, (startmethod(inf.mod),))
  return mod
end

function wasmmodule(inf::Inference)
  mod = wasm_ir(inf)
  strings = mod.strings
  fs = [WebAssembly.irfunc(name, ir) for (name, ir) in values(mod.funcs)]
  mod = WebAssembly.Module(
    funcs = fs,
    imports = default_imports,
    exports = [WebAssembly.Export(:_start, Symbol("_start:method:1"), :func)],
    globals = [WebAssembly.Global(t) for t in mod.gtypes],
    mems = [WebAssembly.Mem(0)])
  return mod, strings
end

function binary(m::WebAssembly.Module, file; optimise = true)
  wat = tempname() * ".wat"
  WebAssembly.write_wat(wat, m)
  try
    run(`wat2wasm $wat -o $file`)
    optimise && run(`wasm-opt --enable-multivalue $file -O4 -o $file`)
  finally
    rm(wat)
  end
  return
end

function emitwasm(file, out; optimise = true)
  mod, strings = wasmmodule(loadfile(file))
  binary(mod, out; optimise)
  return strings
end

sigmatch(sig, func) = sig[1] == func || ismethod(sig[1], func)

function code_wasm(cx::Inference, func = :_main)
  mod = wasm_ir(cx)
  IdDict{Any,IR}(sig => fr[2] for (sig, fr) in mod.funcs if sigmatch(sig, func))
end

function code_typed(cx::Inference, func = :_main)
  IdDict{Any,IR}(sig => fr.ir for (sig, fr) in cx.frames if sigmatch(sig, func))
end

function code_lowered(cx::Inference, func = :_main)
  return IdDict(meth.pattern => meth.func for meth in cx.mod.methods[func])
end

code_wasm(src::AbstractString, func = :_main) = code_wasm(loadfile(src), func)
code_typed(src::AbstractString, func = :_main) = code_typed(loadfile(src), func)
code_lowered(src::AbstractString, func = :_main) = code_lowered(loadfile(src), func)
