struct Source
  mod::RModule
  main::Vector{Any}
end

Source() = Source(RModule(), [])

function load_expr(cx::Source, x)
  push!(cx.main, x)
end

function vload(cx::Source, x::Block)
  x.name == :fn || return load_expr(m, x)
  f = x.args[1].func
  args = Tuple(x.args[1].args)
  pat, args = lowerpattern(cx.mod, args)
  method!(cx.mod, f, RMethod(pat, args, lowerfn(x, args)))
  return f
end

vload(m::Source, x) = load_expr(m, x)

function finish!(cx::Source)
  fn = Block(:fn, [Call(:_start, [])], cx.main)
  method!(cx.mod, :_start, RMethod(lowerpattern(cx.mod, Tuple([])), [], lowerfn(fn, [])))
end

function loadfile(io::IO)
  cx = Source()
  io = LineNumberingReader(io)
  out = rnothing
  stmts(io)
  while !eof(io)
    ex = parse(io, 0)
    out = vload(cx, ex)
    stmts(io)
  end
  finish!(cx)
  return Inference(cx.mod)
end

loadfile(f::String) = open(loadfile, f)
loadstring(f::String) = loadfile(IOBuffer(f))
