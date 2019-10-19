struct Source
  mod::VModule
  main::Vector{Any}
end

Source() = Source(VModule(), [])

function load_expr(cx::Source, x)
  push!(cx.main, x)
end

function vload(cx::Source, x::Block)
  x.name == :fn || return load_expr(m, x)
  f = x.args[1].func
  args = Tuple(x.args[1].args)
  pat, args = lowerpattern(cx.mod, args)
  method!(cx.mod, f, VMethod(pat, args, lowerfn(x, args)))
  return f
end

vload(m::VModule, x) = load_expr(m, x)

function loadfile(io::IO)
  cx = Source()
  io = LineNumberingReader(io)
  out = vnothing
  stmts(io)
  while !eof(io)
    ex = parse(io, 0)
    out = vload(cx, ex)
    stmts(io)
  end
  return cx
end

loadfile(f::String) = open(loadfile, f)
loadstring(f::String) = loadfile(IOBuffer(f))
