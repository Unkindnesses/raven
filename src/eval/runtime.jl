struct VModule
  defs::Dict{Symbol,Any}
end

VModule() = VModule(Dict{Symbol,Any}())

@forward VModule.defs Base.getindex, Base.setindex!

function eval_expr(m::VModule, x)
  ir = lowerexpr(x)
  interpret(ir)
end

function veval(m::VModule, x::Block)
  x.name == :fn || eval_expr(m, x)
  f = x.args[1].func
  m[f] = lowerfn(x)
  return f
end

veval(m::VModule, x) = eval_expr(m, x)

const main = VModule()

veval(x) = veval(main, x)

function evalfile(io::IO)
  io = LineNumberingReader(io)
  out = nothing
  stmts(io)
  while !eof(io)
    ex = parse(io, 0)
    out = veval(ex)
    stmts(io)
  end
  return out
end

evalfile(f::String) = open(evalfile, f)
evalstring(f::String) = evalfile(IOBuffer(f))

macro vs_str(x)
  :(evalstring($x))
end

# Builtins

main[:>] = >
main[:+] = +
main[:-] = -
main[:*] = *
main[:/] = /

main[:struct] = (a...) -> Struct([a...])
