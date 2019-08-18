struct VModule
  defs::Dict{Symbol,Any}
end

VModule() = VModule(Dict{Symbol,Any}())

@forward VModule.defs Base.getindex, Base.setindex!

function veval(m::VModule, x::Block)
  x.name == :fn || error("Unrecognised expr $x")
  f = x.args[1].func
  m[f] = lowerfn(x)
  return f
end

const main = VModule()

veval(x) = veval(main, x)

function evalfile(io::IO)
  ts = TokenStream(LineNumberingReader(io))
  out = nothing
  while (ex = parse(ts, 0)) != '\0'
    out = veval(ex)
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
