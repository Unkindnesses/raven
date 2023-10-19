struct Compiler
  sources::Compilation
  defs::Definitions
  wasm::Wasm
  pipe::Pipeline
end

function Compiler(sources::Compilation = load(src""))
  defs = Definitions(sources)
  inferred = Inferred(defs)
  lowered = lowerir(inferred)
  counted = refcounts(lowered)
  wasm = Wasm(inferred, counted)
  pipe = Pipeline([sources, defs, inferred, lowered, counted, wasm])
  foreach(m -> pipe[(m,)], defs[tag"common.core.main"])
  return Compiler(sources, defs, wasm, pipe)
end

Base.getindex(c::Compiler, sig) = c.compiled[sig]

frame(inf::Compiler, T) = inf[sig(inf, T)]

function reload!(c::Compiler, src)
  reload!(c.sources, src)
  reset!(c.pipe)
  return c
end

function emitwasm(c::Compiler, out; path)
  main = c.defs[tag"common.core.main"]
  options().memcheck && push!(main, c.defs[tag"common.checkAllocations"][1])
  emitwasm(c.wasm, main, out; path)
end

function compile(file, opts = Options();
                 dir = dirname(file),
                 compiler = compiler)
  path = normpath(joinpath(pwd(), file))
  path, _ = splitext(path)
  name = basename(path)
  mkpath(dir)
  withoptions(opts) do
    reload!(compiler, file)
    strings = emitwasm(compiler, joinpath(dir, "$name.wasm"); path)
    emitjs(joinpath(dir, "$name.js"), "$name.wasm", strings)
  end
  return joinpath(dir, "$name.js")
end

function exec(file, opts = Options(); kw...)
  js = compile(file, opts; kw...)
  run(`node --experimental-wasm-stack-switching $js`)
  return
end

function __init__()
  global compiler = Compiler()
end
