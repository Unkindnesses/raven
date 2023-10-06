struct Compiler
  sources::Compilation
  defs::Definitions
  compiled::Pipeline
end

function Compiler(sources::Compilation = load(src""))
  defs = Definitions(sources)
  inferred = Inferred(defs)
  lowered = lowerir(inferred)
  counted = refcounts(lowered)
  pipe = Pipeline([sources, defs, inferred, lowered, counted])
  foreach(m -> pipe[(m,)], defs[tag"common.core.main"])
  return Compiler(sources, defs, pipe)
end

Base.getindex(c::Compiler, sig) = c.compiled[sig]

frame(inf::Compiler, T) = inf[sig(inf, T)]

function reload!(c::Compiler, src)
  reload!(c.sources, src)
  reset!(c.compiled)
  return c
end

function compile(file, opts = Options(); dir = dirname(file))
  path = normpath(joinpath(pwd(), file))
  path, _ = splitext(path)
  name = basename(path)
  mkpath(dir)
  withoptions(opts) do
    comp = Compiler()
    reload!(comp, file)
    strings = emitwasm(comp, joinpath(dir, "$name.wasm"); path)
    emitjs(joinpath(dir, "$name.js"), "$name.wasm", strings)
  end
  return
end

function exec(file, opts = Options())
  compile(file, opts)
  run(`node --experimental-wasm-stack-switching $(splitext(file)[1]).js`)
  return
end
