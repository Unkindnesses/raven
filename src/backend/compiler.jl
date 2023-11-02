struct Compiler
  sources::Modules
  defs::Definitions
  final::Cache
  wasm::Wasm
  pipe::Pipeline
  emitter::BatchEmitter
end

function Compiler()
  sources = Modules()
  defs = Definitions(sources)
  inferred = Inferred(defs)
  lowered = lowerir(inferred)
  inlined = Inlined(lowered)
  counted = refcounts(inlined)
  wasm = Wasm(defs, counted)
  pipe = Pipeline([sources, defs, inferred, lowered, inlined, counted, wasm])
  return Compiler(sources, defs, counted, wasm, pipe, BatchEmitter()) |> loadcommon!
end

function Compiler(src)
  c = Compiler()
  reload!(c, src)
  return c
end

Base.getindex(c::Compiler, sig) = c.compiled[sig]

frame(inf::Compiler, T) = inf[sig(inf, T)]

withemit(f, p) = dynamic_bind(f, :emit, p)
emit(x::IR) = dynamic_value(:emit, _ -> nothing)(x)

# TODO less backend-dependent
function emit!(c::Compiler, em::BatchEmitter, m::RMethod)
  name = c.wasm.names[(m,)]
  ir = c.final[(m,)]
  gs = assigned_globals(ir)
  ir = lowerwasm(ir, c.wasm.names, c.wasm.env)
  for (b, T) in gs
    c.sources[b] = T
  end
  isempty(gs) || reset!(c.pipe)
  ir = lowerwasm_globals(ir, c.wasm.env.globals)
  ir = WebAssembly.irfunc(name, ir)
  emit!(em, c.wasm, ir)
  push!(em.main, name)
end

emit!(c::Compiler, em::BatchEmitter, ir::IR) =
  emit!(c::Compiler, em::BatchEmitter,
        RMethod(tag"common.core.main", lowerpattern(AST.List()), ir))

function loadcommon!(c::Compiler)
  main = IR[]
  withemit(ir -> push!(main, ir)) do
    module!(c.sources, core())
    # TODO toplevel exprs should compile in the context of the relevant module,
    # rather than main. Which would make the following unnecessary.
    import!(module!(c.sources, tag""), module!(c.sources, tag"common"))
    loadmodule(c.sources, tag"common.core", "$common/core.rv")
    loadmodule(c.sources, tag"common", "$common/common.rv")
  end
  foreach(ir -> emit!(c, c.emitter, ir), main)
  return c
end

function reload!(c::Compiler, src)
  emitter = copy(c.emitter)
  function emit(ir)
    reset!(c.pipe)
    emit!(c, emitter, ir)
  end
  withemit(emit) do
    reload!(c.sources, src)
  end
  reset!(c.pipe)
  options().memcheck && emit!(c, emitter, c.defs[tag"common.checkAllocations"][1])
  foreach(f -> emit!(emitter, c.wasm, f), c.wasm.env.table)
  return emitter
end

function compile(file, opts = Options();
                 dir = dirname(file),
                 compiler = compiler)
  opts == Options() || (compiler = Compiler())
  path = normpath(joinpath(pwd(), file))
  path, _ = splitext(path)
  name = basename(path)
  mkpath(dir)
  withoptions(opts) do
    em = reload!(compiler, file)
    strings = emitwasm(em, compiler.wasm.env, joinpath(dir, "$name.wasm"); path)
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
