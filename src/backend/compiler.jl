struct Compiler
  pipe::Pipeline
  emitter::BatchEmitter
end

function Compiler()
  pipe = @Pipeline(
    sources = Modules(),
    defs = Definitions(sources),
    inferred = Inferred(defs),
    expanded = Expanded(inferred),
    inlined = Inlined(expanded),
    counted = refcounts(inlined),
    wasm = Wasm(defs, counted),
  )
  return Compiler(pipe, BatchEmitter()) |> loadcommon!
end

function Compiler(src)
  c = Compiler()
  reload!(c, src)
  return c
end

Base.show(io::IO, c::Compiler) = print(io, "Compiler(...)")

Base.getindex(c::Compiler, sig) = c.compiled[sig]

frame(inf::Compiler, T) = inf[sig(inf, T)]

withemit(f, p) = dynamic_bind(f, :emit, p)
emit(x::IR) = dynamic_value(:emit, _ -> nothing)(x)

# TODO less backend-dependent
function emit!(c::Compiler, em::BatchEmitter, m::RMethod)
  ir = c.pipe.counted[(m,)]
  gs = assigned_globals(ir)
  for (b, T) in gs
    c.pipe.sources[b] = T
  end
  opcount(ir) > 0 || return
  ir = lowerwasm(ir, c.pipe.wasm.names, c.pipe.wasm.env)
  isempty(gs) || reset!(c.pipe)
  ir = lowerwasm_globals(ir, c.pipe.wasm.env.globals)
  name = c.pipe.wasm.names[(m,)]
  ir = WebAssembly.irfunc(name, ir)
  emit!(em, c.pipe.wasm, ir)
  push!(em.main, name)
end

emit!(c::Compiler, em::BatchEmitter, ir::IR) =
  emit!(c::Compiler, em::BatchEmitter,
        RMethod(tag"common.core.main", lowerpattern(AST.List()), ir))

function loadcommon!(c::Compiler)
  function emit(ir)
    reset!(c.pipe)
    emit!(c, c.emitter, ir)
  end
  withemit(emit) do
    module!(c.pipe.sources, core())
    # TODO toplevel exprs should compile in the context of the relevant module,
    # rather than main. Which would make the following unnecessary.
    import!(module!(c.pipe.sources, tag""), module!(c.pipe.sources, tag"common"))
    loadmodule(c.pipe.sources, tag"common.core", "$common/core.rv")
    loadmodule(c.pipe.sources, tag"common", "$common/common.rv")
  end
  return c
end

function reload!(c::Compiler, src)
  emitter = copy(c.emitter)
  function emit(ir)
    reset!(c.pipe)
    emit!(c, emitter, ir)
  end
  withemit(emit) do
    reload!(c.pipe.sources, src)
  end
  reset!(c.pipe)
  options().memcheck && emit!(c, emitter, c.pipe.defs[tag"common.checkAllocations"][1])
  foreach(f -> emit!(emitter, c.pipe.wasm, f), c.pipe.wasm.env.table)
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
    strings = emitwasm(em, compiler.pipe.wasm.env, joinpath(dir, "$name.wasm"); path)
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
