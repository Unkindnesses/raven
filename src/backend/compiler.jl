compile_pipeline(tables) =
  @Pipeline(
    sources = Modules(),
    defs = Definitions(sources),
    interp = Interpreter(defs),
    inferred = Inferred(defs, interp),
    expanded = Expanded(inferred),
    inlined = Inlined(expanded),
    counted = refcounts(inlined),
    wasm = Wasm(defs, counted, tables))

struct Compiler
  pipe::Pipeline
  emitter
end

function Compiler(; emitter = BatchEmitter())
  return Compiler(compile_pipeline(tables(emitter)), emitter) |> loadcommon!
end

function Compiler(src; emitter = BatchEmitter())
  c = Compiler(; emitter)
  reload!(c, src)
  return c
end

Base.show(io::IO, c::Compiler) = print(io, "Compiler(...)")

Base.getindex(c::Compiler, sig) = c.compiled[sig]

frame(inf::Compiler, T) = inf[sig(inf, T)]

withemit(f, p) = dynamic_bind(f, :emit, p)
emit(x::IR) = dynamic_value(:emit, _ -> nothing)(x)

# TODO less backend-dependent
function emit!(c::Pipeline, em, m::RMethod)
  ir = c.counted[(m,)]
  gs = assigned_globals(ir)
  for (b, T) in gs
    c.sources[b] = T
  end
  opcount(ir) > 0 || return
  ir = lowerwasm(ir, c.wasm, tables(em))
  isempty(gs) || reset!(c)
  ir = lowerwasm_globals(ir, c.wasm.globals)
  name = c.wasm.names[(m,)]
  ir = WebAssembly.irfunc(name, ir)
  emit!(em, c.wasm, ir)
end

emit!(c::Pipeline, em, ir::IR) =
  emit!(c, em, RMethod(tag"common.core.main", lowerpattern(AST.List()), ir))

function loadcommon!(c::Pipeline, emitter)
  function emit(ir)
    reset!(c)
    emit!(c, emitter, ir)
  end
  withemit(emit) do
    module!(c.sources, core())
    # TODO toplevel exprs should compile in the context of the relevant module,
    # rather than main. Which would make the following unnecessary.
    import!(module!(c.sources, tag""), module!(c.sources, tag"common"))
    loadmodule(c.sources, tag"common.core", "$common/core.rv")
    loadmodule(c.sources, tag"common", "$common/common.rv")
  end
  return c
end

function loadcommon!(c::Compiler)
  loadcommon!(c.pipe, c.emitter)
  return c
end

function reload!(c::Compiler, src)
  emitter = copy(c.emitter)
  function emit(ir)
    reset!(c.pipe)
    emit!(c.pipe, emitter, ir)
  end
  withemit(emit) do
    reload!(c.pipe.sources, src)
  end
  reset!(c.pipe)
  options().memcheck && emit!(c.pipe, emitter, c.pipe.defs[tag"common.checkAllocations"][1])
  return emitter
end

compiler = nothing

function compile(file, opts = Options();
                 dir = dirname(file),
                 comp = nothing)
  opts == Options() || (comp = Compiler())
  if comp == nothing
    global compiler
    compiler == nothing && (compiler = Compiler())
    comp = compiler
  end
  path = normpath(joinpath(pwd(), file))
  path, _ = splitext(path)
  name = basename(path)
  mkpath(dir)
  withoptions(opts) do
    em = reload!(comp, file)
    strings = emitwasm(em, comp.pipe.wasm, joinpath(dir, "$name.wasm"); path)
    emitjs(joinpath(dir, "$name.js"), "$name.wasm", strings)
  end
  return joinpath(dir, "$name.js")
end

function exec(file, opts = Options(); kw...)
  js = compile(file, opts; kw...)
  run(`node --experimental-wasm-stack-switching $js`)
  return
end
