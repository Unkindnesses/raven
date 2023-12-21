struct Compiler
  pipe::Pipeline
  emitter
end

function Compiler(; emitter = BatchEmitter())
  pipe = @Pipeline(
    sources = Modules(),
    defs = Definitions(sources),
    inferred = Inferred(defs),
    expanded = Expanded(inferred),
    inlined = Inlined(expanded),
    counted = refcounts(inlined),
    wasm = Wasm(defs, counted, wenv(emitter)),
  )
  return Compiler(pipe, emitter) |> loadcommon!
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
function emit!(c::Compiler, em, m::RMethod)
  ir = c.pipe.counted[(m,)]
  gs = assigned_globals(ir)
  for (b, T) in gs
    c.pipe.sources[b] = T
  end
  opcount(ir) > 0 || return
  ir = lowerwasm(ir, c.pipe.wasm)
  isempty(gs) || reset!(c.pipe)
  ir = lowerwasm_globals(ir, c.pipe.wasm.globals)
  name = c.pipe.wasm.names[(m,)]
  ir = WebAssembly.irfunc(name, ir)
  emit!(em, c.pipe.wasm, ir)
end

emit!(c::Compiler, em, ir::IR) =
  emit!(c::Compiler, em,
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
    strings = emitwasm(em, compiler.pipe.wasm, joinpath(dir, "$name.wasm"); path)
    emitjs(joinpath(dir, "$name.js"), "$name.wasm", strings)
  end
  return joinpath(dir, "$name.js")
end

function exec(file, opts = Options(); kw...)
  js = compile(file, opts; kw...)
  run(`node --experimental-wasm-stack-switching $js`)
  return
end
