struct REPLConn
  proc::Base.Process
  sock::Base.PipeEndpoint
end

Base.close(r::REPLConn) = close(r.sock)

function REPLConn(; stdin = stdin, stdout = stdout, stderr = stderr)
  proc = run(`node --experimental-wasm-stack-switching $(@__DIR__)/repl.js $addr`,
             stdin, stdout, stderr, wait = false)
  sock = accept(server)
  return REPLConn(proc, sock)
end

function command!(r::REPLConn, data)
  write(r.sock, JSON.json(data))
  write(r.sock, '\n')
  JSON.parse(r.sock)
end

registerString(r::REPLConn, s) = command!(r, Dict(:type => "string", :value => s))["id"]

runWasm(r::REPLConn, m::WebAssembly.Module) =
  command!(r, Dict(:type => "wasm", :module => base64(m; path = "repl")))

struct REnv
  conn::REPLConn
  strings::Dict{String,Int32}
  table::Vector{Symbol}
end

REnv(conn::REPLConn) = REnv(conn, Dict(), [])

stringid!(env::REnv, s) =
  get!(() -> registerString(env.conn, s), env.strings, s)

funcid!(env::REnv, f) = tableid!(env.table, f)

struct REPLEmitter
  conn::REPLConn
  emitter::StreamEmitter
end

REPLEmitter(conn::REPLConn) = REPLEmitter(conn, StreamEmitter())

Base.copy(em::REPLEmitter) = REPLEmitter(em.conn, copy(em.emitter))

wenv(em::REPLEmitter) = REnv(em.conn)

emit!(em::REPLEmitter, args...) = emit!(em.emitter, args...)

struct REPL
  conn::REPLConn
  compiler::Compiler
end

function REPL(; stdin = stdin, stdout = stdout, stderr = stderr)
  conn = REPLConn(; stdin, stdout, stderr)
  repl = REPL(conn, Compiler(emitter = REPLEmitter(conn)))
  reload!(repl.compiler, src"") # init imports
  flush!(repl)
  return repl
end

Base.close(r::REPL) = close(r.conn)

function flush!(r::REPL)
  while !isempty(r.compiler.emitter.emitter.queue)
    runWasm(r.conn, popfirst!(r.compiler.emitter.emitter.queue))
  end
end

function toplevels(src; path)
  exs = []
  io = LineNumberingReader(IOBuffer(src))
  while true
    Parse.skip(io)
    cur = cursor(io)
    (ex = parse(io; path)) == nothing && break
    push!(exs, (cur, ex))
  end
  return exs
end

function wrap_print(ex)
  ex isa AST.Syntax && return ex
  return AST.Call(:println, ex)
end

function eval!(r::REPL, src)
  function emit(ir)
    reset!(r.compiler.pipe)
    emit!(r.compiler, r.compiler.emitter, ir)
    flush!(r)
  end
  withemit(emit) do
    defs = r.compiler.pipe.sources
    cx = LoadState(defs, defs[tag""])
    exs = toplevels(src; path = "repl")
    for (i, (cur, ex)) in enumerate(exs)
      ex = wrap_print(ex)
      vload(cx, ex, src = Source("repl", cur.line, cur.column))
    end
  end
  return
end
