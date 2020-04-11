const support = joinpath(@__DIR__, "support.js")

jsstring(s) = Base.string('"', escape_string(s), '"')

function emitjs(path, wasm, strings)
  open(path, "w") do io
    println(io, "// This file is auto-generated.\n")
    write(io, Base.read(support))
    println(io)
    println(io, "const wasmFile = '$wasm';")
    println(io, "registerStrings([$(join(jsstring.(strings), ", "))])")
    println(io, "main();")
  end
end

function compile(file)
  path, _ = splitext(file)
  strings = emitwasm(file, "$path.wasm")
  emitjs("$path.js", "$(basename(path)).wasm", strings)
  return
end
