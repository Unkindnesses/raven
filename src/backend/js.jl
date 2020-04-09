const support = joinpath(@__DIR__, "support.js")

function emitjs(path, wasm)
  open(path, "w") do io
    println(io, "// This file is auto-generated.\n")
    write(io, Base.read(support))
    println(io)
    println(io, "const wasmFile = '$wasm';")
    println(io, "main();")
  end
end

function compile(file)
  path, _ = splitext(file)
  emitwasm(file, "$path.wasm")
  emitjs("$path.js", "$(basename(path)).wasm")
  return
end
