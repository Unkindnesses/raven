function jsmain(wasm)
  """
  const fs = require('fs').promises;

  async function loadWasm(f) {
    let imports = {support};
    let buf = await fs.readFile(f)
    let res = await WebAssembly.instantiate(new Uint8Array(buf), imports);
    return res.instance.exports;
  }

  async function main() {
    let {_start} = await loadWasm(__dirname + '/$(wasm)');
    console.log(_start());
  }

  main();
  """
end

const support = joinpath(@__DIR__, "support.js")

function emitjs(path, wasm)
  open(path, "w") do io
    println(io, "// This file is auto-generated.\n")
    write(io, Base.read(support))
    println(io)
    write(io, jsmain(wasm))
  end
end

function compile(file)
  path, _ = splitext(file)
  emitwasm(file, "$path.wasm")
  emitjs("$path.js", "$(basename(path)).wasm")
  return
end
