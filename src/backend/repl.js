const fs = require('fs').promises;
const net = require('net');
let {loadWasm, registerString, support} = require('./support.js');

let imports = {support, wasm: {}};

async function runWasm(file) {
  try {
    let exports = await loadWasm(file, imports);
    Object.assign(imports.wasm, exports);
    _start = WebAssembly.promising(exports._start);
    await _start();
  } catch (e) {
    console.error(e);
  }
}

function bufferLines(f) {
  let buffer = '';
  return async (data) => {
    buffer += data.toString();
    let lines = buffer.split('\n');
    buffer = lines.pop();
    for (let line of lines) {
      await f(line);
    }
  };
}

async function main() {
  let socket = net.connect(process.argv[2]);
  socket.on('data', bufferLines(async (data) => {
    let command = JSON.parse(data);
    if (command.type === 'string') {
      let id = registerString(command.value);
      socket.write(JSON.stringify({id}));
    } else if (command.type === 'wasm') {
      let module = Buffer.from(command.module, 'base64');
      await runWasm(module);
      socket.write(JSON.stringify({}));
    } else {
      process.stderr.write(`REPL subprocess: unknown command ${JSON.stringify(command)}\n`);
      socket.write(JSON.stringify({}));
    }
  }));
  socket.on('close', () => {
    process.exit(0);
  });
}

main();
