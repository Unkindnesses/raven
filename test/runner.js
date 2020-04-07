const fs = require('fs').promises;
const support = require('../js/support')

async function loadWasm(f) {
  buf = await fs.readFile(process.argv[2])
  res = await WebAssembly.instantiate(new Uint8Array(buf));
  return res.instance.exports;
}

loadWasm('test.wasm').then(({_start}) => {
  console.log(_start());
});
