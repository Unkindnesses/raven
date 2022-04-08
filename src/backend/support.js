const fs = require('fs').promises;

let idcounter = 0;
const table = {};

function createRef(obj) {
  const ref = idcounter++;
  table[ref] = obj;
  return ref;
}

function fromRef(ref) {
  return table[ref];
}

function registerStrings(ss) {
  for (const s of ss) {
    const ref = createRef(s);
  }
}

function global() {
  return createRef(globalThis);
}

function property(obj, prop) {
  const r = fromRef(obj)[fromRef(prop)];
  if (r === undefined) {
    throw new Error(`No such property ${prop}`);
  }
  return createRef(r);
}

function call(obj, meth, ...args) {
  obj = fromRef(obj);
  meth = fromRef(meth);
  args = args.map(fromRef);
  const func = obj[meth];
  if (func === undefined) {
    throw new Error(`No such method ${meth}`);
  }
  return createRef(obj[meth].call(obj, ...args));
}

function panic(obj) {
  obj = fromRef(obj);
  throw new Error(obj);
}

const support = {global, property, call,
                 createRef, fromRef, panic};

async function loadWasm(f) {
  let imports = {support};
  let buf = await fs.readFile(f)
  let res = await WebAssembly.instantiate(new Uint8Array(buf), imports);
  return res.instance.exports;
}

async function main() {
  let {_start} = await loadWasm(__dirname + '/' + wasmFile);
  try {
    _start();
  } catch (e) {
    console.error(e);
    process.exit(1);
  }
}
