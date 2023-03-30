const fs = require('fs').promises;

let idcounter = 0;
const table = {};

function createRef(obj) {
  if (idcounter >= 4294967295) throw new Error("too many JSRefs");
  const ref = idcounter++;
  table[ref] = obj;
  return ref;
}

function fromRef(ref) {
  return table[ref];
}

function registerStrings(ss) {
  nStrings = ss.length;
  for (const s of ss) {
    const ref = createRef(s);
  }
}

function release(ref) {
  if (ref >= nStrings) delete table[ref];
}

function global() {
  return createRef(globalThis);
}

function property(obj, prop) {
  const r = fromRef(obj)[fromRef(prop)];
  if (r === undefined) {
    throw new Error(`No such property ${fromRef(prop)}`);
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

function equal(a, b) {
  return fromRef(a) === fromRef(b);
}

function panic(obj) {
  obj = fromRef(obj);
  throw new Error(obj);
}

const support = {global, property, call,
                 createRef, fromRef, panic,
                 equal, release};

async function loadWasm(f) {
  let imports = {support};
  let buf = await fs.readFile(f)
  let res = await WebAssembly.instantiate(new Uint8Array(buf), imports);
  return res.instance.exports;
}

async function main() {
  let {_start} = await loadWasm(__dirname + '/' + wasmFile);
  _start = new WebAssembly.Function(
    {parameters: [], results: ['externref']},
    _start,
    {promising: 'first'}
  );
  try {
    await _start();
  } catch (e) {
    console.error(e);
    process.exit(1);
  }
  if (Object.keys(table).length !== nStrings)
    throw new Error("Memory management fault: JSObject");
}
