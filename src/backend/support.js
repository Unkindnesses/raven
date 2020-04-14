const fs = require('fs').promises;

let idcounter = 0;
const references = {};

function createRef(obj) {
  const ref = idcounter++;
  references[ref] = {count: 0, object: obj};
  return ref;
}

function fromRef(ref) {
  return references[ref].object;
}

function incrementRefCount(ref) {
  references[ref].count += 1;
}

function decrementRefCount(ref) {
  references[ref].count -= 1;
  if (references[ref].count == 0) {
    delete references[ref];
  }
}

function registerStrings(ss) {
  for (const s of ss) {
    const ref = createRef(s);
    incrementRefCount(ref);
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
                 incrementRefCount, decrementRefCount,
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
