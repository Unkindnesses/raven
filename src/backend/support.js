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
  obj = fromRef(obj);
  prop = fromRef(prop);
  if (!(prop in obj)) { throw `No property ${prop} found.`; }
  return createRef(obj[prop]);
}

function call(f, ...args) {
  f = fromRef(f);
  args = args.map(fromRef);
  return createRef(f(...args));
}

const support = {global, property, call,
                 incrementRefCount, decrementRefCount,
                 createRef, fromRef};

async function loadWasm(f) {
  let imports = {support};
  let buf = await fs.readFile(f)
  let res = await WebAssembly.instantiate(new Uint8Array(buf), imports);
  return res.instance.exports;
}

async function main() {
  let {_start} = await loadWasm(__dirname + '/' + wasmFile);
  console.log(_start());
}
