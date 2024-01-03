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

let await = new WebAssembly.Function(
    {parameters: ['externref', 'i32'], results: ['i32']},
    async ref => createRef(await fromRef(ref)),
    {suspending: "first"}
);

let nStrings = 0;

function registerString(s) {
  nStrings += 1;
  return createRef(s);
}

function release(ref) {
  delete table[ref];
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

function abort(obj) {
  obj = fromRef(obj);
  throw new Error(obj);
}

globalThis.sleep = function (n) {
  return new Promise(resolve => {
      setTimeout(() => { resolve() }, n * 1000)
  });
}

globalThis.readline = function () {
  return new Promise(resolve => {
      process.stdin.once('data', data => {
          process.stdin.pause();
          resolve(data.toString().trim());
      });
  });
}

// TODO: used for testing, remove
globalThis.dummyPromise = function (n) {
  return new Promise(resolve => resolve(n));
}

const support = {global, property, call,
                 createRef, fromRef, abort,
                 equal, release, await};

async function loadWasm(buf, imports = {support}) {
  if (typeof buf === 'string')
    buf = await fs.readFile(buf);
  let res = await WebAssembly.instantiate(new Uint8Array(buf), imports);
  return res.instance.exports;
}

module.exports = {loadWasm, registerString, support};

async function main({memcheck = true} = {}) {
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
  if (memcheck && Object.keys(table).length !== nStrings)
    throw new Error("Memory management fault: JSObject");
}
