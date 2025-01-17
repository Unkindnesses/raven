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

async function await(ref) {
  return createRef(await fromRef(ref));
}

function string(i) {
  return createRef(support.strings[i]);
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

async function errcall(obj, meth, ...args) {
  obj = fromRef(obj);
  meth = fromRef(meth);
  args = args.map(fromRef);
  const func = obj[meth];
  if (func === undefined) {
    throw new Error(`No such method ${meth}`);
  }
  try {
    result = await obj[meth].call(obj, ...args);
    return [0, createRef(result)];
  } catch (e) {
    return [1, createRef(e)];
  }
}

function equal(a, b) {
  return fromRef(a) === fromRef(b);
}

function abort(obj, cause) {
  throw new Error(fromRef(obj), {cause: fromRef(cause)});
}

globalThis.sleep = function (n) {
  return new Promise(resolve => {
      setTimeout(() => { resolve() }, n * 1000)
  });
}

globalThis.readline = function () {
  process.stdin.resume();
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

globalThis.dummyErr = function () {
  throw new Error('dummy error');
}

const support = {global, property, call, errcall,
                 string, strings: [],
                 createRef, fromRef, abort,
                 equal, release,
                 await: new WebAssembly.Suspending(await),
                 errcall: new WebAssembly.Suspending(errcall)};

async function loadWasm(buf, imports = {support}) {
  if (typeof buf === 'string')
    buf = await fs.readFile(buf);
  let res = await WebAssembly.instantiate(new Uint8Array(buf), imports);
  return res.instance.exports;
}

module.exports = {loadWasm, support};

async function main({memcheck = true} = {}) {
  let {_start} = await loadWasm(__dirname + '/' + wasmFile);
  _start = WebAssembly.promising(_start);
  try {
    await _start();
  } catch (e) {
    console.error(e);
    process.exit(1);
  }
  if (memcheck && Object.keys(table).length !== 0)
    throw new Error("Memory management fault: JSObject");
}
