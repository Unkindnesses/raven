let idcounter = 0;
const references = {};

function createRef(obj) {
  const ref = idcounter++;
  references[ref] = {count: 0, object: obj};
  return ref;
}

function objectFromRef(ref) {
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

function global() {
  return createRef(globalThis);
}

const support = {global, incrementRefCount, decrementRefCount};
