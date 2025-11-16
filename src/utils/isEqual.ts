export { isEqual }

function isEqual(a: unknown, b: unknown): boolean {
  if (a === b) return true
  if (typeof a !== typeof b) return false
  if (a === null || b === null) return false
  if (typeof a !== 'object') return false
  const objA = a as object
  const objB = b as object
  let result: boolean
  if (Array.isArray(objA) || Array.isArray(objB))
    result = arraysEqual(objA, objB)
  else if (objA instanceof Map || objB instanceof Map)
    result = mapsEqual(objA, objB)
  else if (objA instanceof Set || objB instanceof Set)
    result = setsEqual(objA, objB)
  else
    result = objectsEqual(objA, objB)
  return result
}

function arraysEqual(a: unknown, b: unknown): boolean {
  if (!Array.isArray(a) || !Array.isArray(b)) return false
  if (a.length !== b.length) return false
  for (let i = 0; i < a.length; i++)
    if (!isEqual(a[i], b[i]))
      return false
  return true
}

function mapsEqual(a: unknown, b: unknown): boolean {
  if (!(a instanceof Map) || !(b instanceof Map)) return false
  if (a.size !== b.size) return false
  for (const [key, val] of a) {
    if (!b.has(key)) return false
    if (!isEqual(val, b.get(key))) return false
  }
  return true
}

function setsEqual(a: unknown, b: unknown): boolean {
  if (!(a instanceof Set) || !(b instanceof Set)) return false
  if (a.size !== b.size) return false
  for (const val of a)
    if (!b.has(val)) return false
  return true
}

function objectsEqual(a: object, b: object): boolean {
  if (Object.getPrototypeOf(a) !== Object.getPrototypeOf(b)) return false
  const keysA = Reflect.ownKeys(a)
  const keysB = Reflect.ownKeys(b)
  if (keysA.length !== keysB.length) return false
  const keySetB = new Set(keysB)
  for (const key of keysA) {
    if (!keySetB.has(key)) return false
    if (!isEqual((a as any)[key], (b as any)[key])) return false
  }
  return true
}
