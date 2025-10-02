export { binding, Options, withOptions, options }

// Simple dynamic binding for recursive types

function binding<V>(name: string, fallback?: V) {
  const stack: V[] = []
  function withValue<T>(value: V, f: () => T): T
  function withValue<T>(value: V, f: () => Promise<T>): Promise<T>
  function withValue<T>(value: V, f: () => T | Promise<T>): T | Promise<T> {
    stack.push(value)
    let released = false
    const release = () => {
      if (released) return
      released = true
      stack.pop()
    }
    try {
      const result = f()
      if (result instanceof Promise) return result.finally(release)
      release()
      return result
    } catch (err) {
      release()
      throw err
    }
  }
  const getValue = (d = fallback): V => {
    if (stack.length === 0) {
      if (d !== undefined) return d
      throw new Error(`${name} is not set`)
    }
    return stack[stack.length - 1]
  }
  return [withValue, getValue] as const
}

// Compiler options

interface Options {
  memcheck: boolean // checkAllocations() call after main
  jspanic: boolean  // Use JS interop for error handling
  jsalloc: boolean  // JS interop uses malloc/refcounting
  inline: boolean   // Allow inlining
}

function defaults(): Options {
  return { memcheck: true, jspanic: true, jsalloc: true, inline: true }
}

const [_withOptions, options] = binding('options', defaults())

const withOptions = <T>(opts: Partial<Options>, f: () => T): T => {
  return _withOptions({ ...defaults(), ...opts }, f)
}
