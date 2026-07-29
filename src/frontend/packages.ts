import { Tag, tag } from "./types.js"

export { Loader, Read, join, dirname }

type Read = (path: string) => Promise<string>

function join(...parts: string[]): string {
  const path = parts.filter(Boolean).join('/').replace(/\\/g, '/')
  const segments: string[] = []
  for (const part of path.split('/')) {
    if (part === '' || part === '.') continue
    if (part === '..' && segments.length && segments[segments.length - 1] !== '..') segments.pop()
    else segments.push(part)
  }
  return (path.startsWith('/') ? '/' : '') + segments.join('/')
}

function dirname(path: string): string {
  const i = path.lastIndexOf('/')
  return i === -1 ? '' : path.slice(0, i)
}

function inside(dir: string, path: string): boolean {
  return dir === '' || path.startsWith(`${dir}/`)
}

function relative(dir: string, path: string): string {
  return dir === '' ? path : path.slice(dir.length + 1)
}

// A package is a name and an entry point file, eg `common` is `[..]/common/common.rv`.
// Files alongside the entry point belong to the package, and take their module
// tags from it.

class Loader {
  private entries = new Map<string, string>()

  constructor(readonly read: Read, packages: Record<string, string> = {}) {
    for (const [name, entry] of Object.entries(packages)) this.package(name, entry)
  }

  package(name: string, entry: string): this {
    this.entries.set(name, join(entry))
    return this
  }

  entry(name: string): string {
    const entry = this.entries.get(name)
    if (entry === undefined) throw new Error(`Unknown package "${name}"`)
    return entry
  }

  // `./foo.rv` is relative to the importing file; `foo` names a package.
  resolve(from: string, spec: string): string {
    return spec.endsWith('.rv') ? join(dirname(from), spec) : this.entry(spec)
  }

  // The innermost package a file belongs to.
  private owner(path: string): string {
    let best: string | undefined
    for (const [name, entry] of this.entries) {
      if (path === entry) return name
      if (!inside(dirname(entry), path)) continue
      if (best === undefined || dirname(entry).length > dirname(this.entry(best)).length) best = name
    }
    if (best === undefined) throw new Error(`${path} does not belong to any package`)
    return best
  }

  // Entry points take the package's tag, and other files extend it, eg
  // `common/structures/list.rv` is `tag"common.structures.list"`.
  modtag(path: string): Tag {
    const name = this.owner(path)
    const entry = this.entry(name)
    if (path === entry) return tag(name)
    return new Tag(name, ...relative(dirname(entry), path).replace(/\.rv$/, '').split('/'))
  }
}
