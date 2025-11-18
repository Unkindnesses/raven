#!/usr/bin/env node
import { promises as fs } from 'fs'
import path from 'path'
import { fileURLToPath } from 'url'

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')
const commonDir = path.join(root, 'common')
const outFile = path.join(root, 'src/common.json')

async function collect(dir) {
  const entries = await fs.readdir(dir, { withFileTypes: true })
  const tree = {}
  for (const entry of entries) {
    const full = path.join(dir, entry.name)
    if (entry.isDirectory()) {
      const contents = await collect(full)
      if (Object.keys(contents).length) tree[entry.name] = contents
    } else if (entry.isFile() && entry.name.endsWith('.rv')) {
      tree[entry.name] = await fs.readFile(full, 'utf8')
    }
  }
  return tree
}

const tree = await collect(commonDir)
const body = `${JSON.stringify(tree, null, 2)}\n`
await fs.mkdir(path.dirname(outFile), { recursive: true })
await fs.writeFile(outFile, body)
