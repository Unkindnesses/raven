import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import { createTwoFilesPatch, FILE_HEADERS_ONLY } from 'diff'
import { format } from '../frontend/format.js'

export { fmt, formatDiff }

type Change = [file: string, source: string, formatted: string]

function formatDiff(file: string, source: string, formatted: string): string {
  return createTwoFilesPatch(file, file, source, formatted, '', '', {
    context: 3,
    headerOptions: FILE_HEADERS_ONLY
  })
}

async function walk(dir: string, files: string[]) {
  const entries = await fs.readdir(dir, { withFileTypes: true })
  entries.sort((a, b) => a.name.localeCompare(b.name))

  for (const entry of entries) {
    const file = path.join(dir, entry.name)
    if (entry.isDirectory()) {
      await walk(file, files)
    } else if (entry.isFile() && file.endsWith('.rv')) {
      files.push(file)
    }
  }
}

async function ravenFiles(inputs: string[]): Promise<string[]> {
  const files: string[] = []
  for (const input of inputs) {
    const file = path.resolve(input)
    const stat = await fs.stat(file)
    if (stat.isDirectory()) {
      await walk(file, files)
    } else if (file.endsWith('.rv')) {
      files.push(file)
    }
  }
  return [...new Set(files)].sort()
}

async function* fmt(inputs: string[]): AsyncGenerator<Change> {
  for (const file of await ravenFiles(inputs)) {
    const source = await fs.readFile(file, 'utf8')
    const formatted = format(file, source)
    if (formatted === source) continue
    yield [file, source, formatted]
  }
}
