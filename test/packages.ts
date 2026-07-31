import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { spawnSync } from 'node:child_process'
import { test } from 'vitest'
import assert from 'assert'
import { test as rv } from '../src/cli/test.js'
import { Loader } from '../src/frontend/packages.js'

test('import from common', async () => {
  await rv(`
    import { map, collect, range } from "common"
    fn twice(x) { x * 2 }
    test collect(map(range(1, 3), twice)) == [2, 4, 6]
  `)
})

test('import an unexported name', async () => {
  await assert.rejects(rv(`
    import { collectStrings } from "common"
  `), /does not export collectStrings/)
})

const files: Record<string, string> = {
  'main.rv': `
    import { quadruple, half, Scaled, Measure } from "maths"
    import { Scale } from "./maths/scale.rv"
    import { greet } from "./greeting.rv"

    println(greet())
    println(quadruple(3))
    println(half(10))
    println(string(tag(Scaled(2))))
    println(string(tag(Scale())))
    println(string(tag(Measure())))
  `,
  'greeting.rv': `
    export { greet }

    fn greet() { "hello from main's directory" }
  `,
  'maths/maths.rv': `
    import { double } from "./scale.rv"
    import { half, Measure } from "./util/measure.rv"

    export { quadruple, half, Measure, Scaled }

    bundle Scaled(x)

    fn quadruple(x) { double(double(x)) }
  `,
  'maths/scale.rv': `
    export { double, Scale }

    bundle Scale()

    fn double(x) { x * 2 }
  `,
  'maths/util/measure.rv': `
    export { half, Measure }

    bundle Measure()

    fn half(x) { x / 2 }
  `
}

function project(files: Record<string, string>): string {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'raven-package-'))
  for (const [name, source] of Object.entries(files)) {
    const file = path.join(dir, name)
    fs.mkdirSync(path.dirname(file), { recursive: true })
    fs.writeFileSync(file, source)
  }
  return dir
}

function raven(dir: string, args: string[]): string {
  const out = spawnSync(process.execPath, ['--enable-source-maps', cliPath, ...args], {
    cwd: dir, encoding: 'utf8'
  })
  assert.strictEqual(out.status, 0, out.stderr || out.stdout)
  return out.stdout.trim()
}

test('loader resolves Windows paths', () => {
  const loader = new Loader(async () => '', {
    '': String.raw`C:\project\main.rv`,
    maths: String.raw`C:\project\maths\maths.rv`
  })
  const scale = loader.resolve(String.raw`C:\project\main.rv`, './maths/scale.rv')
  assert.strictEqual(scale, 'C:/project/maths/scale.rv')
  assert.strictEqual(loader.modtag(scale).path, 'maths.scale')
})

test('cli loads packages into main', () => {
  const dir = project(files)
  try {
    const out = raven(dir, ['--package', 'maths=maths/maths.rv', 'main.rv'])
    assert.deepStrictEqual(out.split('\n'), [
      "hello from main's directory",
      '12',
      '5.0',
      // Entry points take the package tag; siblings and folders extend it. The
      // module is the same however it's imported, so `Measure` is shared
      // between `maths` and main.
      'maths/Scaled',
      'maths.scale/Scale',
      'maths.util.measure/Measure'
    ])
  } finally {
    fs.rmSync(dir, { recursive: true, force: true })
  }
})

const cliPath = path.join(process.cwd(), 'dist/cli/index.js')

// `api.rv` forwards names it never binds itself; `main.rv` takes the lot.
const reexports: Record<string, string> = {
  'main.rv': `
    import { ... } from "./api.rv"

    println(double(3))
    println(half(10))
  `,
  'api.rv': `
    export { ... } from "./scale.rv"
    export { half } from "./measure.rv"
  `,
  'scale.rv': `
    export { double }

    fn double(x) { x * 2 }
  `,
  'measure.rv': `
    export { half, quarter }

    fn half(x) { x / 2 }
    fn quarter(x) { x / 4 }
  `
}

function output(dir: string, main: string): string {
  fs.writeFileSync(path.join(dir, 'main.rv'), main)
  const out = spawnSync(process.execPath, ['--enable-source-maps', cliPath, 'main.rv'], {
    cwd: dir, encoding: 'utf8'
  })
  return out.stdout + out.stderr
}

test('cli re-exports in bulk', () => {
  const dir = project(reexports)
  try {
    assert.deepStrictEqual(raven(dir, ['main.rv']).split('\n'), ['6', '5.0'])
    // `export ... from` forwards only the names it lists ...
    assert.match(output(dir, 'import { quarter } from "./api.rv"'), /does not export quarter/)
    // ... and doesn't bind them in the re-exporting module.
    fs.appendFileSync(path.join(dir, 'api.rv'), '\nprintln(double(1))\n')
    assert.match(output(dir, 'import { ... } from "./api.rv"'), /double is not defined/)
  } finally {
    fs.rmSync(dir, { recursive: true, force: true })
  }
})

test('circular imports are rejected', () => {
  const dir = project(files)
  try {
    fs.writeFileSync(path.join(dir, 'maths/util/measure.rv'), `
      import { quadruple } from "../maths.rv"

      export { half, Measure }

      bundle Measure()

      fn half(x) { x / 2 }
    `)
    const out = spawnSync(process.execPath, [
      '--enable-source-maps', cliPath, '--package', 'maths=maths/maths.rv', 'main.rv'
    ], { cwd: dir, encoding: 'utf8' })
    assert.notStrictEqual(out.status, 0, `expected an error, got:\n${out.stdout}`)
    assert.match(out.stderr, /Circular import: .*maths\.rv -> .*measure\.rv -> .*maths\.rv/)
  } finally {
    fs.rmSync(dir, { recursive: true, force: true })
  }
})
