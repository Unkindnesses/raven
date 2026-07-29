import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { spawnSync } from 'node:child_process'
import { test } from 'vitest'
import assert from 'assert'
import { test as rv } from '../src/cli/test.js'

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

    bundle Scaled { Scaled(x) }

    fn quadruple(x) { double(double(x)) }
  `,
  'maths/scale.rv': `
    export { double, Scale }

    bundle Scale { Scale() }

    fn double(x) { x * 2 }
  `,
  'maths/util/measure.rv': `
    export { half, Measure }

    bundle Measure { Measure() }

    fn half(x) { x / 2 }
  `
}

function project(): string {
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

test('cli loads packages into main', () => {
  const dir = project()
  try {
    const out = raven(dir, ['--package', 'maths=maths/maths.rv', 'main.rv'])
    assert.deepStrictEqual(out.split('\n'), [
      "hello from main's directory",
      '12',
      '5.0',
      // Entry points take the package tag; siblings and folders extend it. The
      // module is the same however it's imported, so `Measure` is shared
      // between `maths` and main.
      'maths.Scaled',
      'maths.scale.Scale',
      'maths.util.measure.Measure'
    ])
  } finally {
    fs.rmSync(dir, { recursive: true, force: true })
  }
})

const cliPath = path.join(process.cwd(), 'dist/cli/index.js')

test('circular imports are rejected', () => {
  const dir = project()
  try {
    fs.writeFileSync(path.join(dir, 'maths/util/measure.rv'), `
      import { quadruple } from "../maths.rv"

      export { half, Measure }

      bundle Measure { Measure() }

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
