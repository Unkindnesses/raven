import { test } from 'uvu'
import * as assert from 'assert'
import { leb128U, leb128S, Def } from '../src/dwarf'
import * as wasm from '../src/wasm/wasm'
import { Locals, stackshuffle } from '../src/wasm/ir'
import { HashSet } from '../src/utils/map'

test('leb128 unsigned', () => {
  assert.deepEqual(leb128U(0), [0x00])
  assert.deepEqual(leb128U(1), [0x01])
  assert.deepEqual(leb128U(624485), [0xE5, 0x8E, 0x26])
  assert.deepEqual(leb128U(0xffffffff), [0xff, 0xff, 0xff, 0xff, 0x0f])
})

test('leb128 signed', () => {
  assert.deepEqual(leb128S(-1), [0x7f])
  assert.deepEqual(leb128S(-123456), [0xC0, 0xBB, 0x78])
})

test('stack shuffling', () => {
  let state = Locals([6, 3, 2])
  let target = Locals([3, 4], new HashSet([3]))
  assert.deepEqual(stackshuffle(state, target)[0], [{ kind: 'drop' }, { kind: 'tee', x: 3 }, { kind: 'get', x: 4 }])

  state = Locals([3])
  target = Locals([3, 3])
  assert.deepEqual(stackshuffle(state, target)[0], [{ kind: 'tee', x: 3 }, { kind: 'get', x: 3 }])

  state = Locals([1, 2])
  target = Locals([] as number[])
  assert.deepEqual(stackshuffle(state, target)[0], [])
})

import { binary } from '../src/wasm/binary'
import { spawnSync } from 'node:child_process'
import * as fs from 'node:fs'
import * as path from 'node:path'

function compiled_wat(m: wasm.Module): string {
  const dir = path.join(process.cwd(), 'test', `.wasm-tmp-${Date.now()}-${Math.random().toString(36).slice(2)}`)
  fs.mkdirSync(dir, { recursive: true })
  const tmp = path.join(dir, 'mod.wasm')
  fs.writeFileSync(tmp, binary(m))
  const res = spawnSync('wasm-tools', ['print', tmp], { encoding: 'utf-8' })
  fs.rmSync(dir, { recursive: true, force: true })
  if (res.status !== 0) throw new Error(res.stderr || 'wasm-tools not available')
  return res.stdout
}

function Block(instrs: wasm.Instruction[]): wasm.Block {
  return wasm.Block(instrs, instrs.map(_ => wasm.LineInfo([[Def('test'), undefined]])))
}

test('binary', () => {
  let m = wasm.Module({ mems: [wasm.Mem(0)] })
  assert.ok(compiled_wat(m).includes('(memory'))

  m = wasm.Module({ globals: [wasm.Global(wasm.i64)] })
  assert.ok(compiled_wat(m).includes('(global (;0;) (mut i64) i64.const 0)'))

  m = wasm.Module({ funcs: [wasm.Func('add', wasm.Signature([], [wasm.i32]), [], Block([wasm.Const(wasm.i32, 5n)]), Def('add'))] })
  let s = compiled_wat(m)
  assert.ok(s.includes('func $add (;0;) (type 0) (result i32)'))
  assert.ok(s.includes('i32.const 5'))

  m = wasm.Module({ funcs: [wasm.Func('add', wasm.Signature([], [wasm.f64]), [], Block([wasm.Const(wasm.f64, 1.0)]), Def('add'))] })
  assert.ok(compiled_wat(m).includes('f64.const 0x1p+0'))

  m = wasm.Module({ imports: [wasm.Import('support', 'global', 'jsglobal', wasm.Signature([wasm.f32], [wasm.i32]))] })
  s = compiled_wat(m)
  assert.ok(s.includes('import "support" "global"'))
  assert.ok(s.includes('(param f32) (result i32)'))

  m = wasm.Module({ exports: [wasm.Export('add', 'wasmAdd')], funcs: [wasm.Func('add', wasm.Signature([wasm.i32], [wasm.i32]), [], Block([wasm.Const(wasm.i32, 5n)]), Def('add'))] })
  assert.ok(compiled_wat(m).includes('(export "wasmAdd" (func $add))'))
})

test.run()
