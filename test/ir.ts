import { test } from 'uvu'
import * as assert from 'assert'
import { IR, CFG, components, expand, prune, expr, stmt, unreachable, renumber } from '../src/utils/ir'
import { looped, unloop } from '../src/middle/loop'

test('components: acyclic chain', () => {
  const ir = new IR<unknown, unknown, null>(null)
  const b1 = ir.block()
  const b2 = ir.newBlock()
  const b3 = ir.newBlock()
  b1.branch(b2)
  b2.branch(b3)
  b3.unreachable()
  const cfg = new CFG(ir)
  assert.deepStrictEqual(components(cfg), [1, 2, 3])
})

test('components: self-loop singleton', () => {
  const ir = new IR<unknown, unknown, null>(null)
  const b1 = ir.block()
  const b2 = ir.newBlock()
  const b3 = ir.newBlock()
  b1.branch(b2)
  b2.branch(b3)
  b2.branch(b2)
  b3.unreachable()
  const cfg = new CFG(ir)
  assert.deepStrictEqual(components(cfg), [1, [2], 3])
})

test('components: two-block cycle', () => {
  const ir = new IR<unknown, unknown, null>(null)
  const b1 = ir.block()
  const b2 = ir.newBlock()
  const b3 = ir.newBlock()
  b1.branch(b2)
  b2.branch(b3)
  b3.branch(b2)
  const cfg = new CFG(ir)
  assert.deepStrictEqual(components(cfg), [1, [2, 3]])
})

test('expand/prune', () => {
  const ir = new IR<unknown, unknown, null>(null)
  const b1 = ir.block()
  const v = b1.push(stmt(expr('val')))
  b1.branch(2)
  const b2 = ir.newBlock()
  b2.push(stmt(expr('use', v)))
  b2.unreachable()
  const expanded = expand(ir)
  assert.equal(expanded.toString(), `1:
  %1 = val
  %2 = br 2 (%1)
2: (%5)
  %3 = use %5
  %4 = unreachable`)
  const pruned = prune(expand(ir))
  assert.equal(pruned.toString(), `1:
  %1 = val
  %2 = br 2
2:
  %3 = use %1
  %4 = unreachable`)
})

test('looped/unloop', () => {
  const ir = new IR<unknown, unknown, null>(null)
  const b1 = ir.block()
  const input = ir.argument(unreachable)
  b1.branch(2, [input])
  const b2 = ir.newBlock()
  const x = b2.argument(unreachable)
  const cond = b2.push(stmt(expr('check', x)))
  b2.branch(3, [], { when: cond })
  b2.branch(4)
  const b3 = ir.newBlock()
  const next = b3.push(stmt(expr('step', x)))
  b3.branch(2, [next])
  const b4 = ir.newBlock()
  b4.return(x)

  const original = `1: (%1)
  %2 = br 2 (%1)
2: (%3)
  %4 = check %3
  %5 = br 3 if %4
  %6 = br 4
3:
  %7 = step %3
  %8 = br 2 (%7)
4:
  %9 = return %3`
  assert.equal(ir.toString(), original)

  const l = looped(expand(ir))
  assert.equal(l.ir.toString(), `1: (%1)
  %2 = br 2 (%1)
2: (%3)
  %4 = loop %3:
    #1:
      1: (%1)
        %2 = check %1
        %3 = br 3 (%1) if %2
        %4 = br 4 (%1)
      2: (%5)
        %6 = step %5
        %7 = br 2 (%6)
3: (%5)
  %6 = return %5`)

  const back = unloop(l)
  const simplified = prune(back)
  const restored = renumber(simplified)
  assert.equal(restored.toString(), original)
})

test.run()
