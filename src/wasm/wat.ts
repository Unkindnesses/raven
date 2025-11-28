import * as wasm from './wasm.js'

export { instructionToString }

function basename(p: string): string {
  if (p.length === 0) return ''
  const trimmed = p.replace(/[\\\/]+$/, '')
  if (trimmed.length === 0) return ''
  const parts = trimmed.split(/[\\\/]/)
  return parts[parts.length - 1] ?? ''
}

function lineToString(li: wasm.LineInfo): string {
  const src = li.src[li.src.length - 1][1]
  if (!src) return ''
  const file = basename(src.file)
  let s = ` ;; ${file}:${src.line}:${src.col}`
  if (li.bp) s += ' 🔴'
  return s
}

function instructionToString(i: wasm.Instruction, level: number = 0): string {
  switch (i.kind) {
    case 'nop': return 'nop'
    case 'const': return `${i.type}.const ${i.val}`
    case 'get_local': return `local.get ${i.id}`
    case 'set_local': return i.tee ? `local.tee ${i.id}` : `local.set ${i.id}`
    case 'get_global': return `global.get $${i.id}`
    case 'set_global': return `global.set $${i.id}`
    case 'op': return i.name
    case 'call': return `call $${i.name}`
    case 'convert': return `${i.to}.${i.name}/${i.from}`
    case 'drop': return 'drop'
    case 'select': return 'select'
    case 'branch': return i.cond ? `br_if ${i.level}` : `br ${i.level}`
    case 'return': return 'return'
    case 'unreachable': return 'unreachable'
    case 'call_indirect':
      let s = `call_indirect (table $${i.table})`
      s += varsToString('param', i.sig.params)
      s += varsToString('result', i.sig.result)
      return s
    case 'ref_null':
      return `ref.null ${i.type}`
    case 'block':
    case 'loop':
      return i.kind + bodyToString(i.body, i.srcs, level + 1)
    default:
      let _: never = i
      throw new Error(`unreachable`)
  }
}

function bodyToString(xs: wasm.Instruction[], ss: wasm.LineInfo[], level: number): string {
  let result = ''
  for (let i = 0; i < xs.length; i++) {
    result += '\n' + '  '.repeat(level)
    result += '('
    result += instructionToString(xs[i], level)
    result += ')'
    result += lineToString(ss[i])
  }
  return result
}

function varsToString(name: string, vs: wasm.ValueType[]): string {
  if (vs.length === 0) return ''
  return ` (${name} ${vs.join(' ')})`
}
