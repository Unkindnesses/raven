import * as wasm from './wasm'

export { moduleToString, instructionToString }

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
    case 'get_global': return `global.get ${i.id}`
    case 'set_global': return `global.set ${i.id}`
    case 'op': return i.name
    case 'call': return `call $${i.name}`
    case 'convert': return `${i.to}.${i.name}/${i.from}`
    case 'drop': return 'drop'
    case 'select': return 'select'
    case 'branch': return i.cond ? `br_if ${i.level}` : `br ${i.level}`
    case 'return': return 'return'
    case 'unreachable': return 'unreachable'
    case 'call_indirect':
      // TODO table idx
      let s = 'call_indirect'
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

function signatureToString(sig: wasm.Signature): string {
  return varsToString('param', sig.params) + varsToString('result', sig.result)
}

function funcToString(f: wasm.Func, level: number = 1): string {
  let result = '\n' + '  '.repeat(level)
  result += `(func $${f.name}`
  result += signatureToString(f.sig)
  if (f.locals.length > 0) {
    result += '\n' + '  '.repeat(level) + ' '
    result += varsToString('local', f.locals)
  }
  result += bodyToString(f.body.body, f.body.srcs, level + 1)
  result += ')'
  return result
}

function memToString(x: wasm.Mem, level: number): string {
  let result = '\n' + '  '.repeat(level)
  result += `(memory ${x.min})` // TODO: add x.max
  return result
}

function dataToString(x: wasm.Data, level: number): string {
  let result = '\n' + '  '.repeat(level)
  result += `(data (i32.const ${x.offset}) "${new TextDecoder().decode(x.data)}")`
  return result
}

function exportToString(x: wasm.Export, level: number): string {
  let result = '\n' + '  '.repeat(level)
  result += `(export "${x.as}" (func $${x.name}))`
  return result
}

function importToString(x: wasm.Import, level: number): string {
  let result = '\n' + '  '.repeat(level)
  result += `(import "${x.mod}" "${x.name}" (func $${x.as}`
  if (x.sig.kind === 'signature') result += signatureToString(x.sig)
  result += '))'
  return result
}

function globalToString(x: wasm.Global, level: number): string {
  let result = '\n' + '  '.repeat(level)
  result += '(global '
  if (x.mut) {
    result += `(mut ${x.type}) `
  } else {
    result += `${x.type} `
  }
  result += `(${instructionToString(x.init, level)}))`
  return result
}

function moduleToString(m: wasm.Module): string {
  let result = '(module'
  for (const imp of m.imports) result += importToString(imp, 1)
  for (const exp of m.exports) result += exportToString(exp, 1)
  for (const glob of m.globals) result += globalToString(glob, 1)
  for (const mem of m.mems) result += memToString(mem, 1)
  for (const data of m.data) result += dataToString(data, 1)
  for (const func of m.funcs) result += funcToString(func, 1)
  result += '\n)'
  return result
}
