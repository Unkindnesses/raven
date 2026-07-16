import * as ast from './ast.js'
import { parse } from './parse.js'

export { format, trailingWhitespace, commas, brackets, indentTree }

function setLeading(tree: ast.Tree, leading: string) {
  tree.trivia ??= { leading: '', trailing: '', inner: '' }
  tree.trivia.leading = leading
}

function setTrailing(tree: ast.Tree, trailing: string) {
  tree.trivia ??= { leading: '', trailing: '', inner: '' }
  tree.trivia.trailing = trailing
}

function setInner(tree: ast.Tree, inner: string) {
  tree.trivia ??= { leading: '', trailing: '', inner: '' }
  tree.trivia.inner = inner
}

// Final newline

function lineEnding(source: string): string {
  return source.match(/\r?\n/g)?.at(-1) ?? '\n'
}

function endWith(text: string, newline: string): string {
  return text.replace(/(?:\r?\n[ \t,]*)+$/, '') + newline
}

function ensureFinalNewline(file: ast.Expr, newline: string): ast.Expr {
  const last = file.args.at(-1)
  const inner = file.trivia?.inner ?? ''
  if (/\S/.test(inner)) {
    file.trivia!.inner = endWith(inner, newline)
  } else if (last) {
    if (file.trivia) file.trivia.inner = ''
    setTrailing(last, endWith(last.trivia?.trailing ?? '', newline))
  } else if (file.trivia) file.trivia.inner = ''
  return file
}

// Trailing whitespace

function trim(text: string): string {
  return text.replace(/[ \t,]+(?=\r?\n)/g, '')
}

function trailingWhitespace(tree: ast.Tree): ast.Tree {
  const out = tree.map(trailingWhitespace)
  if (tree.trivia) out.trivia = {
    leading: trim(out.trivia!.leading),
    trailing: trim(out.trivia!.trailing),
    inner: trim(out.trivia!.inner)
  }
  return out
}

// Commas

function bracketStart(tree: ast.Expr): number | undefined {
  if (['Group', 'List', 'Block', 'File'].includes(tree.head)) return 0
  if (['Call', 'Index'].includes(tree.head)) return 1
}

function commas(tree: ast.Tree): ast.Tree {
  const out = tree.map(commas)
  if (out instanceof ast.Token) return out
  const start = bracketStart(out)
  if (start === undefined) return out
  const items = out.args.slice(start)
  const spaceFirst = start === 0
  items.forEach((item, i) => {
    // Strip trailing commas
    const trivia = item.trivia
    if (trivia && (i === items.length - 1 || trivia.trailing.includes('\n')))
      trivia.trailing = trivia.trailing.replace(/(?<=^[^#]*),/g, '')
    // Inline commas
    const previous = items[i - 1]
    const prefix = (previous?.trivia?.trailing ?? '') + (item.trivia?.leading ?? '')
    if (prefix.includes('\n')) return
    if (previous) {
      setTrailing(previous, ',')
      setLeading(item, ' ')
    } else setLeading(item, spaceFirst && prefix.length > 0 ? ' ' : '')
  })
  return out
}

// Trailing brackets

function brackets(tree: ast.Tree): ast.Tree {
  const out = tree.map(brackets)
  if (out instanceof ast.Token || out.head === 'File') return out
  const start = bracketStart(out)
  if (start === undefined) return out
  const [first, last] = [out.args[start], out.args.at(-1)]
  if (!first || !last) return out
  const openNewline = (first.trivia?.leading ?? '').match(/\r?\n/)?.[0]
  const closeNewline = ((last.trivia?.trailing ?? '') + (out.trivia?.inner ?? '')).match(/\r?\n/)?.[0]
  if (openNewline && !closeNewline)
    setTrailing(last, (last.trivia?.trailing ?? '') + openNewline)
  else if (closeNewline && !openNewline)
    ast.lead(first, closeNewline)
  return out
}

// Indentation

function indentTrivia(text: string, lineStart: boolean, depth: number, closeDepth = depth): string {
  const prefix = lineStart ? '\n' : ''
  text = (prefix + text).replace(/\n[ \t,]*(?=#)/g, `\n${' '.repeat(depth)}`)
  text = text.replace(/\n[ \t,]*$/, `\n${' '.repeat(closeDepth)}`)
  return text.slice(prefix.length)
}

function indentStatements(statements: ast.Tree[], depth: number, lineStart: boolean): ast.Tree[] {
  return statements.map(statement => {
    const out = indentTree(statement, depth)
    setLeading(out, indentTrivia(out.trivia?.leading ?? '', lineStart, depth))
    lineStart = statement.trivia?.trailing.endsWith('\n') ?? false
    return out
  })
}

function indentTree(tree: ast.Tree, depth: number = 0): ast.Tree {
  if (tree instanceof ast.Token) return tree.clone()
  let args: ast.Tree[]
  let lineStart = false
  const start = bracketStart(tree)
  if (tree.head === 'File') {
    args = indentStatements(tree.args, depth, true)
    lineStart = tree.args.at(-1)?.trivia?.trailing.endsWith('\n') ?? true
  } else if (start === undefined) {
    args = tree.args.map(arg => indentTree(arg, depth))
  } else {
    const before = tree.args.slice(0, start).map(arg => indentTree(arg, depth))
    const statements = tree.args.slice(start)
    depth += 2
    lineStart = /^[^#]*\n/.test(statements[0]?.trivia?.leading ?? '')
    args = [...before, ...indentStatements(statements, depth, lineStart)]
    lineStart = statements.at(-1)?.trivia?.trailing.endsWith('\n') ?? false
  }
  const out = new ast.Expr(tree.head, args, tree.meta)
  if (tree.trivia) out.trivia = { ...tree.trivia }
  setInner(out, indentTrivia(out.trivia?.inner ?? '', lineStart, depth, Math.max(depth - 2, 0)))
  return out
}

// Combined pass

function format(path: string, source: string): string {
  let file: ast.Tree = parse(path, source)
  file = commas(file)
  file = brackets(file)
  file = indentTree(file)
  file = trailingWhitespace(file)
  file = ensureFinalNewline(ast.asExpr(file), lineEnding(source))
  return ast.print(file)
}
