import * as ast from './ast.js'
import { parse } from './parse.js'

export { format, trailingWhitespace, commas, brackets, indentTree }

// Final newline

function lineEnding(source: string): string {
  return source.match(/\r?\n/g)?.at(-1) ?? '\n'
}

function endWith(text: string, newline: string): string {
  return text.replace(/(?:\r?\n[ \t,]*)+$/, '') + newline
}

function ensureFinalNewline(file: ast.Expr, newline: string): ast.Expr {
  const last = file.args.at(-1)
  const inner = file.trivia.inner
  if (/\S/.test(inner)) return ast.inner(file, endWith(inner, newline))
  if (!last) return ast.inner(file, '')
  const out = file.map((arg, i) => i === file.args.length - 1
    ? ast.trailing(arg, endWith(arg.trivia.trailing, newline))
    : arg)
  return ast.inner(out, '')
}

// Trailing whitespace

function trim(text: string): string {
  return text.replace(/[ \t,]+(?=\r?\n)/g, '')
}

function trailingWhitespace(tree: ast.Tree): ast.Tree {
  const out = tree.map(trailingWhitespace)
  return out.withtrivia({
    leading: trim(out.trivia.leading),
    trailing: trim(out.trivia.trailing),
    inner: trim(out.trivia.inner)
  })
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
  const args = [...out.args]
  const items = args.slice(start)
  const spaceFirst = start === 0
  items.forEach((item, i) => {
    // Strip trailing commas
    if (i === items.length - 1 || item.trivia.trailing.includes('\n'))
      item = ast.trailing(item, item.trivia.trailing.replace(/(?<=^[^#]*),/g, ''))
    // Inline commas
    const previous = items[i - 1]
    const prefix = (previous?.trivia.trailing ?? '') + item.trivia.leading
    if (prefix.includes('\n')) { items[i] = item; return }
    if (previous) {
      items[i - 1] = ast.trailing(previous, ',')
      item = ast.leading(item, ' ')
    } else item = ast.leading(item, spaceFirst && prefix.length > 0 ? ' ' : '')
    items[i] = item
  })
  args.splice(start, items.length, ...items)
  return new ast.Expr(out.head, args, out.meta, out.trivia)
}

// Trailing brackets

function brackets(tree: ast.Tree): ast.Tree {
  const out = tree.map(brackets)
  if (out instanceof ast.Token || out.head === 'File') return out
  const start = bracketStart(out)
  if (start === undefined) return out
  const [first, last] = [out.args[start], out.args.at(-1)]
  if (!first || !last) return out
  const openNewline = first.trivia.leading.match(/\r?\n/)?.[0]
  const closeNewline = (last.trivia.trailing + out.trivia.inner).match(/\r?\n/)?.[0]
  if (openNewline && !closeNewline)
    return out.map((arg, i) => i === out.args.length - 1 ? ast.trail(arg, openNewline) : arg)
  if (closeNewline && !openNewline)
    return out.map((arg, i) => i === start ? ast.leading(arg, closeNewline) : arg)
  return out
}

// Indentation

function indentTrivia(text: string, lineStart: boolean, depth: number, closeDepth = depth): string {
  const prefix = lineStart ? '\n' : ''
  text = (prefix + text).replace(/\n[ \t,]*(?=#)/g, `\n${' '.repeat(depth)}`)
  text = text.replace(/\n[ \t,]*$/, `\n${' '.repeat(closeDepth)}`)
  return text.slice(prefix.length)
}

function indentStatements(statements: readonly ast.Tree[], depth: number, lineStart: boolean): ast.Tree[] {
  return statements.map(statement => {
    const out = ast.leading(indentTree(statement, depth), indentTrivia(statement.trivia.leading, lineStart, depth))
    lineStart = statement.trivia.trailing.endsWith('\n')
    return out
  })
}

function indentTree(tree: ast.Tree, depth: number = 0): ast.Tree {
  if (tree instanceof ast.Token) return tree
  let args: ast.Tree[]
  let lineStart = false
  const start = bracketStart(tree)
  if (tree.head === 'File') {
    args = indentStatements(tree.args, depth, true)
    lineStart = tree.args.at(-1)?.trivia.trailing.endsWith('\n') ?? true
  } else if (start === undefined) {
    args = tree.args.map(arg => indentTree(arg, depth))
  } else {
    const before = tree.args.slice(0, start).map(arg => indentTree(arg, depth))
    const statements = tree.args.slice(start)
    depth += 2
    lineStart = /^[^#]*\n/.test(statements[0]?.trivia.leading ?? '')
    args = [...before, ...indentStatements(statements, depth, lineStart)]
    lineStart = statements.at(-1)?.trivia.trailing.endsWith('\n') ?? false
  }
  const out = new ast.Expr(tree.head, args, tree.meta, tree.trivia)
  return ast.inner(out, indentTrivia(tree.trivia.inner, lineStart, depth, Math.max(depth - 2, 0)))
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
