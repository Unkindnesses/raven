import * as ast from './ast.js'
import { parse } from './parse.js'

export { format, trailingWhitespace, comments, commas, operators, brackets, indent }

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

// Comments

function comment(text: string): string {
  return text.replace(/^[ \t]*(?=#)/, ' ')
}

function comments(tree: ast.Tree): ast.Tree {
  const out = ast.trailing(tree.map(comments), comment(tree.trivia.trailing))
  if (out instanceof ast.Token || out.head === 'File') return out
  const start = bracketStart(out)
  if (start === undefined) return out
  const first = out.args[start]
  if (!first) return ast.inner(out, comment(out.trivia.inner))
  return out.map((arg, i) => i === start ? ast.leading(arg, comment(arg.trivia.leading)) : arg)
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
  const allowSpace = start === 0
  if (items.length === 0) // collapse empty lists without newlines
    return /[\n#]/.test(out.trivia.inner) ? out : ast.inner(out, '')
  items.forEach((item, i) => {
    // strip trailing commas
    if (i === items.length - 1 || item.trivia.trailing.includes('\n'))
      item = ast.trailing(item, item.trivia.trailing.replace(/(?<=^[^#]*),/g, ''))
    // at most one space after opener
    const previous = items[i - 1]
    const prefix = (previous?.trivia.trailing ?? '') + item.trivia.leading
    if (prefix.includes('\n')) { items[i] = item; return }
    if (!previous) item = ast.leading(item, allowSpace && prefix.length > 0 ? ' ' : '')
    else { // inline commas
      items[i - 1] = ast.trailing(previous, ',')
      item = ast.leading(item, ' ')
    }
    items[i] = item
  })
  const [first, last] = [items[0], items.at(-1)!]
  if (!first.trivia.leading.includes('\n') && !last.trivia.trailing.includes('\n'))
    items[items.length - 1] = ast.trailing(last, first.trivia.leading) // mirror opening space at close
  args.splice(start, items.length, ...items)
  return new ast.Expr(out.head, args, out.meta, out.trivia)
}

// Operators

function operators(tree: ast.Tree): ast.Tree {
  const out = tree.map(operators)
  if (!ast.isExpr(out, 'Operator') || out.args.length !== 3) return out
  const operator = out.args[1]
  const before = ast.symbol(':').isEqual(operator.unwrap()) ? '' : ' '
  return out.map((arg, i) => {
    if (i === 0) return ast.trailing(arg, before)
    if (!operator.trivia.trailing.includes('\n')) {
      if (i === 1) return ast.trailing(arg, ' ')
      return ast.leading(arg, '')
    }
    return arg
  })
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

function indentTraverse(tree: ast.Traverse, depth: number): ast.Tree {
  const node = tree.node
  if (node instanceof ast.Token) return node
  let lineStart = false
  let itemDepth = depth
  const operatorStart = ast.isExpr(node, 'Operator') && node.args.length === 3 &&
    node.args[1].trivia.trailing.includes('\n') ? 2 : undefined
  const start = operatorStart ?? bracketStart(node)
  if (node.head === 'File') lineStart = true
  else if (start !== undefined) itemDepth += 2
  const out = tree.map((child, index) => {
    if (start === undefined || index < start) return indentTraverse(child, depth)
    if (index === start && node.head !== 'File') {
      lineStart = operatorStart !== undefined || /^[^#]*\n/.test(child.trivia.leading)
      if (operatorStart === undefined && !child.trivia.leading.includes('\n')) itemDepth = child.loc.column - 1
    }
    child = child.replace(ast.leading(child.node, indentTrivia(child.trivia.leading, lineStart, itemDepth)))
    const out = indentTraverse(child, itemDepth)
    lineStart = out.trivia.trailing.endsWith('\n')
    return out
  })
  return ast.inner(out, indentTrivia(tree.trivia.inner, lineStart, itemDepth, depth))
}

function indent(tree: ast.Tree, depth: number = 0): ast.Tree {
  return indentTraverse(new ast.Traverse(tree), depth)
}

// Combined pass

function format(path: string, source: string): string {
  let file: ast.Tree = parse(path, source)
  file = commas(file)
  file = operators(file)
  file = brackets(file)
  file = trailingWhitespace(file)
  file = comments(file)
  file = indent(file)
  file = ensureFinalNewline(ast.asExpr(file), lineEnding(source))
  return ast.print(file)
}
