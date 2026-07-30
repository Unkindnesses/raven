import * as process from 'process'
import { fileURLToPath, pathToFileURL } from 'url'
import * as lsp from 'vscode-languageserver/node'
import * as ast from '../frontend/ast.js'
import { parse } from '../frontend/parse.js'
import { format } from '../frontend/format.js'
import { dirname, join } from '../frontend/packages.js'

export { startLsp }

// Formatting

function endPosition(source: string): lsp.Position {
  const lines = source.split(/\r?\n/)
  return lsp.Position.create(lines.length - 1, lines.at(-1)!.length)
}

function formatEdits(path: string, source: string): lsp.TextEdit[] {
  let formatted: string
  try {
    formatted = format(path, source)
  } catch {
    return [] // TODO fault-tolerant parsing
  }
  if (formatted === source) return []
  const range = lsp.Range.create(lsp.Position.create(0, 0), endPosition(source))
  return [lsp.TextEdit.replace(range, formatted)]
}

// Import/export file links

function position({ line, column }: ast.Cursor): lsp.Position {
  return lsp.Position.create(line - 1, column - 1)
}

function range(tree: ast.Traverse): lsp.Range {
  return lsp.Range.create(position(tree.start), position(tree.end))
}

// The range of a string token's contents, excluding its quotes.
function contentRange(spec: ast.Traverse): lsp.Range {
  const { start, end } = range(spec)
  return lsp.Range.create(
    lsp.Position.create(start.line, start.character + 1),
    lsp.Position.create(end.line, end.character - 1))
}

// `import { x } from "./foo.rv"` links to the file, relative to the importer.
function importLink(spec: ast.Traverse, dir: string): lsp.DocumentLink | undefined {
  const node = spec.node
  if (!(node instanceof ast.Token) || typeof node.value !== 'string') return
  if (!node.value.endsWith('.rv') || node.raw === undefined) return
  const target = pathToFileURL(join(dir, node.value)).toString()
  return lsp.DocumentLink.create(contentRange(spec), target)
}

function links(tree: ast.Traverse, dir: string): lsp.DocumentLink[] {
  const args = tree.args
  if (ast.isSyntax(tree.node, 'import') || ast.isSyntax(tree.node, 'export') && args.length === 4) {
    const link = importLink(args[3], dir)
    if (link) return [link]
  }
  return args.flatMap(arg => links(arg, dir))
}

// TODO replace with a definition provider
function documentLinks(uri: string, source: string): lsp.DocumentLink[] {
  if (!uri.startsWith('file:')) return []
  const path = join(fileURLToPath(uri))
  let tree: ast.Expr
  try {
    tree = parse(path, source)
  } catch {
    return [] // TODO fault-tolerant parsing
  }
  return links(new ast.Traverse(tree), dirname(path))
}

function startLsp() {
  const conn = lsp.createConnection(process.stdin, process.stdout)
  const documents = new Map<string, string>()

  conn.onInitialize(() => ({
    capabilities: {
      documentFormattingProvider: true,
      documentLinkProvider: { resolveProvider: false },
      textDocumentSync: {
        openClose: true,
        change: lsp.TextDocumentSyncKind.Full
      }
    }
  }))

  conn.onDidOpenTextDocument(({ textDocument }) => {
    documents.set(textDocument.uri, textDocument.text)
  })

  conn.onDidChangeTextDocument(({ textDocument, contentChanges }) => {
    const change = contentChanges.at(-1)
    if (documents.has(textDocument.uri) && change) documents.set(textDocument.uri, change.text)
  })

  conn.onDidCloseTextDocument(({ textDocument }) => { documents.delete(textDocument.uri) })

  conn.onDocumentFormatting(({ textDocument }) => {
    const source = documents.get(textDocument.uri)
    return source === undefined ? [] : formatEdits(textDocument.uri, source)
  })

  conn.onDocumentLinks(({ textDocument }) => {
    const source = documents.get(textDocument.uri)
    return source === undefined ? [] : documentLinks(textDocument.uri, source)
  })

  conn.listen()
}
