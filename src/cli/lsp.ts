import * as process from 'process'
import * as lsp from 'vscode-languageserver/node'
import { format } from '../frontend/format.js'

export { startLsp }

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

function startLsp() {
  const conn = lsp.createConnection(process.stdin, process.stdout)
  const documents = new Map<string, string>()

  conn.onInitialize(() => ({
    capabilities: {
      documentFormattingProvider: true,
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
  conn.listen()
}
