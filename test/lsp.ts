import * as assert from 'assert'
import * as path from 'node:path'
import { ChildProcess, spawn } from 'node:child_process'
import * as jsonrpc from 'vscode-jsonrpc/node.js'
import { test } from 'vitest'

const cliPath = path.join(process.cwd(), 'dist/cli/index.js')

async function waitForClose(child: ChildProcess, timeout = 2000): Promise<number> {
  return await new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      child.kill()
      reject(new Error('timed out waiting for lsp process to exit'))
    }, timeout)
    child.on('error', err => {
      clearTimeout(timer)
      reject(err)
    })
    child.on('close', code => {
      clearTimeout(timer)
      resolve(code ?? 1)
    })
  })
}

test('lsp formats open documents', async () => {
  const child = spawn(process.execPath, ['--enable-source-maps', cliPath, 'lsp'], {
    stdio: ['pipe', 'pipe', 'pipe']
  })
  let stderr = ''
  child.stderr.on('data', chunk => { stderr += chunk.toString('utf8') })

  const connection = jsonrpc.createMessageConnection(
    new jsonrpc.StreamMessageReader(child.stdout),
    new jsonrpc.StreamMessageWriter(child.stdin),
    jsonrpc.NullLogger
  )
  connection.listen()

  const initialize = await connection.sendRequest<{ capabilities: Record<string, unknown> }>('initialize', {
    capabilities: {}
  })
  assert.deepStrictEqual(initialize.capabilities, {
    documentFormattingProvider: true,
    textDocumentSync: { openClose: true, change: 1 }
  })

  await connection.sendNotification('initialized', {})
  await connection.sendNotification('textDocument/didOpen', {
    textDocument: {
      uri: 'file:///test.rv',
      languageId: 'raven',
      version: 1,
      text: 'x\n'
    }
  })
  await connection.sendNotification('textDocument/didChange', {
    textDocument: { uri: 'file:///test.rv', version: 2 },
    contentChanges: [{ text: 'fn f() {\nx\n}' }]
  })

  const edits = await connection.sendRequest('textDocument/formatting', {
    textDocument: { uri: 'file:///test.rv' },
    options: { tabSize: 2, insertSpaces: true }
  })
  assert.deepStrictEqual(edits, [{
    range: {
      start: { line: 0, character: 0 },
      end: { line: 2, character: 1 }
    },
    newText: 'fn f() {\n  x\n}\n'
  }])

  await connection.sendNotification('textDocument/didChange', {
    textDocument: { uri: 'file:///test.rv', version: 3 },
    contentChanges: [{ text: '???' }]
  })
  const invalidEdits = await connection.sendRequest('textDocument/formatting', {
    textDocument: { uri: 'file:///test.rv' },
    options: { tabSize: 2, insertSpaces: true }
  })
  assert.deepStrictEqual(invalidEdits, [])

  await connection.sendRequest('shutdown')
  await connection.sendNotification('exit')
  connection.end()

  const code = await waitForClose(child)
  connection.dispose()

  assert.strictEqual(code, 0, stderr)
})
