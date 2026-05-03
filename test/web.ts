import { afterAll, beforeAll, test } from 'vitest'
import * as assert from 'assert'
import { chromium } from 'playwright'
import * as http from 'http'
import { AddressInfo } from 'net'
import * as fs from 'fs'
import * as path from 'path'
import { fileURLToPath } from 'url'
import { compileJS } from '../src/cli/compile.js'

declare const window: any

const __dirname = path.dirname(fileURLToPath(import.meta.url))
const root = path.join(__dirname, '..')
const fixtures = path.join(root, 'test/web')
const libPath = path.join(fixtures, 'lib.rv')
const libJSPath = path.join(fixtures, 'lib.js')
const libWasmPath = path.join(fixtures, 'lib.wasm')

let server: http.Server
let port: number

function contentType(file: string): string {
  const ext = path.extname(file)
  if (ext === '.js') return 'application/javascript'
  if (ext === '.json') return 'application/json'
  if (ext === '.wasm') return 'application/wasm'
  return 'text/html'
}

beforeAll(async () => {
  await compileJS(libPath, { output: libJSPath })

  server = http.createServer((req, res) => {
    const filePath = path.join(root, req.url!)
    const stream = fs.createReadStream(filePath)
    stream.on('open', () => {
      res.writeHead(200, { 'Content-Type': contentType(filePath) })
      stream.pipe(res)
    })
    stream.on('error', () => {
      if (!res.headersSent) res.writeHead(404)
      res.end('Error')
    })
  })

  await new Promise<void>((resolve) => {
    server.listen(0, () => {
      port = (server.address() as AddressInfo).port
      resolve()
    })
  })
})

afterAll(async () => {
  await new Promise<void>(resolve => server.close(() => resolve()))
})

test('browser compile', async () => {
  const browser = await chromium.launch()
  const page = await browser.newPage()
  const url = `http://localhost:${port}/test/web/web.html`
  await page.goto(url)
  await page.waitForFunction(() => (window as any).testResult !== undefined)
  const result = await page.evaluate(() => window.testResult)
  await browser.close()
  assert.ok(result.success)
})

test('browser js libs', async () => {
  const browser = await chromium.launch()
  const page = await browser.newPage()
  const url = `http://localhost:${port}/test/web/lib.html`
  await page.goto(url)
  await page.waitForFunction(() => (window as any).testLibResult !== undefined)
  const result = await page.evaluate(() => window.testLibResult)
  await browser.close()
  assert.ok(result.success, result.error)
  assert.strictEqual(result.value, 42)
})

