import { test } from 'uvu'
import * as assert from 'assert'
import { chromium } from 'playwright'
import * as http from 'http'
import { AddressInfo } from 'net'
import * as fs from 'fs'
import * as path from 'path'
import { fileURLToPath } from 'url'

declare const window: any

const __dirname = path.dirname(fileURLToPath(import.meta.url))
const root = path.join(__dirname, '..')

let server: http.Server
let port: number

test.before(async () => {
  server = http.createServer((req, res) => {
    const file = req.url === '/' ? 'test/web.html' : req.url!
    const filePath = path.join(root, file)
    const ext = path.extname(file)
    const type = ext === '.js' ? 'application/javascript' : ext === '.json' ? 'application/json' : 'text/html'
    res.writeHead(200, { 'Content-Type': type })
    fs.createReadStream(filePath)
      .on('error', () => res.end('Error'))
      .pipe(res)
  })

  await new Promise<void>((resolve) => {
    server.listen(0, () => {
      port = (server.address() as AddressInfo).port
      resolve()
    })
  })
})

test.after(() => {
  server.close()
})

test('browser compile', async () => {
  const browser = await chromium.launch()
  const page = await browser.newPage()
  const url = `http://localhost:${port}/test/web.html`
  await page.goto(url)
  await page.waitForFunction(() => (window as any).testResult !== undefined)
  const result = await page.evaluate(() => window.testResult)
  await browser.close()
  assert.ok(result.success)
})

test.run()
