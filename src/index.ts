#!/usr/bin/env node --enable-source-maps --experimental-wasm-stack-switching
import * as commander from 'commander'
import * as nodeRepl from 'node:repl'
import { compile, compileJS, exec } from './backend/compiler'
import { REPL } from './backend/repl'

async function startRepl() {
  const raven = await REPL.create()
  try {
    const server = nodeRepl.start({
      prompt: '> ',
      ignoreUndefined: true,
      eval(cmd, _context, _filename, callback) {
        const source = cmd.replace(/[\r\n]+$/, '')
        if (!source.trim()) return callback(null, undefined)
        raven.eval(source).then(
          () => callback(null, undefined),
          error => {
            const err = error instanceof Error ? error : new Error(String(error))
            callback(err, undefined)
          }
        )
      }
    })
    await new Promise(resolve => server.on('exit', resolve))
  } finally {
    await raven.close()
  }
}

async function main() {
  const program = new commander.Command()
  program
    .name('raven')
    .description('The Raven Programming Language')

  program
    .command('compile')
    .description('Compile a Raven source file')
    .argument('<source>', 'Source file to compile')
    .option('-o, --output <file>', 'Rename output file')
    .option('--js', 'Emit JS')
    .action(async (source, { output, js }) => {
      let { inline, memcheck } = program.optsWithGlobals()
      if (js) await compileJS(source, { options: { inline, memcheck }, output })
      else await compile(source, { options: { inline, memcheck }, output })
    })

  program
    .argument('[source] [args...]', 'Source file to execute')
    .option('--no-inline', 'Disable function inlining')
    .option('--no-memcheck', 'Disable allocation checks')
    .action(async (xs) => {
      let { inline, memcheck } = program.optsWithGlobals()
      let [source, ...args] = xs
      if (source) await exec(source, args, { options: { inline, memcheck } })
      else await startRepl()
    })

  await program.parseAsync()
}

main().catch(err => {
  console.error(err)
  process.exit(1)
})
