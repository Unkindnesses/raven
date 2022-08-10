<p align="center">
<img width="400px" src=".github/logo.png"/>
</p>

```
brew install --cask julia
brew install wabt binaryen node
julia --project
```

On Linux, download the Julia binaries and add `julia` to the path, then use `brew` to get the rest. On Windows, get WSL and then GOTO Linux. (You can alternatively `npm i -g wabt binaryen` once you have node, on any platform.)

`test.rv`:

```rust
fn fib(n) { fib(n-1) + fib(n-2) }
fn fib(1) { 1 }
fn fib(0) { 0 }

fn fibSequence(n) {
  xs = []
  for i in range(1, n) {
    push(&xs, fib(i))
  }
  return xs
}

xs = fibSequence(10)
println(xs)
```

```julia
julia> using Raven

julia> Raven.compile("test.rv"); run(`node test.js`);
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
```

See also [complex numbers](base/numbers/complex.rv) or [malloc](base/wasm/malloc.rv).

## Editor Support

For Atom:

```bash
ln -s $PWD/editor/language-raven ~/.atom/packages/
```

For VS Code:

```bash
cd editor/raven-language
npm install
npm run compile
ln -s $PWD ~/.vscode/extensions
```
