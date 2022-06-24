<p align="center">
<img width="400px" src=".github/logo.png"/>
</p>

```
brew install --cask julia
brew install wabt binaryen node
julia --project
```

`test.rv`:

```rust
fn range(n) {
  xs = []
  i = 1
  while (i <= n) {
    push(&xs, i)
    i = i + 1
  }
  return xs
}

xs = range(10)
println(xs)
```

```julia
julia> using Raven

julia> Raven.compile("test.rv"); run(`node test.js`);
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```
