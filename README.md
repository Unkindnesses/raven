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
fn squares(n) {
  xs = []
  for i in range(1, n) {
    push(&xs, i^2)
  }
  return xs
}

xs = squares(10)
println(xs)
```

```julia
julia> using Raven

julia> Raven.compile("test.rv"); run(`node test.js`);
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```
