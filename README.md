<p align="center">
<img width="400px" src=".github/logo.png"/>
</p>

See the [documentation](https://quoththeraven.io/docs/manual/setup) for setup.

`test.rv`:

```rust
fn fib(n) { fib(n-1) + fib(n-2) }
fn fib(1) { 1 }
fn fib(0) { 0 }

fn fibSequence(n) {
  xs = []
  for i in range(1, n) {
    append(&xs, fib(i))
  }
  return xs
}

xs = fibSequence(10)
println(xs)
```

```julia
julia> using Raven

julia> Raven.exec("test.rv")
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
```

See also [complex numbers](common/numbers/complex.rv) or [malloc](common/wasm/malloc.rv).
