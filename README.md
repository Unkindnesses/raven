<p align="center">
<img width="400px" src=".github/logo.png"/>
</p>

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

show fibSequence(10)
```

```julia
julia> using Raven

julia> Raven.exec("test.rv")
fibSequence(10) = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
```

You can also get to the Raven REPL, from Julia's, with the `=` key.

```julia
raven> x = 2+2
4

raven> fn square(x) { x * x }

raven> square(x)
16
```

See also [complex numbers](common/numbers/complex.rv) or [malloc](common/wasm/malloc.rv).
