# The Raven Programming Language

## The CLI

Use `raven --help` for a command reference. On its own, the `raven` command launches a REPL.

```bash
$ raven
> 2+2
4
```

Run a script:

```bash
$ cat hello.rv
println("Cacaw, World!")
$ raven hello.rv
Cacaw, World!
```

Build and then run a wasm binary:

```bash
$ raven build hello.rv
$ raven hello.wasm
Cacaw, World!
```

You can name the output file with `-o`, so this is the same as `raven build hello.rv -o hello.wasm`.

Build and run a JS file:

```bash
$ raven build --js hello.rv
$ ./hello.js
Cacaw, World!
```

To run with Node.js directly, you need the `--experimental-wasm-stack-switching` option.

```bash
$ node --experimental-wasm-stack-switching hello.js
Cacaw, World!
```

Profile the compiler:

```bash
$ raven build hello.rv --time
Load          113.76ms
Definitions    89.35ms
Interpret     119.46ms
Inference     496.77ms
Expansion     228.47ms
Inlining         1.04s
Refcounts     257.79ms
WASM          158.22ms
Total            2.38s
```

## The Syntax

Simple literals, function calls and operators are conventional.

```ruby
# a comment
println("Cacaw, World!")
(1 + 2.5) / 3
```

When running from a file, lines are evaluated one by one, so `println("hello")` is a complete script.

Identifiers are alphanumeric with `_`, `?` and `!` allowed. By convention we use `snakeCase` with `!` for mutating functions and `?` for predicates, eg `even?(number)`.

Functions and control flow also look standard:

```rust
fn pow(x, n: Int) {
  r = one(x)
  while n > 0 {
    n = n - one(n)
    r = r * x
  }
  return r
}
```

`x: Foo` is a type annotation, which is always optional. Plain `x` in a function signature is the same as `x: Any`.

`return`s can be implicit; a function returns whatever its body evaluated to.

```rust
fn square(x) { x * x }
```

Similarly, loops can be used as expressions, and produce lists.

```rust
> for i = range(1, 5) { i^2 }
[1, 4, 9, 16, 25]
```

Variables don't need to be declared. Assignment `x = ...` modifies an existing variable within the current function, if there is one, or creates a new one in the nearest `{ ... }` brackets.

```ruby
{
  x = 1
  { x = 2, y = 3 }
  show x # x = 2
  show y # Error: `y` is not defined
}
```

```ruby
x = 1
fn foo() {
  x = 2
  show x
}
foo() # x = 2
show x # x = 1
```

You can override the default with `let`:

```ruby
{
  x = 1
  let x = 2 { show x } # x = 2
  show x # x = 1
}
```

An unusual operator is the swap `&`, which appears in both function definitions and calls.

```ruby
fn inc(&x) {
  x = x + 1
}

x = 5
inc(&x)
show x # x = 6
```

This resembles taking an address in languages like C, but it's really just a shorthand for `x = inc(x)`. When you use `&foo` in a signature, `foo` will be returned back to the caller. Data in Raven is generally immutable, so when we talk about variables changing, it's in the sense of changing clothes. Changes can't turn up in unexpected places.

```ruby
xs = [1, 2, 3]
ys = xs
append(&ys, 4)
show xs # xs = [1, 2, 3]
show ys # ys = [1, 2, 3, 4]
```

The statement separator is a comma `,` or newline, not a semicolon `;`. Conceptually, blocks `{...}` are just lists of statements. There is no difference to the parser between `{a, b, c}`, `[a, b, c]` and `(a, b, c)` – you can write a multi-statement block as `i = {println(i), i+1}` or a list as

```rust
xs = [
  1
  2
  3
]
```

`fn`, `while`, `for` and so on aren't keywords, but macros. The syntax `a b c` applies the macro `a` to the inputs `b` and `c`. Macros read to the end of the line, but this doesn't count lines absorbed by brackets, so

```
if cond {
  truth
} else {
  falth
}
```

invokes the `if` macro with the four arguments `cond`, `{ truth }`, `else` and `{ falth }`. User-defined macros are not yet implemented, but many basic constructs like `for` loops are implemented as AST transforms, and there are some handy built-in macros like `show`.

```rust
> show 2+2
(2 + 2) = 4
```

Raven uses `bundle` to describe new data types (similar to ADTs in functional languages).

```ruby
bundle Maybe { Some(x), Nil() }
show Some(5) # Some(5) = Some(5)
```

Pattern matching / destructuring is pervasive. Assignments can use constructors (`Some(x)`) or lists (`[x, y]`) to unpack data structures, eg

```ruby
m = Some(5)
Some(x) = m
show x # x = 5
```

This will error if `m` is `Nil()`. But we can extend it to handle both cases using `if let`, `match` or function overloading. Here's three ways to do the same thing:

```rust
if let Some(x) = m {
  println("x is not nil :D")
} else {
  println("x is nil :(")
}

match m {
  let Some(x) { println("x is not nil :D") }
  let Nil() { println("x is nil :(") }
}

fn nil?(Some(x)) { println("x is not nil :D") }
fn nil?(Nil()) { println("x is nil :(") }

nil?(m)
```

`match` is generally the best option, except where `if let` would be significantly terser. (In future `match` will check you've covered all cases correctly.)

Loops can be labelled.

```rust
@label outer
for i = range(0, 3) {
  @label inner
  for j = range(0, 3) {
    if i == 1 && j == 1 { continue outer }
    if i == 2 && j == 0 { break outer }
  }
}
```

You can also label plain blocks. `continue` goes to the start, and `break` to the end. (Note that unlabelled `break` and `continue` will always target the nearest loop.)

```rust
x = 3
@label blk
{
  if x > 0 { break blk }
  x = 0-x
}
show x
```

The `@foo` annotation syntax is general. Annotations are somewhat like macros, and read arguments up to the end of the line and apply to the next line. However, unlike macros they are passive; they get passed as metadata to the relevant macro (in this case the `for` loop), which decides how to interpret them.

A couple other examples are `@extend`, which is used to add methods to functions from other modules:

```rust
@extend
fn Float32(x: Float64) {
  wasm { f32.demote_f64(x: f64): f32 }
}
```

And `@doc`, which is used for markdown doc strings.

```rust
@doc """
    identity(x) == x

The identity function.
"""
fn identity(x) { x }
```
