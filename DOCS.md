# The Raven Programming Language

## Getting Started

Start by trying out the [language tour](https://mikeinnes.io/posts/raven-tour/). Once you've got a feel, you may want to get Raven set up on your own machine, so you can build and debug bigger programs.

### With VS Code

The easiest way to set up Raven is using the [VS Code](https://code.visualstudio.com) editor extension. Install [the Raven plugin](https://marketplace.visualstudio.com/items?itemName=unkindnesses.raven-lang). Then open the command palette (`View -> Command Palette...`) and run `Raven: Add Terminal Command`. The `raven` command will then be available in new terminals (if you already have a terminal open, you may need to relaunch it). See [below](#the-cli) for how to use the `raven` command.

### With Node.js

Slightly more advanced: If you want to use Raven outside of VS Code, you can install [Node.js](https://nodejs.org/en) and then add the Raven package globally. It doesn't matter how you install Node, but here's my recommended setup:

* **Windows**: Run `winget install -e --id OpenJS.NodeJS` in a terminal.
* **MacOS**: Set up [Homebrew](https://brew.sh) and run `brew install node`.
* **Linux**: Set up [fnm](https://github.com/Schniz/fnm) with `curl -fsSL https://fnm.vercel.app/install | bash`, restart shell, `fnm install 25`.

Once you have node, it's just `npm i -g @unkindnesses/raven`, and you'll be able to use `raven` from there! See [below](#the-cli) for guidance.

### From source

For experts who want the latest and greatest, or to tweak Raven's code, it's pretty easy to clone the repo and go; you'll still need Node as above.

```
git clone https://github.com/Unkindnesses/raven
cd raven
npm install
npm run build
npm i -g .
```

The `npm i` command will put `raven` on your path, just like the packaged install. You can use `npm run watch` to live-update as you make changes to the source code.

Raven has no other run-time dependencies, but does rely on a few other tools for the full test suite: [wasm-tools](https://github.com/bytecodealliance/wasm-tools), [wasmtime](https://github.com/bytecodealliance/wasmtime) and [llvm](https://llvm.org) (MacOS/Linux only). On MacOS these are all a `brew install` away. You'll also need to `npx playwright install chromium`. Then you can run `npm test`.

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

To run with Node.js directly, you need the `--experimental-wasm-jspi` option.

```bash
$ node --experimental-wasm-jspi hello.js
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

Raven also has template strings, which use a prefix to alter how the string is interpreted (like a macro). Single-quote strings use a prefix, like for regexes or JavaScript:

```ruby
r`\d`
js`return 2+2`
```

Triple-quote templates put the tag just after the opening quote, like so:

````
```js
console.log("hello, world")
```
````

## Strings

Strings in double quotes, `"hello, world"`, understand escapes like `\n`, `\t`, `\\` and `\"`. Backticks avoid escaping.

```
> println("hello,\n world")
hello,
 world
> println(`hello,\n world`)
hello,\n world
```

You can add backslashes before and after the outer quotes to avoid escaping inner ones.

```
> println(\"hello, "world""\)
hello, "world"
> println(\`hello, `world``\)
hello, `world`
```

In double quotes with backslashes, escapes need the number of backslashes to match inside and out.

```
> println(\\"hello,\n world"\\)
hello,\n world
> println(\\"hello,\\n world"\\)
hello,
 world
```

Triple-quote strings, with both `"` and backticks, support escaping and extended delimiters in the same way. They remove indentation from the beginning of the string.

```
{
  a = """
  hello, world
  """
  b = "hello, world"
  test a == b
}
```

Regular expressions use the `r"..."` string macro. Test a string with `contains?` and iterate over matches (and capture groups) with `matches`.

```
> contains?("1, 2, 3", r`\d`)
true
> collect(matches("1, 2, 3", r`\d`))
[["1"], ["2"], ["3"]]
```

Strings are sequences of unicode scalar values, represented as 21-bit integers in the `Char` type.

```ruby
> collect("hello 🔥")
[c"h", c"e", c"l", c"l", c"o", c" ", c"🔥"]
> "hello 🔥"[7] == c"🔥"
true
> UInt32("hello 🔥"[7])
0x0001f525
```

Strings are abstracted from the storage format, and indexing is linear-time. You can get data views with constant-time access to code points, in a given encoding, with `chars`, `utf16` and `utf8`.

```ruby
> map(UInt32, chars("A"))
[0x00000041]
> collect(utf16("A"))
[0x0041]
> collect(utf8("A"))
[0x41]

> map(UInt32, chars("🔥"))
[0x0001f525]
> collect(utf16("🔥"))
[0xd83d, 0xdd25]
> collect(utf8("🔥"))
[0xf0, 0x9f, 0x94, 0xa5]
```

Note that graphemes may be composed from multiple `Char`s.

```ruby
> collect("🤦🏼‍♂️")
[c"🤦", c"🏼", c"‍", c"♂", c"️"]
```

## Calling JavaScript

The `js` function can be used to convert Raven objects to JavaScript ones. Use these largely as you would in JS proper.

```ruby
> js("hello")
js("hello")
> js("hello").toUpperCase()
js("HELLO")
```

You can also use `js` like a namespace; it represents `globalThis`.

```ruby
> js.Math.sqrt(5)
js(2.23606797749979)
```

Note that calling JS will result in (boxed) JS objects, not Raven ones, so you'll usually want to convert back.

```ruby
> String(js("hello").toUpperCase())
"HELLO"
> Float64(js.Math.sqrt(5))
2.23606797749979
```

You can write JavaScript inline, too.

```ruby
fn mysqrt(x) {
  result = js`return Math.sqrt(\x)`
  return Float64(result)
}
```

You can interpolate Raven values with `\`. The code is evaluated in a function context, so you need to `return` to get a value back. Here's how `sleep` is written.

````ruby
fn sleep(n) {
  p = ```js
  return new Promise(resolve => {
    setTimeout(() => { resolve() }, \n * 1000)
  })
  ```
  await(p)
  return
}
````

Unlike JS, Raven has no `async`/`await` keywords, or any distinction between async and sync functions. We still need to unwrap JS promises with `await`, but this is a normal function call.

Here's two other ways to get the UTF8 bytes from a string.

```ruby
> s = "foo"
"foo"
> map(UInt8, new(js.TextEncoder).encode(s))
[0x66, 0x6f, 0x6f]
> map(UInt8, js`return new TextEncoder().encode(\s)`)
[0x66, 0x6f, 0x6f]
```
