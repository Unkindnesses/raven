<p align="center">
<img width="400px" src=".github/logo.png"/>
</p>

Check out the [language tour](https://mikeinnes.io/posts/raven-tour/)!

[Raven](https://mikeinnes.io/posts/raven/) is a small but smart language that compiles to WebAssembly. It combines a simple, functional data model, powerful type inference, and flexible syntax. It's currently in the proof-of-concept stage: you're welcome to play with it, but expect bugs to appear before long! Check out the [docs](DOCS.md) (such as they are) for more detail, including how to get started with the `raven` command on your system.

You can launch a repl:

```bash
$ raven
> 2+2
4
```

Or run a hello world program:

```bash
$ cat hello.rv
println("Cacaw, World!")
$ raven hello.rv
Cacaw, World!
```

You can explicitly compile and run a wasm binary. `fib.rv`:

```rust
fn fib(n) { fib(n-1) + fib(n-2) }
fn fib(1) { 1 }
fn fib(0) { 0 }

fn fibSequence(n) {
  xs = []
  for i = range(1, n) {
    append(&xs, fib(i))
  }
  return xs
}

show fibSequence(10)
```

```bash
$ raven build fib.rv
$ raven fib.wasm
fibSequence(10) = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
```

It may entertain you to create a self-contained JS file (with the shebang and permissions needed to run like a binary).

```bash
$ raven build --js hello.rv -o hello
$ ./hello
Cacaw, World!
```

Here's a brainfuck interpreter:

<details>
<summary><code>brainfuck.rv</code></summary>

```ruby
bundle Instr {
  Left(), Right()
  Inc(), Dec()
  Read(), Write()
  Loop(body: List)
}

fn parse(code) {
  stack = []
  body = []
  for ch = code {
    if ch == "<" {
      append(&body, Left())
    } else if ch == ">" {
      append(&body, Right())
    } else if ch == "+" {
      append(&body, Inc())
    } else if ch == "-" {
      append(&body, Dec())
    } else if ch == "," {
      append(&body, Read())
    } else if ch == "." {
      append(&body, Write())
    } else if ch == "[" {
      append(&stack, body)
      body = []
    } else if ch == "]" {
      loop = Loop(body)
      body = pop(&stack)
      append(&body, loop)
    }
  }
  return body
}

bundle Tape(data: List, i: Int64)

fn Tape() { Tape([0], 1) }

fn Tape(xs, i)[] { xs[i] }

fn left(&tape: Tape(xs, i)) {
  i = i - 1
  if i < 1 { abort("Tape error") }
  tape = Tape(xs, i)
}

fn right(&tape: Tape(xs, i)) {
  i = i + 1
  if i > length(xs) { append(&xs, 0) }
  tape = Tape(xs, i)
}

fn inc(&tape: Tape(xs, i)) {
  xs[i] = xs[i]+1
  tape = Tape(xs, i)
}

fn dec(&tape: Tape(xs, i)) {
  xs[i] = xs[i]-1
  tape = Tape(xs, i)
}

fn eval(&tape: Tape, instr: Instr) {
  match instr {
    let Left()  { left(&tape) }
    let Right() { right(&tape) }
    let Inc()   { inc(&tape) }
    let Dec()   { dec(&tape) }
    let Write() { print(js().String.fromCharCode(tape[])) }
    let Loop(body) {
      while tape[] != 0 {
        interpret(&tape, body)
      }
    }
  }
  return
}

fn interpret(&tape: Tape, code) {
  for instr = code {
    eval(&tape, instr)
  }
  return tape
}

fn interpret(code) {
  interpret(Tape(), code)
}

{
  code = parse(readFile(args()[3]))
  interpret(code)
}
```

</details>

```bash
$ raven build --js brainfuck.rv -o bf
$ cat hello.bf
++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
$ ./bf hello.bf
Hello World!
```

For more code samples, see [complex numbers](common/numbers/complex.rv) or [malloc](common/wasm/malloc.rv).
