---
title: Getting Set Up
---

## Using the Grimoire

You don't *need* any setup to start exploring Raven. This documentation is interactive, so you can run and tweak all its examples from the comfort of your browser. To get started we recommend perusing these pages for a while, without installing anything.

```raven
2+2 # Try editing this!
```

If you just want to explore, you can move straight on to the basics.

If you want start doing more ambitious things, the best way is to get the [VS Code editor with the Raven plugin](#setup-with-vs-code).

## Setup with VS Code

First, install [VS Code](https://code.visualstudio.com/). (If you have VS Code already, you can skip this step.)

Go the [Raven Language plugin page](https://marketplace.visualstudio.com/items?itemName=MikeInnes.raven-language) and hit `Install` to get Raven support in VS Code. That's it!

### Try it out

In VS Code, create a new file and save it as `hello.rv`. Type

```raven
2+2
```

into the file. You'll see the answer, `4`, appear immediately.

Type this into the file:

```raven
println("Hello, World!")
```

Open the [command palette](https://code.visualstudio.com/docs/getstarted/userinterface#_command-palette) and run the `Raven: Run File` command. You'll see `Hello, World!` appear in the console below.

## Installing Raven locally

If you want to use Raven with a different editor, from the command line, or to run code on a server, you'll need to install the command line tool. (Beginners do not need to do this.)

First, install [Node.js](https://nodejs.org/en/download/). (If you have Node.js already, you can skip this step.)

Then open a terminal or command prompt. Run `npm install -g raven-lang` to get the Raven language command line tool.

You can then run `raven` in the terminal. On its own `raven` will start an interactive session.

### Try it out

Create a file `hello.rv` with the contents:

```raven
println("Hello, World!")
```

In a terminal, navigate to the folder containing `hello.rv` and run `raven hello.rv`. You will see `Hello, World!` displayed as output.

```
$ raven hello.rv
Hello, World!
```

:::tip
If you're on a Mac and use [Homebrew](https://brew.sh/), you can alternatively get set up with `brew install raven`. You don't need to install Node.js separately.
:::

:::warning
Installing Raven via the command line doesn't give you any editor support, so you'll miss out on a lot of what makes it fun. We highly recommend getting the [VS Code plugin](#setup-with-vs-code) whether or not you use this setup.
:::
