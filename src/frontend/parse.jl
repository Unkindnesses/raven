# Declared in middle/utils.jl
function withpath end
function path end

module Parse

using LNR
using ..AST: Expr, Return, Break, Continue, List, Splat, Call,
  Operator, Block, Syntax, Quote, Template, Swap, Meta, meta,
  unwrapToken
using ..Raven: withpath, path

struct ParseError
  m
  loc
end

Base.mark(io::LineNumberingReader) = mark(io.io)
Base.reset(io::LineNumberingReader) = reset(io.io)

loc(io) = (cursor(io).line, cursor(io).column)
curstring(cur::Cursor) = "$(cur.line):$(cur.column)"
curstring(io::LineNumberingReader) = curstring(cursor(io))

read(io::IO) = Base.read(io, Char)

function peek(f, io::IO)
  p = position(io)
  c = f(io)
  seek(io, p)
  return c
end

peek(io::IO) = peek(read, io)

function parse(f, io, a...; kw...)
  eof(io) && return
  p = position(io)
  result = f(io, a...; kw...)
  result === nothing && (seek(io, p); return)
  return result
end

function tryparse(f, io, args...; kw...)
  p = position(io)
  try
    parse(f, io, args...; kw...)
  catch e
    e isa ParseError || rethrow(e)
    seek(io, p)
    nothing
  end
end

function parseone(io, fs...; kw...)
  for f in fs
    (x = parse(f, io; kw...)) != nothing && return x
  end
end

@eval macro $:try(x)
  quote
    local result = $(esc(x))
    result === nothing && return
    result
  end
end

char(x) = io -> read(io) == x ? x : nothing

const whitespace = (' ', '\t', '\r')

function consume_line(io)
  while (!eof(io) && peek(io) != '\n')
    read(io)
  end
end

# Skip whitespace, not including newlines.
function skip_ws(io::IO)
  while !eof(io)
    c = read(io)
    c == '#' && (consume_line(io); break)
    c in whitespace || (seek(io, position(io)-1); break)
  end
end

# Tokens

exact(s) = function (io)
  for ch in s
    (!eof(io) && (read(io) == ch)) || return
  end
  return s
end

function integer(io::IO)
  num = IOBuffer()
  isnumeric(peek(io)) || return
  while !eof(io)
    p = position(io)
    c = read(io)
    c in '0':'9' || (seek(io, p); break)
    write(num, c)
  end
  seek(num, 0)
  return Base.parse(Int, String(Base.read(num)))
end

function negnum(io::IO)
  read(io) == '-' || return
  num = number(io)
  num == nothing && return
  return -num
end

number(io::IO) = parseone(io, negnum, integer)

function symbol(io)
  sym = IOBuffer()
  c = peek(io)
  isletter(c) || c in ('_',) || return
  while !eof(io)
    p = position(io)
    c = read(io)
    isletter(c) || c in ('!','_') || c in ('0':'9') ||
      (seek(io, p); break)
    write(sym, c)
  end
  seek(sym, 0)
  return Symbol(Base.read(sym))
end

function swap(io)
  c = read(io)
  c == '&' || return
  name = symbol(io)
  name == nothing && return
  return Swap(name)
end

operators = ["=", "==", "!=", "+", "-", "*", "/", "^", ">", "<", ">=", "<=",
             ":", ".", "&", "|", "|>", "&&", "||"]
opchars = unique(reduce(*, operators))

function op_token(io::IO)
  op = IOBuffer()
  while !eof(io)
    peek(io) in opchars || break
    write(op, read(io))
  end
  seek(op, 0)
  s = String(Base.read(op))
  s in operators || return
  return Symbol(s)
end

function string(io::IO)
  read(io) == '"' || return
  s = IOBuffer()
  while (c = read(io)) != '"'
    write(s, c)
  end
  return unescape_string(String(take!(s)))
end

function quotation(io::IO)
  read(io) == '`' || return
  x = parse(io, quasi = false)
  read(io) == '`' || error("Expecting a `")
  return Quote(x)
end

struct Stmt
  indent::Int
end

function stmt(io::IO)
  skip_ws(io)
  read(io) == '\n' || return
  i = 0
  while !eof(io) && peek(io) ∈ whitespace
    i += 1
    read(io)
  end
  return Stmt(i)
end

function stmts(io)
  s = parse(stmt, io)
  s == nothing && return
  while true
    skip_ws(io)
    s′ = parse(stmt, io)
    s′ == nothing && return s
    s = s′
  end
end

# Parsing

function template(io::IO)
  name = symbol(io)
  name == nothing && return
  eof(io) && return
  s = string(io)
  s == nothing && return
  return Template(name, s)
end

bracketmap = Dict(
  '(' => ')',
  '[' => ']',
  '{' => '}')

function brackets(io, start = '(', stop = bracketmap[start])
  read(io) == start || return
  xs = []
  while true
    parse(char(stop), io) == nothing || break
    x = parse(io)
    push!(xs, x)
    nt = read(io)
    nt == stop && break
    nt == ',' || error("Expected a delimiter at $(curstring(io))")
  end
  return xs
end

list(io) = List(@try(brackets(io, '['))...)

function block(io)
  read(io) == '{' || return
  skip_ws(io)
  args = []
  # TODO don't allow multiple statements on one line (without a separator)
  while !eof(io)
    stmts(io)
    peek(io) == '}' && break
    push!(args, parse(io))
  end
  read(io)
  return Block(args...)
end

function grouping(io)
  read(io) == '(' || return
  x = parse(io)
  read(io) == ')' || error("Expected closing bracket at $(curstring(io))")
  return x
end

function ret(io)
  symbol(io) == :return || return
  tryparse(stmt, io) != nothing && return Return(Call(:pack, Template(:tag, "Nil")))
  Return(expr(io))
end

function _break(io)
  symbol(io) == Symbol("break") || return
  return Break()
end

nop(io) = nothing

function expr(io; quasi = true)
  skip_ws(io)
  cur = cursor(io)
  # TODO nested quotation
  quot = quasi ? quotation : nop
  ex = parseone(io, ret, _break, template, symbol, swap, string, number, op_token, quot, grouping, list, block)
  ex == nothing && throw(ParseError("Unexpected character $(read(io))", loc(io)))
  ex = meta(ex, path(), cur)
  # Function calls
  while true
    cur = cursor(io)
    args = tryparse(brackets, io)
    args == nothing && break
    ex = Call(ex, args...)
    ex = meta(ex, path(), cur)
  end
  skip_ws(io)
  # Operators
  cur = cursor(io)
  if (op = tryparse(op_token, io)) != nothing
    op = meta(op, path(), cur)
    ex = Operator(op, ex, parse(io))
    ex = meta(ex, path(), cur)
  end
  # Splats
  if parse(exact("..."), io) != nothing
    ex = Splat(ex)
  end
  return ex
end

function syntax(io; quasi = true)
  cur = cursor(io)
  name = @try expr(io)
  unwrapToken(name) isa Symbol || return
  !eof(io) || return
  args = []
  block = false
  while !eof(io)
    parse(stmt, io) == nothing || break
    skip_ws(io)
    peek(io) in ('}', ')', ']', ',') && break
    next = expr(io; quasi)
    next == nothing && return
    next isa Block && (block = true)
    push!(args, next)
  end
  block || return
  ex = Syntax(name, args...)
  ex = meta(ex, path(), cur)
  return ex
end

# Parse a Raven statement.
# returns `nothing` if there is no valid input (EOF or only whitespace/comments)
function parse(io::LineNumberingReader; path = nothing, quasi = true)
  path == nothing || return withpath(() -> parse(io; quasi), path)
  stmts(io)
  parseone(io, syntax, expr; quasi)
end

parse(io::IO; path) = unwrapToken(parse(LineNumberingReader(io); path))

parse(s::String; path) = parse(IOBuffer(s); path)

macro rvx_str(x)
  QuoteNode(parse(x, path = String(__source__.file)))
end

end

using .Parse: parse, @rvx_str
