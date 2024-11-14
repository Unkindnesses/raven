# Declared in middle/utils.jl
function withpath end
function path end

module Parse

using LNR
using ..AST: Expr, Group, List, Splat, Call, Field,
  Operator, Block, Syntax, Quote, Template, Swap, Meta, meta,
  unwrapToken
using ..Raven: withpath, path

# Precedence Table

@enum Precedence begin
  Left  =  1
  Right = -1
  None  =  0
end

inverse(p::Precedence) = Precedence(-Int(p))

struct PrecedenceTable
  ops::Dict{String,Int}
  table::Matrix{Precedence}
end

PrecedenceTable(ops) =
  PrecedenceTable(Dict(op => i for (i, op) in enumerate(ops)),
            fill(None, length(ops), length(ops)))

Base.getindex(t::PrecedenceTable, a, b) = t.table[t.ops[a], t.ops[b]]

function Base.setindex!(t::PrecedenceTable, p::Precedence, a, b)
  t.table[t.ops[a], t.ops[b]] in (None, p) || @warn "overwriting precedence"
  t.table[t.ops[b], t.ops[a]] = inverse(p)
  t.table[t.ops[a], t.ops[b]] = p
end

function precedence!(t::PrecedenceTable, ops::String...)
  for i = 1:length(ops)-1
    t[ops[i], ops[i+1]] = Left
  end
end

function closure!(t::PrecedenceTable)
  N = length(t.ops)
  for i = 1:N, j = 1:N, k = 1:N
    ab, bc = t.table[i, j], t.table[j, k]
    if ab == bc != None
      t.table[k, i] = inverse(ab)
      t.table[i, k] = ab
    end
  end
  return t
end

# Other utils

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

function parseone(io, fs...; kw...)
  for f in fs
    (x = parse(f, io; kw...)) != nothing && return x
  end
end

@eval macro $:try(x, alt = nothing)
  quote
    local result = $(esc(x))
    result === nothing && return $(esc(alt))
    result
  end
end

char(x) = io -> read(io) == x ? x : nothing

const whitespace = (' ', '\t', '\r')

function skip_lineend(io)
  while (!eof(io) && peek(io) != '\n')
    read(io)
  end
end

# Skip whitespace, not including newlines.
function skip_ws(io::IO)
  while !eof(io)
    c = read(io)
    c == '#' && (skip_lineend(io); break)
    c in whitespace || (seek(io, position(io)-1); break)
  end
end

# Skip whitespace and `,` to get to the next statement.
function skip(io::IO)
  while !eof(io)
    c = read(io)
    c == '#' && (skip_lineend(io); continue)
    c in whitespace || c in (',', '\n') || (seek(io, position(io)-1); break)
  end
  return io
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
  num = @try number(io)
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
  name = @try symbol(io)
  return Swap(name)
end

terminators = ['}', ')', ']', ',', '\n']

operators = ["=", "==", "!=", "+", "-", "*", "/", "^", ">", "<", ">=", "<=",
             ":", "&", "|", "|>", "&&", "||"]
opchars = unique(reduce(*, operators))

function op_token(io::IO)
  skip_ws(io)
  cur = cursor(io)
  op = IOBuffer()
  while !eof(io)
    peek(io) in opchars || break
    write(op, read(io))
  end
  seek(op, 0)
  s = String(Base.read(op))
  s in operators || return
  return meta(Symbol(s), path(), cur)
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
  x = expr(io, quasi = false)
  read(io) == '`' || error("Expecting a `")
  return Quote(x)
end

# Parsing

function template(io::IO)
  name = @try symbol(io)
  eof(io) && return
  s = @try string(io)
  return Template(name, s)
end

function brackets(io, start = '(', stop = ')')
  read(io) == start || return
  xs = []
  while true
    skip(io)
    peek(io) == stop && break
    push!(xs, statement(io))
  end
  read(io)
  return xs
end

group(io) = Group(@try(brackets(io))...)

list(io) = List(@try(brackets(io, '[', ']'))...)

block(io) = Block(@try(brackets(io, '{', '}'))...)

nop(io) = nothing

# Combine all simple expressions with little backtracking
function item(io; quasi = true)
  skip_ws(io)
  cur = cursor(io)
  quot = quasi ? quotation : nop # TODO nested quotation
  ex = parseone(io, template, symbol, swap, string, number, op_token, quot, group, list, block)
  ex == nothing && throw(ParseError("Unexpected character $(read(io))", loc(io)))
  ex = meta(ex, path(), cur)
  return ex
end

# The following parsers fall back to simpler ones, to avoid excessive
# backtracking / re-parsing. So they don't need to be called in sequence.

# Does calls and fields, so we can handle eg `foo.bar(a).baz`
function call(io; quasi = true)
  ex = item(io; quasi)
  while true
    cur = cursor(io)
    if (args = parse(brackets, io)) != nothing
      ex = meta(Call(ex, args...), path(), cur)
    elseif parse(exact("."), io) != nothing
      peek(io) == '.' && (seek(io, cur); break)
      field = item(io)
      field == nothing && error("expected a field at $(curstring(io))")
      ex = meta(Field(ex, field), path(), cur)
    else
      break
    end
  end
  return ex
end

begin
  table = PrecedenceTable(operators)
  precedence!(table, "^", "/", "*", "+", "-", "=")
  for op in ["/", "*", "+", "-", "|", "&"]
    table[op, op] = Left
  end
  closure!(table)
end

precedence(a, b) = table[String.(unwrapToken.((a, b)))...]

function operator(io; quasi = true, syn = true, prev = nothing)
  left = call(io; quasi)
  while true
    pos = position(io)
    op = @try parse(op_token, io) left
    prec = prev == nothing ? Right : precedence(prev, op)
    if prec == Left
      seek(io, pos)
      return left
    elseif prec == Right
      skip(io)
      right = syn ? parse(syntax, io; quasi) : nothing
      right == nothing && (right = operator(io; quasi, prev = op))
      left = Operator(op, left, right)
      left = meta(left, meta(op))
    else prec == None
      error("Operators $(unwrapToken(prev)) and $(unwrapToken(op)) are ambiguous at $(curstring(prev.meta.loc))")
    end
  end
end

function splat(io; quasi = true, syn = true)
  ex = operator(io; quasi, syn)
  skip_ws(io)
  if parse(exact("..."), io) != nothing
    ex = Splat(ex)
  end
  return ex
end

# Syntax blocks
# TODO try to avoid as much re-parsing as possible.
function syntax(io; quasi = true)
  cur = cursor(io)
  name = splat(io; quasi)
  unwrapToken(name) isa Symbol || return
  args = []
  while !eof(io)
    skip_ws(io)
    peek(io) in terminators && break
    # `syn` fixes eg `fn x + y {}`, where `y {}` would be
    # parsed as an argument to `+` otherwise.
    next = splat(io; quasi, syn = false)
    next == nothing && return
    push!(args, next)
  end
  isempty(args) && return
  ex = Syntax(name, args...)
  ex = meta(ex, path(), cur)
  return ex
end

function statement(io; quasi = true)
  skip(io)
  eof(io) && return
  ex = parse(syntax, io; quasi)
  ex == nothing && (ex = splat(io))
  skip_ws(io)
  eof(io) || peek(io) in terminators || error("Expected statement end at $(curstring(io))")
  return ex
end

# Parse a single statement.
# returns `nothing` if there is no valid input (EOF or only whitespace/comments)
function parse(io::LineNumberingReader; path)
  withpath(path) do
    statement(io)
  end
end

parse(io::IO; path) = unwrapToken(parse(LineNumberingReader(io); path))

parse(s::String; path) = parse(IOBuffer(s); path)

macro rvx_str(x)
  QuoteNode(parse(x, path = String(__source__.file)))
end

end

using .Parse: parse, @rvx_str
