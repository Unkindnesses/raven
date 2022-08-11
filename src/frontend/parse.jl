module Parse

using LNR
using ..AST: Expr, Return, Break, Continue, Tuple, Splat, Call,
  Operator, Block, Syntax, Quote, Swap

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

function consume_ws(io::IO)
  while !eof(io)
    c = read(io)
    c == '#' && (consume_line(io); break)
    c in whitespace || (seek(io, position(io)-1); break)
  end
end

# Tokens

function exact(s)
  function (io)
    for ch in s
      (!eof(io) && (read(io) == ch)) || return
    end
    return s
  end
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
  consume_ws(io)
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
    consume_ws(io)
    s′ = parse(stmt, io)
    s′ == nothing && return s
    s = s′
  end
end

# Parsing

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
    if parse(exact("..."), io) != nothing
      x = Splat(x)
    end
    push!(xs, x)
    nt = read(io)
    nt == stop && break
    nt == ',' || error("Expected a delimiter at $(curstring(io))")
  end
  return xs
end

_tuple(io) = Tuple(@try(brackets(io, '[')))

function _block(io)
  read(io) == '{' || return
  consume_ws(io)
  peek(io) == '}' && return Block([])
  args = []
  while !eof(io)
    stmts(io)
    peek(io) == '}' && break
    push!(args, parse(io))
  end
  read(io)
  return Block(args)
end

function grouping(io)
  read(io) == '(' || return
  x = parse(io)
  nt = read(io)
  nt == ')' || error("Expected closing bracket at $(curstring(io))")
  return x
end

function ret(io)
  symbol(io) == :return || return
  tryparse(stmt, io) != nothing && return Return(Call(:data, [Quote(:Nil)]))
  Return(expr(io))
end

function _break(io)
  symbol(io) == Symbol("break") || return
  return Break()
end

nop(io) = nothing

function expr(io; quasi = true)
  consume_ws(io)
  quot = quasi ? quotation : nop
  ex = parseone(io, ret, _break, symbol, swap, string, number, op_token, quot, grouping, _tuple, _block)
  ex == nothing && throw(ParseError("Unexpected character $(read(io))", loc(io)))
  while (args = tryparse(brackets, io)) != nothing
    ex = Call(ex, args)
  end
  consume_ws(io)
  if (op = tryparse(op_token, io)) != nothing
    ex = Operator(op, [ex, parse(io)])
  end
  return ex
end

function _syntax(io; quasi = true)
  name = @try expr(io)
  name isa Symbol || return
  !eof(io) || return
  args = []
  block = false
  while !eof(io)
    parse(stmt, io) == nothing || break
    consume_ws(io)
    peek(io) in ('}', ')', ']') && break
    next = tryparse(expr, io; quasi)
    next == nothing && return
    next isa Block && (block = true)
    push!(args, next)
  end
  block || return
  return Syntax(name, args)
end

# returns `nothing` if there is no valid input (EOF or only whitespace/comments)
function parse(io::LineNumberingReader; quasi = true)
  stmts(io)
  parseone(io, _syntax, expr; quasi)
end

parse(io::IO) = parse(LineNumberingReader(io))

parse(s::String) = parse(IOBuffer(s))

macro rvx_str(x)
  QuoteNode(parse(x))
end

end

using .Parse: parse, @rvx_str
