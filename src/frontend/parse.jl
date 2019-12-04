using LNR

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

function tryparse(args...; kw...)
  try
    parse(args...; kw...)
  catch e
    e isa ParseError || rethrow(e)
    nothing
  end
end

function parseone(io, fs...)
  for f in fs
    (x = parse(f, io)) != nothing && return x
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

function number(io::IO)
  sym = IOBuffer()
  isnumeric(peek(io)) || return
  while !eof(io)
    p = position(io)
    c = read(io)
    c in '0':'9' || (seek(io, p); break)
    write(sym, c)
  end
  seek(sym, 0)
  return Base.parse(Int, String(Base.read(sym)))
end

function symbol(io)
  sym = IOBuffer()
  isletter(peek(io)) || return
  while !eof(io)
    p = position(io)
    c = read(io)
    isletter(c) || c in ('?','!') || c in ('0':'9') ||
      (seek(io, p); break)
    write(sym, c)
  end
  seek(sym, 0)
  return Symbol(Base.read(sym))
end

operators = ["=", "+", "-", "*", "/", ">", "<", ">=", "<=", "::", "."]
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
  return String(take!(s))
end

function quotation(io::IO)
  read(io) == '`' || return
  x = parse(io)
  read(io) == '`' || error("Expecting a `")
  return Quote(x)
end

struct Stmt
  indent::Int
end

function stmt(io::IO)
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
    push!(xs, x)
    nt = read(io)
    nt == stop && break
    nt == ',' || error("Expected a delimiter at $(curstring(io))")
  end
  return xs
end

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

function _tuple(io)
  bs = @try brackets(io)
  return Tuple(bs)
end

function expr(io)
  consume_ws(io)
  ex = parseone(io, symbol, string, number, op_token, quotation, _tuple, _block)
  ex == nothing && throw(ParseError("Unexpected character $(read(io))", loc(io)))
  ex == :return && return Return(expr(io))
  while (args = tryparse(brackets, io)) != nothing
    ex = Call(ex, args)
  end
  consume_ws(io)
  if (op = tryparse(op_token, io)) != nothing
    ex = Operator(op, [ex, parse(io)])
  end
  return ex
end

function _syntax(io)
  name = @try symbol(io)
  (!eof(io) && peek(io) in ('{', ' ')) || return
  args = []
  block = false
  while !eof(io)
    parse(stmt, io) == nothing || break
    consume_ws(io)
    peek(io) in ('}', ')', ']') && break
    next = block ? parse(io) : parse(expr, io)
    next isa Block && (block = true)
    push!(args, next)
  end
  block || return
  return Syntax(name, args)
end

function parse(io::IO)
  consume_ws(io); stmts(io)
  parseone(io, _syntax, expr)
end

parse(s::String) = parse(LineNumberingReader(IOBuffer(s)))

macro rvx_str(x)
  QuoteNode(parse(x))
end
