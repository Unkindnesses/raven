macro sym_str(s) Expr(:quote, Symbol(s)) end

# Tokenisation

import Base: peek
using LNR

const whitespace = (' ', '\t', '\r')

function consume_ws(io::IO)
  while !eof(io)
    eof(io) && return
    c = Char(read(io, UInt8))
    c in whitespace || (seek(io, position(io)-1); break)
  end
end

function num_token(io::IO)
  sym = IOBuffer()
  while !eof(io)
    c = Char(read(io, UInt8))
    c in '0':'9' || (seek(io, position(io)-1); break)
    write(sym, c)
  end
  seek(sym, 0)
  return Base.parse(Int, String(read(sym)))
end

function symbol_token(io::IO)
  sym = IOBuffer()
  while !eof(io)
    c = Char(read(io, UInt8))
    c in 'a':'z' || c in 'A':'Z' || c in ('?','!') || c in ('0':'9') ||
      (seek(io, position(io)-1); break)
    write(sym, c)
  end
  seek(sym, 0)
  return String(read(sym))
end

operators = [":", "=", ":=", "+", "-", "*", "/", ">", "<", ">=", "<="]
opchars = unique(map(first, operators))

function op_token(io::IO)
  ops = operators
  i = 1
  while !eof(io)
    c = Char(read(io, UInt8))
    ops′ = filter(x -> get(x, i, nothing) == c, ops)
    isempty(ops′) && (seek(io, position(io)-1); break)
    ops = ops′
    i += 1
  end
  return Symbol(ops[1])
end

struct Stmt
  indent::Int
end

function stmt_token(io::IO)
  i = 0
  read(io, Char)
  while !eof(io) && Char(peek(io)) ∈ whitespace
    i += 1
    read(io, UInt8)
  end
  return Stmt(i)
end

curstring(cur::Cursor) = "$(cur.line):$(cur.column)"

chartokens = [',', '(', ')', '{', '}']

function tokenise(io::IO)
  consume_ws(io)
  cur = cursor(io)
  eof(io) && return ('\0', cur)
  c = Char(peek(io))
  tk = c ∈ '0':'9' ? num_token(io) :
       c ∈ 'a':'z' || c ∈ 'A':'Z' ? symbol_token(io) :
       c == '\n' ? stmt_token(io) :
       c in opchars ? op_token(io) :
       c in chartokens ? (read(io, UInt8); c) :
       error("Unrecognised character $(sprint(show, c)) at $(curstring(cur))")
  return (tk, cur)
end

# LNR extension
Base.mark(io::LineNumberingReader) = mark(io.io)
Base.reset(io::LineNumberingReader) = reset(io.io)

struct TokenStream{I<:IO}
  io::IO
end

TokenStream(io::IO) = TokenStream{typeof(io)}(io)

TokenStream(s::String) = TokenStream(LineNumberingReader(IOBuffer(s)))

@forward TokenStream.io Base.mark, Base.reset, Base.position, Base.seek

Base.read(ts::TokenStream) = tokenise(ts.io)

function Base.peek(ts::TokenStream)
  mark(ts)
  x = read(ts)
  reset(ts)
  return x
end

# Parsing

function consume_stmts!(ts)
  while (t = peek(ts)[1]) isa Stmt
    read(ts)
  end
  return
end

brackets = Dict(
  '(' => ')',
  '[' => ']',
  '{' => '}')

function parse_brackets(ts, level, stop)
  xs = []
  while true
    peek(ts)[1] == stop && (read(ts); break)
    x = parse(ts, level)
    push!(xs, x)
    nt, pos = read(ts)
    nt == stop && break
    nt == ',' || error("Expected a delimiter at $(curstring(pos))")
  end
  return xs
end

function parse_atom(ts::TokenStream, level)
  consume_stmts!(ts)
  tk, cur = read(ts)
  tk isa String ? Symbol(tk) :
  tk == '(' ? Tuple(parse_brackets(ts, level, ')')) :
  tk
end

function parse_ex(ts::TokenStream, level)
  ex = parse_atom(ts, level)
  ex == :return && return Return(parse_ex(ts, level))
  while (nt = peek(ts)[1]) == '('
    args = parse_atom(ts, level)
    ex = Call(ex, args.args)
  end
  if (nt = peek(ts)[1]) ∈ Symbol.(operators) && (nt != :(:))
    read(ts)
    ex = Operator(nt, [ex, parse(ts, level)])
  end
  return ex
end

function parse_block(ts, name, level)
  args = []
  while true
    next = peek(ts)[1]
    !(next isa Stmt || next in (',', :(:))) || break
    push!(args, parse_ex(ts, level))
  end
  c, cur = read(ts)
  c == :(:) || error("$name block requires a colon at $(curstring(cur))")
  nt, _ = peek(ts)
  nt isa Stmt || return Block(name, args, [parse(ts, level)], true)
  inner = nt.indent
  inner <= level && return Block(name, args, [])
  exs = []
  while true
    nt, pos = peek(ts)
    nt == '\0' && break
    @assert nt isa Stmt
    nt.indent == inner || break
    push!(exs, parse(ts, inner))
  end
  return Block(name, args, exs)
end

function parse_if(ts, ex, level)
  cond = Any[ex.args[1]]
  body = Any[ex.block]
  while true
    m = position(ts)
    consume_stmts!(ts)
    peek(ts)[1] in ("else", "elseif") || (seek(ts, m); break)
    ex = parse_block(ts, Symbol(read(ts)[1]), level)
    push!(cond, ex.name == :elseif ? ex.args[1] : true)
    push!(body, ex.block)
  end
  return If(cond, body)
end

function parse(ts::TokenStream, level)
  ex = parse_ex(ts, level)
  next = peek(ts)[1]
  if ex isa Symbol && !(next isa Stmt || next in (',', ')', '\0'))
    ex = parse_block(ts, ex, 0)
    ex.name == :if && (ex = parse_if(ts, ex, 0))
    return ex
  end
  return ex
end

parse(s::String) = parse(TokenStream(s), 0)

macro vsx_str(x)
  QuoteNode(parse(x))
end
