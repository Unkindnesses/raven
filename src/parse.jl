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
    c in 'a':'z' || c in 'A':'Z' || c in ('?','!') ||
      (seek(io, position(io)-1); break)
    write(sym, c)
  end
  seek(sym, 0)
  return String(read(sym))
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

chartokens = [':', ',', '(', ')', '{', '}']

function tokenise(io::IO)
  consume_ws(io)
  cur = cursor(io)
  eof(io) && return (:eof, cur)
  c = Char(peek(io))
  tk = c ∈ '0':'9' ? num_token(io) :
       c ∈ 'a':'z' || c ∈ 'A':'Z' ? symbol_token(io) :
       c == '\n' ? stmt_token(io) :
       c in chartokens ? (read(io, UInt8); c) :
       error("Unrecognised character $(sprint(show, c)) at $(curstring(cur))")
  return (tk, cur)
end

struct TokenStream{I<:IO}
  io::IO
  stack::Vector{Any}
end

TokenStream(io::IO) = TokenStream{typeof(io)}(io, [])

Base.read(ts::TokenStream) =
  isempty(ts.stack) ? tokenise(ts.io) : pop!(ts.stack)

function Base.push!(ts::TokenStream, x)
  push!(ts.stack, x)
  return ts
end

function Base.peek(ts::TokenStream)
  x = read(ts)
  push!(ts, x)
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
    x = parse(ts)
    push!(xs, x)
    nt, pos = read(ts)
    nt == stop && break
    nt == ',' || error("Expected a delimiter at $(curstring(pos))")
  end
  return xs
end

function parse_dict(ts)
  xs = []
  while true
    peek(ts)[1] == '}' && (read(ts); break)
    k = String(parse(ts))
    nt, pos = read(ts)
    nt == ':' || error("Expected a `:` at $(curstring(pos))")
    v = parse(ts)
    push!(xs, :($k=>$v))
    nt, pos = read(ts)
    nt == '}' && break
    nt == ',' || error("Expected a delimiter at $(curstring(pos))")
  end
  return Expr(:call, Dict, xs...)
end

function parse_block(ts, level)
  _, pos = read(ts)
  nt, _ = peek(ts)
  nt isa Stmt || error("Block at $(curstring(pos)) requires a newline")
  inner = nt.indent
  inner <= level && return Expr(:block)
  exs = []
  while true
    nt, pos = peek(ts)
    nt == :eof && break
    @assert nt isa Stmt
    nt.indent == inner || break
    push!(exs, parse(ts, inner))
  end
  return Expr(:block, exs...)
end

function parse_def(ts, level)
  sig = parse(ts)
  body = parse(ts)
  Expr(:function, sig, body)
end

function parse1(ts::TokenStream, level = 0)
  consume_stmts!(ts)
  tk, cur = read(ts)
  if tk == "loop"
    body = parse(ts, level)
    Expr(:while, true, body)
  elseif tk == "def"
    parse_def(ts, level)
  elseif tk isa String
    Symbol(tk)
  elseif tk == '('
    Expr(:tuple, parse_brackets(ts, level, ')')...)
  elseif tk == '{'
    parse_dict(ts)
  elseif tk == ':'
    push!(ts, (tk, cur))
    parse_block(ts, level)
  else
    return tk
  end
end

function parse(ts::TokenStream, level = 0)
  ex = parse1(ts)
  while (nt = peek(ts)[1]) == '('
    args = parse1(ts)
    ex = Expr(:call, ex, args.args...)
  end
  return ex
end

parse(s::String) = parse(TokenStream(LineNumberingReader(IOBuffer(s))))
