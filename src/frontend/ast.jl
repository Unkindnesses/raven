module AST

using MacroTools: @forward
import Base: ==, getindex, lastindex, length
using LNR

const Atom = Union{String,Symbol,Int64,Float64}

abstract type Head end

struct Meta
  file::String
  loc::Cursor
end

struct Token{T}
  value::T
  meta::Union{Meta,Nothing}
end

struct Expr{H}
  _args::Vector{Union{Expr,Token}}
  meta::Union{Meta,Nothing}
end

(a::Expr{H} == b::Expr{H}) where H = a._args == b._args

wrapToken(x::Atom) = Token(x, nothing)
wrapToken(x::Union{Expr,Token}) = x

unwrapToken(x::Token) = x.value
unwrapToken(x::Expr) = x

length(x::Expr) = length(x._args)
lastindex(x::Expr) = lastindex(x._args)

getindex(x::Expr, i::Integer) = unwrapToken(x._args[i])
getindex(x::Expr, i::Union{AbstractArray,Colon}) = unwrapToken.(x._args[i])

meta(x::Union{Expr,Token}) = x.meta

meta(x::Expr{H}, m::Meta) where H = Expr{H}(x._args, m)
meta(x::Token, m::Meta) = Token(x.value, m)
meta(x::Atom, m::Meta) = Token(x, m)

meta(x, args...) = meta(x, Meta(args...))

for T in :[Return, Break, Continue, List, Splat, Call, Operator, Swap, Block,
           Syntax, Quote].args
  @eval begin
    const $T = Expr{$(QuoteNode(T))}
    (::Type{$T})(args::Union{Expr,Token,Atom}...) = $T([wrapToken.(args)...], nothing)
  end
end

# Printing

struct ShowContext{IO}
  io::IO
  indent::Int
end

ShowContext(io::IO) = ShowContext(io, 0)

indent(cx::ShowContext) = ShowContext(cx.io, cx.indent+2)

@forward ShowContext.io Base.print, Base.println, Base.join

Base.repr(cx::ShowContext, x) = sprint(io -> _show(ShowContext(io, cx.indent), x))

const Ctx = ShowContext

showline(io, x::Meta) = print(io, " # ", basename(x.file), ":", x.loc.line)

_show(io::Ctx, x::Union{Symbol,Number,String}) = print(io, x)

function _show(io::Ctx, x::Return)
  print(io, "return ")
  _show(io, x[1])
end

_show(io::Ctx, x::Break) = print(io, "break")
_show(io::Ctx, x::Continue) = print(io, "continue")

function _show(io::Ctx, x::List)
  print(io, "[")
  join(io, repr.((io,), x[:]), ", ")
  print(io, "]")
end

function _show(io::Ctx, x::Call)
  _show(io, x[1])
  print(io, "(")
  join(io, repr.((io,), x[2:end]), ", ")
  print(io, ")")
end

function _show(io::Ctx, x::Splat)
  _show(io, x[1])
  print(io, "...")
end

function _show(io::Ctx, x::Operator)
  print(io, "(")
  join(io, repr.((io,), x[2:end]), " $(x[1]) ")
  print(io, ")")
end

function _show(io::Ctx, x::Swap)
  print(io, "&")
  _show(io, x[1])
end

function _show(io::Ctx, x::Quote)
  print(io, "`")
  print(io, x[1])
  print(io, "`")
end

function _show(io::Ctx, x::Block)
  io′ = indent(io)
  print(io, "{")
  if x.meta != nothing
    showline(io, x.meta)
  end
  for x in x[:]
    print(io, "\n", " "^io′.indent)
    _show(io′, x)
  end
  print(io, "\n", " "^io.indent, "}")
end

function _show(io::Ctx, x::Syntax)
  _show(io, x[1])
  print(io, " ")
  for i in 2:length(x)
    if i == length(x)
      _show(io, x[i]::Block)
    else
      _show(io, x[i])
      print(io, " ")
    end
  end
end

Base.show(io::IO, x::Expr) = _show(ShowContext(io), x)

end
