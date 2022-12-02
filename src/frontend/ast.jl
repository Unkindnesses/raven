# AST Structure

module AST

using MacroTools: @forward
using LNR

abstract type Expr end

struct Meta <: Expr
  file::String
  loc::Cursor
  expr::Any
end

unwrap(ex) = ex
unwrap(ex::Meta) = unwrap(ex.expr)

isexpr(ex, T) =
  ex isa Meta ?
  isexpr(ex.expr, T) :
  ex isa T

struct Return <: Expr
  val
end

struct Break <: Expr end

struct Continue <: Expr end

struct List <: Expr
  args::Vector{Any}
end

struct Splat <: Expr
  expr::Any
end

struct Call <: Expr
  func
  args::Vector{Any}
end

struct Operator <: Expr
  op::Symbol
  args::Vector{Any}
end

struct Swap <: Expr
  op
end

struct Block <: Expr
  args::Vector{Any}
end

struct Syntax <: Expr
  name
  args::Vector{Any}
end

struct Quote <: Expr
  expr::Any
end

using MacroTools: @q

for T in [Return, List, Call, Operator, Syntax, Block]
  @eval Base.:(==)(a::$T, b::$T) = $(Base.Expr(:&&, [:(a.$f == b.$f) for f in fieldnames(T)]...))
end

meta(x) = nothing
meta(x::Meta) = x
meta(x::Syntax) = meta(x.name)

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
  _show(io, x.val)
end

_show(io::Ctx, x::Break) = print(io, "break")
_show(io::Ctx, x::Continue) = print(io, "continue")

function _show(io::Ctx, x::List)
  print(io, "[")
  join(io, repr.((io,), x.args), ", ")
  print(io, "]")
end

function _show(io::Ctx, x::Call)
  _show(io, x.func)
  print(io, "(")
  join(io, repr.((io,), x.args), ", ")
  print(io, ")")
end

function _show(io::Ctx, x::Splat)
  _show(io, x.expr)
  print(io, "...")
end

function _show(io::Ctx, x::Operator)
  print(io, "(")
  join(io, repr.((io,), x.args), " $(x.op) ")
  print(io, ")")
end

function _show(io::Ctx, x::Swap)
  print(io, "&")
  _show(io, x.op)
end

function _show(io::Ctx, x::Quote)
  print(io, "`")
  print(io, x.expr)
  print(io, "`")
end

function _show(io::Ctx, x::Block; meta = nothing)
  io′ = indent(io)
  print(io, "{")
  if meta != nothing
    showline(io, meta)
  end
  for x in x.args
    print(io, "\n", " "^io′.indent)
    _show(io′, x)
    if !isexpr(x, Syntax) && AST.meta(x) != nothing
      showline(io, AST.meta(x))
    end
  end
  print(io, "\n", " "^io.indent, "}")
end

function _show(io::Ctx, x::Syntax)
  _show(io, x.name)
  print(io, " ")
  for i in 1:length(x.args)
    if i == length(x.args)
      _show(io, unwrap(x.args[i])::Block, meta = meta(x))
    else
      _show(io, x.args[i])
      print(io, " ")
    end
  end
end

function _show(io::Ctx, x::Meta)
  _show(io, x.expr)
end

Base.show(io::IO, x::Expr) = _show(ShowContext(io), x)

function Base.show(io::IO, x::Meta)
  showline(io, x)
  println(io)
  show(io, x.expr)
end

end
