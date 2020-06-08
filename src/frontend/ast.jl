# AST Structure

abstract type Expr end

struct Return <: Expr
  val
end

struct Break <: Expr end

struct Continue <: Expr end

struct Tuple <: Expr
  args::Vector{Any}
end

struct Call <: Expr
  func
  args::Vector{Any}
end

struct Operator <: Expr
  op::Symbol
  args::Vector{Any}
end

struct Block <: Expr
  args::Vector{Any}
end

struct Syntax <: Expr
  name::Symbol
  args::Vector{Any}
end

struct Quote <: Expr
  expr::Any
end

using MacroTools: @q

for T in [Return, Tuple, Call, Operator, Syntax, Block]
  @eval Base.:(==)(a::$T, b::$T) = $(Base.Expr(:&&, [:(a.$f == b.$f) for f in fieldnames(T)]...))
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

_show(io::Ctx, x::Union{Symbol,Number,String}) = print(io, x)

function _show(io::Ctx, x::Return)
  print(io, "return ")
  _show(io, x.val)
end

_show(io::Ctx, x::Break) = print(io, "break")
_show(io::Ctx, x::Continue) = print(io, "continue")

function _show(io::Ctx, x::Tuple)
  print(io, "(")
  join(io, repr.((io,), x.args), ", ")
  length(x.args) == 1 && print(io, ',')
  print(io, ")")
end

function _show(io::Ctx, x::Call)
  _show(io, x.func)
  print(io, "(")
  join(io, repr.((io,), x.args), ", ")
  print(io, ")")
end

function _show(io::Ctx, x::Operator)
  print(io, "(")
  join(io, repr.((io,), x.args), " $(x.op) ")
  print(io, ")")
end

function _show(io::Ctx, x::Quote)
  print(io, "`")
  print(io, x.expr)
  print(io, "`")
end

function _show(io::Ctx, x::Block)
  io′ = indent(io)
  print(io, "{")
  for x in x.args
    print(io, "\n", " "^io′.indent)
    _show(io′, x)
  end
  print(io, "\n", " "^io.indent, "}")
end

function _show(io::Ctx, x::Syntax)
  _show(io, x.name)
  print(io, " ")
  for i in 1:length(x.args)
    _show(io, x.args[i])
    i == length(x.args) || print(io, " ")
  end
end

Base.show(io::IO, x::Expr) = _show(ShowContext(io), x)
