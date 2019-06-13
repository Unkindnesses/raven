# AST Structure

abstract type Expr end

struct Return <: Expr
  val
end

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

struct If <: Expr
  cond::Vector{Any}
  body::Vector{Any}
end

struct Block <: Expr
  name::Symbol
  args::Vector{Any}
  block::Vector{Any}
  short::Bool
end

Block(name, args, block) = Block(name, args, block, false)

using MacroTools: @q

for T in [Return, Tuple, Call, If, Operator, Block]
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

function _show(io::Ctx, x::Tuple)
  print(io, "(")
  join(io, repr.((io,), x.args), ", ")
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

function _show(io::Ctx, x::If)
  _show(io, Block(:if, [x.cond[1]], x.body[1]))
  for i = 2:length(x.cond)
    print(io, "\n", " "^io.indent)
    if x.cond[i] == true
      _show(io, Block(:else, [], x.body[i]))
    else
      _show(io, Block(:elseif, [x.cond[i]], x.body[i]))
    end
  end
end

function _show(io::Ctx, x::Block)
  _show(io, x.name)
  for arg in x.args
    print(io, " ")
    _show(io, arg)
  end
  print(io, ":")
  if x.short
    print(io, " ")
    _show(io, x.block[1])
  else
    io = indent(io)
    for i = 1:length(x.block)
      print(io, "\n", " "^io.indent)
      _show(io, x.block[i])
    end
  end
end

Base.show(io::IO, x::Expr) = _show(ShowContext(io), x)
