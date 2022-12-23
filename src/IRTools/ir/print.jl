import Base: show

function Base.show(io::IO, x::Variable)
  print(io, "%", x.id)
end

print_stmt(io::IO, ex::Expr) = print_stmt(io::IO, Val(ex.head), ex)
print_stmt(io::IO, ::Val, ex) = print(io, ex)
print_stmt(io::IO, ex) = print(io, ex)

function print_stmt(io::IO, ::Val{:branch}, b)
  if b == unreachable
    print(io, "unreachable")
  elseif isreturn(b)
    print(io, "return ", b.args[3])
  else
    print(io, "br $(b.args[1])")
    if !isempty(arguments(b))
      print(io, " (")
      join(io, arguments(b), ", ")
      print(io, ")")
    end
    b.args[2] != nothing && print(io, " if $(b.args[2])")
  end
end

const tab = "  "

function printargs(io::IO, args, types = [Any for arg in args])
  print(io, "(")
  for i = 1:length(args)
    print(io, args[i])
    types[i] != Any && print(io, " :: ", types[i])
    i != length(args) && print(io, ", ")
  end
  print(io, ")")
end

function show(io::IO, b::Block)
  indent = get(io, :indent, 0)
  bb = BasicBlock(b)
  print(io, tab^indent)
  print(io, b.id, ":")
  if !isempty(bb.args)
    print(io, " ")
    printargs(io, bb.args, bb.argtypes)
  end
  for (x, st) in b
    println(io)
    print(io, tab^indent, "  ")
    x == nothing || print(io, string("%", x.id), " = ")
    st.expr == nothing ? print(io, "nothing") :
      print_stmt(io, st.expr)
    st.type == Any || print(io, " :: ", st.type)
  end
end

function show(io::IO, ir::IR)
  show(io, block(ir, 1))
  for b in blocks(ir)[2:end]
    println(io)
    show(io, b)
  end
end
