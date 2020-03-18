blockargtype(bl, i) = exprtype(bl.ir, arguments(bl)[i])

function casts!(ir)
  for bl in blocks(ir), br in branches(bl)
    isreturn(br) && continue # TODO: handle multiple returns
    for i = 1:length(arguments(br))
      S = exprtype(ir, arguments(br)[i])
      T = blockargtype(block(ir, br.block), i)
      S == T && continue
      arguments(br)[i] =
        push!(bl, IRTools.stmt(Base.Expr(:call, :cast, T, arguments(br)[i]),
                               type = T))
    end
  end
  return ir
end
