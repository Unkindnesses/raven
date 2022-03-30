blockargtype(mod::RModule, bl, i) = exprtype(mod, bl.ir, arguments(bl)[i])

function casts!(mod::RModule, ir)
  for bl in blocks(ir), br in branches(bl)
    isreturn(br) && continue # TODO: handle multiple returns
    for i = 1:length(arguments(br))
      S = exprtype(mod, ir, arguments(br)[i])
      T = blockargtype(mod, block(ir, br.block), i)
      S == T && continue
      arguments(br)[i] =
        push!(bl, IRTools.stmt(Base.Expr(:call, :cast, T, arguments(br)[i]),
                               type = T))
    end
  end
  return ir
end
