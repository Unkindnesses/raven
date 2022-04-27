blockargtype(mod::RModule, bl, i) = exprtype(mod, bl.ir, arguments(bl)[i])

function isreachable(bl)
  for (v, st) in bl
    st.type == ⊥ && return false
  end
  return true
end

function casts!(mod::RModule, ir)
  for bl in blocks(ir)
    if !isreachable(bl)
      empty!(branches(bl))
      continue
    end
    for br in branches(bl)
      isreturn(br) && continue # TODO: handle multiple returns
      for i = 1:length(arguments(br))
        S = exprtype(mod, ir, arguments(br)[i])
        T = blockargtype(mod, block(ir, br.block), i)
        S == T && continue
        arguments(br)[i] =
          push!(bl, IRTools.stmt(xcall(:cast, T, arguments(br)[i]),
                                 type = T))
      end
    end
  end
  return ir
end
