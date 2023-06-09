using Raven, Test
using Raven.Parse: PrecedenceTable, Left, Right, None, inverse

function transitive(t::PrecedenceTable)
  trans = true
  N = length(t.ops)
  for i = 1:N, j = 1:N
    i == j || (trans &= t.table[i, j] == inverse(t.table[j, i]))
    for k = 1:N
      ab, bc, ac = t.table[i, j], t.table[j, k], t.table[i, k]
      ab == bc != None && (trans &= ab == ac)
    end
  end
  return trans
end

@test transitive(Raven.Parse.table)
