using Raven, Test
using Raven: @id_str, Or, Recursive, Recur, pack, union, issubset, unroll

T1 = Or([pack(id"Empty"), pack(id"Prepend", pack(id"Empty"), Int64)])
T2 = pack(id"Prepend", Or([pack(id"Empty"), pack(id"Prepend", pack(id"Empty"), Int64)]), Int64)

T = union(T1, T2)

@test T == Recursive(Or([pack(id"Empty"), pack(id"Prepend", Recur(), Int)]))

@test issubset(T1, T)
@test issubset(T2, T)
@test !issubset(T, T1)
@test issubset(T, T)

@test union(T, T) == T

Tw = pack(id"Prepend", T, Int)

@test issubset(Tw, T)

@test issubset(unroll(T), T)
@test issubset(T, unroll(T))
