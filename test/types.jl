using Raven, Test
using Raven: @tag_str, Or, Recursive, Recur, pack, union, issubset, unroll

T1 = Or([pack(tag"Empty"), pack(tag"Prepend", pack(tag"Empty"), Int64)])
T2 = pack(tag"Prepend", Or([pack(tag"Empty"), pack(tag"Prepend", pack(tag"Empty"), Int64)]), Int64)

T = union(T1, T2)

@test T == Recursive(Or([pack(tag"Empty"), pack(tag"Prepend", Recur(), Int)]))

@test union(T2, T1) == T

@test issubset(T1, T)
@test issubset(T2, T)
@test !issubset(T, T1)
@test issubset(T, T)

@test union(T, T) == T

Tw = pack(tag"Prepend", T, Int)

@test issubset(Tw, T)

@test issubset(unroll(T), T)
@test issubset(T, unroll(T))
