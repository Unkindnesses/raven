using Raven, Test
using Raven: Or, Recursive, Recur, pack, union, issubset

T1 = pack(:Prepend, Or([pack(:Empty), pack(:Prepend, pack(:Empty), Int64)]), Int64)
T2 = Or([pack(:Empty), pack(:Prepend, pack(:Empty), Int64)])

T = union(T1, T2)

@test T == Recursive(Or([pack(:Empty), pack(:Prepend, Recur(), Int)]))

@test issubset(T1, T)
@test issubset(T2, T)
@test !issubset(T, T1)
@test issubset(T, T)

Tw = pack(:Prepend, T, Int)

@test issubset(Tw, T)
