using Raven, Test
using Raven: Or, Recursive, Recur, data, union, issubset

T1 = data(:Prepend, Or([data(:Empty), data(:Prepend, data(:Empty), Int64)]), Int64)
T2 = Or([data(:Empty), data(:Prepend, data(:Empty), Int64)])

T = union(T1, T2)

@test T == Recursive(Or([data(:Empty), data(:Prepend, Recur(), Int)]))

@test issubset(T1, T)
@test issubset(T2, T)
@test !issubset(T, T1)
@test issubset(T, T)

Tw = data(:Prepend, T, Int)

@test issubset(Tw, T)
