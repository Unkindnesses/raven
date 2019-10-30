using Raven, Test
using Raven: @rvx_str, lowerpattern, match, data

p, = lowerpattern(Raven.main, rvx"(x, y)")

@test match(p, data(:Tuple, 1, 2)) == Dict(:x=>1,:y=>2)
@test match(p, data(:Tuple, 1, 2, 3)) == nothing
