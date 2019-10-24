using Raven, Test
using Raven: @rvx_str, lowerpattern, match, rstruct

p, = lowerpattern(Raven.main, rvx"(x, y)")

@test match(p, rstruct(:Tuple, 1, 2)) == Dict(:x=>1,:y=>2)
@test match(p, rstruct(:Tuple, 1, 2, 3)) == nothing
