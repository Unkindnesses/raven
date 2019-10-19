using Vespa, Test
using Vespa: @vsx_str, lowerpattern, match, vstruct

p, = lowerpattern(Vespa.main, vsx"(x, y)")

@test match(p, vstruct(:Tuple, 1, 2)) == Dict(:x=>1,:y=>2)
@test match(p, vstruct(:Tuple, 1, 2, 3)) == nothing
