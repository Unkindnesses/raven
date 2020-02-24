using Raven, Test
using Raven: @rv_str, @rvx_str, main, lowerpattern, match, data

p, = lowerpattern(rvx"(x, y)")

@test match(main, p, rv"(1, 2)") == Dict(:x=>1,:y=>2)
@test match(main, p, rv"(1, 2, 3)") == nothing

p, = lowerpattern(rvx"data(`Foo`, x)")

@test match(main, p, data(:Foo, 1)) == Dict(:x=>1)
@test match(main, p, data(:Bar, 1)) == nothing
