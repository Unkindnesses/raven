using Raven, Test
using Raven: @rvx_str, lowerpattern, match, data, rtuple

const main = Raven.RModule()
Raven.interpreter_primitives!(main)
Raven.loadfile(main, joinpath(@__DIR__, "../base/base.rv"))

p, = lowerpattern(rvx"(x, y)")

@test match(main, p, rtuple(1, 2)) == Dict(:x=>1,:y=>2)
@test match(main, p, rtuple(1, 2, 3)) == nothing

p, = lowerpattern(rvx"data(`Foo`, x)")

@test match(main, p, data(:Foo, 1)) == Dict(:x=>1)
@test match(main, p, data(:Bar, 1)) == nothing
