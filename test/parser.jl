using Vespa, Test
using Vespa: Call, Block, parse

@test Vespa.parse("foo(x, y)") == Call(:foo, [:x, :y])
