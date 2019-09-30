using Vespa, Test
using Vespa: PrimitiveHole, main, return_type

ir = main.methods[:pow][1].func
@test return_type(ir, PrimitiveHole{Int}(), PrimitiveHole{Int}()) ==
  PrimitiveHole{Int}()
