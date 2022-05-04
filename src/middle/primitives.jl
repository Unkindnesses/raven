# Primitives for type inference

function partial_part(data, i)
  if i isa Integer
    return part(data, i)
  else
    # TODO: HACK: we assume index != 0 when indexing dynamically.
    # Should instead have a seperate `index` function that enforces this.
    # Implement both as partials
    reduce(union, parts(data))
  end
end

part_method = RMethod(:part, lowerpattern(rvx"[data, i]")..., partial_part, true)

function primitives!(mod)
  mod[Symbol("false")] = Int32(0)
  mod[Symbol("true")] = Int32(1)
  method!(mod, :data, RMethod(:data, lowerpattern(rvx"args")..., args -> data(parts(args)...), true))
  method!(mod, :part, part_method)
  method!(mod, :nparts, RMethod(:nparts, lowerpattern(rvx"[x]")..., nparts, true))
  method!(mod, :datacat, RMethod(:datacat, lowerpattern(rvx"args")..., args -> datacat(parts(args)...), true))

  partial_widen(x::Primitive) = typeof(x)
  partial_widen(x) = x
  method!(mod, :widen, RMethod(:widen, lowerpattern(rvx"[x]")..., partial_widen, true))

  for T in [Int64, Int32, Float64, Float32]
    mod[Symbol(T)] = Symbol(T)
  end
  return mod
end
