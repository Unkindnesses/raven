# Primitives for type inference
# Most of these should eventually end up in Raven itself, via partial functions

isprimitive(x::T, ::Type{T}) where T = true
isprimitive(::Type{T}, ::Type{T}) where T = true
isprimitive(x, ::Type) = false

function partial_part(data, i)
  if i isa Integer
    return part(data, i)
  else
    # TODO: HACK: we assume index != 0 when indexing dynamically.
    # Should instead have a seperate `index` function that enforces this.
    reduce(union, data.parts[2:end])
  end
end

function primitives!(mod)
  mod[Symbol("false")] = Int32(0)
  mod[Symbol("true")] = Int32(1)
  mod[:__backendWasm] = Int32(0)
  method!(mod, :data, RMethod(:data, lowerpattern(rvx"args")..., args -> data(args.parts[2:end]...), true))
  method!(mod, :part, RMethod(:part, lowerpattern(rvx"(data, i)")..., partial_part, true))
  method!(mod, :nparts, RMethod(:nparts, lowerpattern(rvx"(x,)")..., nparts, true))
  # TODO: this is a hacky fallback
  method!(mod, Symbol("isa?"), RMethod(Symbol("isa?"), lowerpattern(rvx"(x, T)")..., (x, T) -> Int32(tag(x) == T)))

  partial_widen(x::Primitive) = typeof(x)
  partial_widen(x) = x
  method!(mod, :widen, RMethod(:widen, lowerpattern(rvx"(x,)")..., partial_widen, true))

  for T in [Int64, Int32, Float64, Float32]
    mod[Symbol(T)] = Symbol(T)
    # TODO: hack
    method!(mod, Symbol("isa?"), RMethod(Symbol("isa?"), lowerpattern(parse("(x, `$T`)"))..., x -> Int32(isprimitive(x, T))))
  end
  mod[:PrimitiveString] = :PrimitiveString
  method!(mod, Symbol("isa?"), RMethod(Symbol("isa?"), lowerpattern(rvx"(x, `PrimitiveString`)")..., x -> Int32(x isa String)))
  return mod
end
