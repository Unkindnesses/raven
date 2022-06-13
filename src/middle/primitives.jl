# Primitives for type inference

partial_part(data::Union{Data,Primitive}, i::Integer) =
  0 <= i <= nparts(data) ? part(data, i) : ⊥

# TODO: HACK: we assume index != 0 when indexing dynamically.
# Should instead have a seperate `index` function that enforces this.
partial_part(data::Data, i::Type{<:Integer}) =
  reduce(union, parts(data))

partial_part(data::VData, i::Union{Int,Type{<:Integer}}) =
  i == 0 ? data.tag : data.parts

partial_nparts(x::Data) = nparts(x)
partial_nparts(::VData) = Int64

partial_widen(x::Primitive) = typeof(x)
partial_widen(x) = x

data_method = RMethod(:data, lowerpattern(rvx"args"), args -> data(parts(args)...), true)
part_method = RMethod(:part, lowerpattern(rvx"[data, i]"), partial_part, true)
nparts_method = RMethod(:nparts, lowerpattern(rvx"[x]"), partial_nparts, true)
datacat_method = RMethod(:datacat, lowerpattern(rvx"args"), args -> datacat(parts(args)...), true)
widen_method = RMethod(:widen, lowerpattern(rvx"[x]"), partial_widen, true)

function primitives!(mod)
  mod[Symbol("false")] = Int32(0)
  mod[Symbol("true")] = Int32(1)
  method!(mod, :data, data_method)
  method!(mod, :part, part_method)
  method!(mod, :nparts, nparts_method)
  method!(mod, :datacat, datacat_method)
  method!(mod, :widen, widen_method)

  for T in [Int64, Int32, Float64, Float32]
    mod[Symbol(T)] = Symbol(T)
  end
  return mod
end
