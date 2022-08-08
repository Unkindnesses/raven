# Primitives for type inference

partial_part(data::Union{Data,Primitive,Type{<:Primitive}}, i::Integer) =
  0 <= i <= nparts(data) ? part(data, i) : ⊥

# TODO: HACK: we assume index != 0 when indexing dynamically.
# Should instead have a seperate `index` function that enforces this.
partial_part(data::Data, i::Type{<:Integer}) =
  reduce(union, parts(data))

partial_part(data::VData, i::Union{Int,Type{<:Integer}}) =
  i == 0 ? data.tag : data.parts

partial_part(data::Or, i) =
  reduce(union, partial_part.(data.patterns, (i,)))

partial_nparts(x::Data) = nparts(x)
partial_nparts(::VData) = Int64

partial_widen(x::Primitive) = typeof(x)
partial_widen(x) = x

# Fast, approximate equality check; basically a stand-in for pointer equality.
# TODO extend to handle vdata
partial_shortcutEquals(a, b) =
  Int32(isvalue(a) && isvalue(b) && a == b)

# Needed by dispatchers, since a user-defined method would need runtime matching
# to deal with unions.
partial_isnil(x::Union{Primitive,Type{<:Primitive}}) = Int32(0)
partial_isnil(x::Data) = Int32(x == data(:Nil))
partial_isnil(x::Or) = any(==(data(:Nil)), x.patterns) ? Int32 : Int32(0)

# Duct tape until the thatcher algorithm works.
partial_notnil(x::Data) = tag(x) == :Nil ? ⊥ : x

function partial_notnil(x::Or)
  ps = filter(x -> tag(x) != :Nil, x.patterns)
  return length(ps) == 1 ? ps[1] : Or(ps)
end

partial_symstring(x::Symbol) = String(x)

data_method = RMethod(:data, lowerpattern(rvx"args"), args -> data(parts(args)...), true)
part_method = RMethod(:part, lowerpattern(rvx"[data, i]"), partial_part, true)
nparts_method = RMethod(:nparts, lowerpattern(rvx"[x]"), partial_nparts, true)
datacat_method = RMethod(:datacat, lowerpattern(rvx"args"), args -> datacat(parts(args)...), true)
widen_method = RMethod(:widen, lowerpattern(rvx"[x]"), partial_widen, true)
shortcutEquals_method = RMethod(:shortcutEquals, lowerpattern(rvx"[a, b]"), partial_shortcutEquals, true)

isnil_method = RMethod(:isnil, lowerpattern(rvx"[x]"), partial_isnil, true)
notnil_method = RMethod(:notnil, lowerpattern(rvx"[x]"), partial_notnil, true)
symstring_method = RMethod(:symstring, lowerpattern(rvx"[x: Symbol]"), partial_symstring, true)

function primitives!(mod)
  mod[Symbol("false")] = Int32(0)
  mod[Symbol("true")] = Int32(1)
  method!(mod, :data, data_method)
  method!(mod, :part, part_method)
  method!(mod, :nparts, nparts_method)
  method!(mod, :datacat, datacat_method)
  method!(mod, :widen, widen_method)
  method!(mod, :shortcutEquals, shortcutEquals_method)

  method!(mod, :isnil, isnil_method)
  method!(mod, :notnil, notnil_method)
  method!(mod, :symstring, symstring_method)
  return mod
end

# TODO handle primitive expansion by dispatch
