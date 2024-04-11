# Primitives

Primitive = Union{Int64,Int32,Float64,Float32,Tag,String}

JSObject() =
  options().jsalloc ?
    pack(tag"common.JSObject", pack(tag"common.Ref", pack(tag"common.Ptr", Int32))) :
    pack(tag"common.JSObject", Int32)

RString() = pack(tag"common.String", JSObject())

const fromSymbol = Dict{Tag,Type}()

for T in :[Int64, Int32, Float64, Float32, Tag].args
  local tag = Tag(tag"common.core", T)
  @eval fromSymbol[$tag] = $T
  @eval part(x::Union{$T,Type{$T}}, i::Integer) =
          i == 0 ? $tag :
          i == 1 ? x :
          error("Tried to access part $i of 1")
  @eval nparts(x::Union{$T,Type{$T}}) = 1
  @eval allparts(x::Union{$T,Type{$T}}) = ($tag, x)
end

part(s::String, i::Integer) =
  i == 0 ? tag"common.String" :
  i == 1 ? JSObject() :
  error("Tried to access part $i of 1")

allparts(s::String) = (tag"common.String", JSObject())

nparts(s::String) = 1

isvalue(x) = false
isvalue(x::Primitive) = true

reconstruct(x::Union{Primitive,Type{<:Primitive}}) = (), _ -> x

# Packs

struct Pack{N}
  parts::NTuple{N,Any}
  Pack(x...) = new{length(x)}(x)
end

pack(x...) = Pack(x...)

nil = pack(tag"common.Nil")

nparts(x::Pack) = length(x.parts)-1
part(x::Pack, i) = x.parts[i+1]
parts(x) = x.parts[2:end]
allparts(x::Pack) = x.parts
isvalue(xs::Pack) = all(isvalue, parts(xs))

Base.getindex(x::Pack, i::Integer) = x.parts[i+1]
Base.getindex(x::Pack, i::AbstractVector) = pack(x.parts[i.+1]...)
Base.lastindex(x::Pack) = lastindex(x.parts)
Base.iterate(x::Pack, st...) = iterate(x.parts, st...)

tag(x) = partial_part(x, 0)

reconstruct(x::Pack) = x.parts, ps -> pack(ps...)

struct VPack # variable width
  tag::Any
  parts::Any
end

vpack(tag, parts) = VPack(tag, parts)

tag(x::VPack) = x.tag

rlist(xs...) = pack(tag"common.List", xs...)

packcat(x) = x
packcat(x, y) = pack(tag(x), parts(x)..., parts(y)...)
packcat(x, y, z, zs...) = packcat(packcat(x, y), z, zs...)

packcat(x::Pack, y::Pack) = invoke(packcat, Tuple{Any,Any}, x, y)

packcat(x::Union{VPack,Pack}, y::Union{VPack,Pack}) =
  VPack(tag(x), union(partial_eltype(x), partial_eltype(y)))

reconstruct(x::VPack) = (x.tag, x.parts), args -> vpack(args...)

const SimpleType = Union{Primitive,Type{<:Primitive},Pack,VPack}

# Abstract Types

struct Unreachable end
const ⊥ = Unreachable()

Base.show(io::IO, ::Unreachable) = print(io, "⊥")

struct Onion
  types::NTuple{N,Any} where N
  function Onion(xs)
    length(xs) == 1 && return only(xs)
    @assert !isempty(xs) && !any(x -> x isa Onion, xs)
    return new((sort(collect(xs), lt = t_isless)...,))
  end
end

reconstruct(x::Onion) = x.types, Onion

function Base.show(io::IO, or::Onion)
  for i = 1:length(or.types)
    i == 1 || print(io, " | ")
    show(io, or.types[i])
  end
end

function splittags(f, x::Onion)
  tags = filter(t -> t isa Tag, x.types)
  t = filter(t -> !(t isa Tag), x.types)
  result = Any[f(t) for t in tags]
  isempty(t) || push!(result, f(Onion(t)))
  return Onion(result)
end

pack(ts::Onion, xs...) = splittags(tag -> Pack(tag, xs...), ts)
vpack(ts::Onion, xs) = splittags(tag -> VPack(tag, xs), ts)

# Recursion

struct Recursive
  type::Any
end

struct Recur end

Base.show(io::IO, T::Recursive) = print(io, "T = ", T.type)
Base.show(io::IO, ::Recur) = print(io, "T")

unroll(S, T::Recursive) = T
unroll(S, T::Recur) = S

function unroll(S, T)
  xs, re = reconstruct(T)
  re(unroll.((S,), xs))
end

unroll(T) = T
unroll(T::Recursive) = unroll(T, T.type)

# Order

_repr(x) = repr(x)
_repr(x::Int32) = "$x::Int32"

t_isless(x, y) = _repr(x) < _repr(y)

# Subset

function _issubset(self, x, y)
  if x == ⊥
    true
  elseif y == ⊥
    false
  elseif x isa Recursive || y isa Recursive
    self(unroll(x), unroll(y))
  elseif x isa Onion
    all(self(T, y) for T in x.types)
  elseif y isa Onion
    any(self(x, T) for T in y.types)
  elseif x isa Primitive && y isa Primitive
    x === y
  elseif x isa Primitive && y isa Type
    x isa y
  elseif x isa Type && y isa Type
    x <: y
  elseif x isa Pack && y isa Pack
    nparts(x) == nparts(y) && all(self.(x.parts, y.parts))
  elseif x isa Pack && y isa VPack
    self(tag(x), tag(y)) && all(self.(parts(x), (y.parts,)))
  elseif x isa VPack && y isa VPack
    self(tag(x), tag(y)) && self(x.parts, y.parts)
  else
    false
  end
end

function issubset(x, y)
  fp = Fixpoint(_ -> true) do self, (x, y)
    _issubset((x, y) -> self[(x, y)], x, y)
  end
  return fp[(x, y)]
end

# Recursion widening

# TODO unroll/fixpoint?
recursive(T, x::Recursive) = issubset(x, T) ? (Recur(), true) : (x, false)

function _recursive(T, x)
  xs, re = reconstruct(x)
  xs = recursive.((T,), xs)
  return re(first.(xs)), any(x[2] for x in xs)
end

recursive(T, x) =
  typeof(T) == typeof(x) && issubset(x, T) ?
    (Recur(), true) :
    _recursive(T, x)

const enable_recursion = Ref(true)

recursive(T) = T

function recursive(T::Union{Onion,VPack})
  enable_recursion[] || return T
  T, r = _recursive(T, T)
  return r ? Recursive(T) : T
end

# Union

tokey(x::Tag) = x
tokey(x) = nothing

typekey(x) = tokey(tag(x))
typekey(x::Tag) = (tag(x), x)

partial_eltype(x::Pack; union = union) = reduce(union, parts(x), init = ⊥)
partial_eltype(x::VPack; union = union) = x.parts

function _union(self, x, y)
  if x == ⊥
    return y
  elseif y == ⊥
    return x
  elseif x isa Recursive || y isa Recursive
    # TODO merge only overlapping recursions
    return self(unroll(x), unroll(y))
  elseif y isa Onion
    return reduce(self, y.types, init = x)
  elseif x isa Onion
    ps = x.types
    i = findfirst(x -> typekey(x) === typekey(y), ps)
    i == nothing && return Onion((ps..., y))
    T = Onion(j == i ? self(y, ps[j]) : ps[j] for j = 1:length(ps))
    return recursive(T)
  elseif typekey(x) === typekey(y)
    if x isa Primitive && y isa Primitive
      return x == y ? x : x isa String ? RString() : typeof(x)
    elseif x isa Type
      return x
    elseif y isa Type
      return y
    elseif x isa VPack || y isa VPack || nparts(x) != nparts(y)
      return recursive(VPack(self(tag(x), tag(y)),
                             self(partial_eltype(x; union = self),
                                  partial_eltype(y; union = self))))
    else
      return pack(self(tag(x), tag(y)), (self(part(x, i), part(y, i)) for i = 1:nparts(x))...)
    end
  else
    return recursive(Onion((x, y)))
  end
end

# TODO use cached `issubset`
function union(x, y)
  fp = Fixpoint(((x, y),) -> x) do self, (x, y)
    _union((x, y) -> self[(x, y)], x, y)
  end
  return fp[(x, y)]
end

# Internal symbols

symbolValues(x::Union{Primitive,Type{<:Primitive},Pack}) = []
symbolValues(x::Tag) = [x]
symbolValues(x::Onion) = reduce(vcat, map(symbolValues, x.types))

# Raven value -> compiler type

rvtype(x::Tag) = fromSymbol[x]

function rvtype(x::Pack)
  if tag(x) == tag"common.List"
    pack(tag(x), rvtype.(parts(x))...)
  elseif tag(x) == tag"common.Pack"
    pack(rvtype.(parts(x))...)
  elseif tag(x) == tag"common.Literal"
    return part(x, 1)
  else
    error("Unrecognised type $x")
  end
end

# Printing

vprint(io::IO, x) = show(io, x)

const printers = Dict{Symbol,Any}()

printers[:List] = function (io::IO, s::Pack)
  print(io, "[")
  join(io, [sprint(vprint, x) for x in parts(s)], ", ")
  print(io, "]")
end

printers[:Pair] = function (io, s)
  vprint(io, part(s, 1))
  print(io, " => ")
  vprint(io, part(s, 2))
end

function vprint(io::IO, s::Pack)
  isempty(s.parts) && return print(io, "pack()")
  haskey(printers, tag(s)) && return printers[tag(s)](io, s)
  print(io, "pack(")
  join(io, [sprint(vprint, x) for x in s.parts], ", ")
  print(io, ")")
end

function vprint(io::IO, s::VPack)
  print(io, "pack(")
  vprint(io, s.tag)
  print(io, ", ")
  vprint(io, s.parts)
  print(io, " ...)")
end

vprint(io::IO, ::Type{T}) where T = print(io, "_: $T")

vprint(io::IO, x::AST.Expr) = print(io, "`", x, "`")

Base.show(io::IO, d::Pack) = vprint(io, d)
Base.show(io::IO, d::VPack) = vprint(io, d)

# Show inside AST
AST._show(io::AST.Ctx, x::Pack) = show(io.io, x)
