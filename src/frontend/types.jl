# Bottom

struct Unreachable end
const ⊥ = Unreachable()

Base.show(io::IO, ::Unreachable) = print(io, "⊥")

reconstruct(::Unreachable) = (), _ -> ⊥

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

pack(x...) = any(==(⊥), x) ? ⊥ : Pack(x...)

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

vpack(tag, parts) =
  tag == ⊥ ? ⊥ :
  parts == ⊥ ? pack(tag) :
  VPack(tag, parts)

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

struct Onion
  types::NTuple{N,Any} where N
  function Onion(xs)
    isempty(xs) && return ⊥
    length(xs) == 1 && return only(xs)
    @assert !any(x -> x isa Onion, xs)
    return new((sort(collect(xs), lt = t_isless)...,))
  end
end

onion(xs...) = Onion([d for ds in xs for d in disjuncts(ds)])

reconstruct(x::Onion) = x.types, xs -> onion(xs...)

disjuncts(::Unreachable) = ()
disjuncts(x) = (x,)
disjuncts(x::Onion) = x.types

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

pack(ts::Onion, xs...) = error("unsupported")
vpack(ts::Onion, xs) = error("unsupported")

# Recursion

struct Recursive
  type::Any
  function Recursive(type)
    @assert !any(x -> x isa Recursive, disjuncts(type))
    return new(type)
  end
end

struct Recur end

Base.show(io::IO, T::Recursive) = print(io, "(T = ", T.type, ")")
Base.show(io::IO, ::Recur) = print(io, "T")

isrecursive(x) = any(x -> x isa Recursive, disjuncts(x))

occursin(x, y) = x == y || any(y -> occursin(x, y), reconstruct(y)[1])
occursin(x, y::Union{Recur,Recursive}) = x == y

simple_unroll(S, T::Recursive) = T
simple_unroll(S, T::Recur) = S

function simple_unroll(S, T)
  xs, re = reconstruct(T)
  re(simple_unroll.((S,), xs))
end

simple_unroll(T) = T
simple_unroll(T::Recursive) = simple_unroll(T, T.type)
simple_unroll(T::Onion) = Onion([x for S in disjuncts(T) for x in disjuncts(simple_unroll(S))])

# Size

typesize(::Unreachable) = 1
typesize(x::Union{Primitive,Type{<:Primitive}}) = 1
typesize(x::Recur) = 1
typesize(x::Onion) = 1 + sum(typesize, x.types)
typesize(x::Raven.Pack) = 1 + sum(typesize, x.parts)
typesize(x::Raven.VPack) = 1 + typesize(x.tag) + typesize(x.parts)
typesize(x::Recursive) = 1 + typesize(x.type)

# Order

_repr(x) = repr(x)
_repr(x::Int32) = "$x::Int32"

t_isless(x, y) = _repr(x) < _repr(y)

# Type errors (known bugs / unsupported cases)

struct TypeError
  name::String
end

# Subset

function _issubset(self, x, y)
  if x == ⊥
    true
  elseif y == ⊥
    false
  elseif x isa Recursive || y isa Recursive
    self(simple_unroll(x), simple_unroll(y))
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
  elseif x isa String
    self(RString(), y)
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

function subsetter()
  fp = Fixpoint(_ -> true) do self, (x, y)
    _issubset((x, y) -> self[(x, y)], x, y)
  end
  (x, y) -> fp[(x, y)]
end

issubset(x, y) = subsetter()(x, y)

# Disjoint

function _isdisjoint(self, x, y)
  if ⊥ in (x, y)
    true
  elseif x isa Recursive || y isa Recursive
    self(simple_unroll(x), simple_unroll(y))
  elseif x isa Onion || y isa Onion
    all(self(x, y) for x in disjuncts(x) for y in disjuncts(y))
  elseif x isa Primitive && y isa Primitive
    x !== y
  elseif x isa Primitive && y isa Type
    !(x isa y)
  elseif x isa Type && y isa Primitive
    !(y isa x)
  elseif x isa Type && y isa Type
    x != y
  elseif x isa Pack && y isa Pack
    nparts(x) != nparts(y) || any(self.(x.parts, y.parts))
  elseif x isa Pack && y isa VPack
    self(tag(x), tag(y)) || any(self.(parts(x), (y.parts,)))
  elseif x isa VPack && y isa Pack
    self(tag(x), tag(y)) || any(self.((x.parts,), parts(y)))
  elseif x isa VPack && y isa VPack
    self(tag(x), tag(y))
  else
    true
  end
end

function disjointer()
  fp = Fixpoint(_ -> false) do self, (x, y)
    _isdisjoint((x, y) -> self[(x, y)], x, y)
  end
  (x, y) -> fp[(x, y)]
end

isdisjoint(x, y) = disjointer()(x, y)

# Type keys

tokey(x::Tag) = x
tokey(x) = nothing

typekey(x) = tokey(tag(x))
typekey(x::Tag) = (tag(x), x)
typekey(x::Unreachable) = nothing

typekeys(x) = Set([typekey(x)])
typekeys(x::Recursive) = typekeys(x.type)
typekeys(x::Onion) = reduce(Base.union, typekeys.(x.types))
typekeys(x::Set) = x

overlapping(a, b) = !Base.isdisjoint(typekeys(a), typekeys(b))

partial_eltype(x::Pack; union = union) = reduce(union, parts(x), init = ⊥)
partial_eltype(x::VPack; union = union) = x.parts

function splitby(f, xs)
  ts = empty(xs)
  fs = empty(xs)
  for x in xs
    push!(f(x) ? ts : fs, x)
  end
  return ts, fs
end

splitby(f, xs::Tuple) = splitby(f, collect(xs))

# Unroll/Reroll

unroll_inner(S, T::Recursive) = T
unroll_inner(S, T::Recur) = S
unroll_inner(S, T::Onion) = onion((unroll_inner(S, d) for d in disjuncts(T))...)

function unroll_inner(S, T)
  xs, re = reconstruct(T)
  T = re(unroll.((S,), xs))
end

unroll(S, T) = unroll_inner(S, T)

unroll(S, T::Union{VPack,Onion}) = reroll(unroll_inner(S, T))

unroll(T) = T
unroll(T::Recursive) = unroll_inner(T, T.type)
unroll(T::Onion) = Onion([x for S in disjuncts(T) for x in disjuncts(unroll(S))])

isrecur(x, T) = !isdisjoint(x, T) && issubset(x, T)

function reroll_inner(T, x; self = reroll, seen)
  xs, re = reconstruct(x)
  ys = self.((T,), xs; seen)
  re(first.(ys)), reduce(Base.union, second.(ys), init = Set())
end

reroll_outer(T, x; seen) = reroll_inner(T, x; seen)

reroll_outer(T, x::Onion; seen) =
  reroll_inner(T, x, self = reroll_inner; seen)

function reroll_inner(T, x::Recursive; seen)
  x in seen && return nothing, Set()
  # TODO use unroll?
  y, ks = reroll_outer(T, simple_unroll(x); seen = Set([seen..., x]))
  isempty(ks) && return x, Set()
  occursin(nothing, y) && return x, Set()
  return y, ks
end

reroll(T, x::Recursive; seen) =
  isrecur(x, T) ? (Recur(), typekeys(x)) : reroll_inner(T, x; seen)

reroll(T, x::Union{VPack,Onion}; seen) =
  isrecur(x, T) ? (Recur(), typekeys(x)) :
  reroll_outer(T, x; seen)

function reroll(T, x; seen)
  y, ks = reroll_inner(T, x; seen)
  (!isempty(ks) || occursin(nothing, y)) && isrecur(x, T) ? (Recur(), typekeys(x)) : (y, ks)
end

reroll(T) = T

function reroll(T::Union{VPack,Onion})
  isrecursive(T) && return T
  xs = disjuncts(T)
  ys = reroll_inner.((T,), xs; seen = Set())
  ys = [(x, Base.union(k1, k2)) for ((x, k1), k2) in zip(ys, typekeys.(xs))]
  xs = []
  # Group by typekeys in common
  for (x, ks) in ys
    as, bs = splitby(((_, k),) -> !Base.isdisjoint(ks, k), xs)
    x = onion(x, [d for a in first.(as) for d in disjuncts(a)]...)
    ks = reduce(Base.union, second.(as), init = ks)
    xs = [bs..., (x, ks)]
  end
  xs = [occursin(Recur(), x) ? Recursive(x) : x for x in first.(xs)]
  R = onion(xs...)
  issubset(R, T) ? R : reroll(unroll(R))
end

# Lift
# (type to merge, subset present, recursion present)

function lift_inner(T, x; self = lift, seen)
  xs, _ = reconstruct(x)
  ys = self.((T,), xs; seen)
  reduce(_union, first.(ys), init = ⊥), any(second.(ys)), any(third.(ys))
end

lift_outer(T, x; seen) = lift_inner(T, x; seen)

lift_outer(T, x::Onion; seen) =
  lift_inner(T, x; self = lift_inner, seen)

function lift_inner(T, x::Recursive; seen)
  x in seen && return ⊥, false, true
  inner, s, r = lift_outer(T, unroll(x); seen = Set([seen..., x]))
  s && r ? (_union(x, inner), true, false) :
    (inner, s, false)
end

lift(T, x::Recursive; seen) =
  !isdisjoint(T, x) ? (x, true, false) : lift_inner(T, x; seen)

lift(T, x::Union{Onion,VPack}; seen) =
  !isdisjoint(T, x) ? (x, true, false) :
  isrecursive(x) ? lift_outer(T, x; seen) :
  lift_outer(_union(T, x), x; seen)

function lift(T, x; seen)
  inner, s, r = lift_inner(T, x; seen)
  (s || r) && !isdisjoint(T, x) ? (x, true, false) :
    (inner, s, r)
end

lift(T) = lift_outer(T, T, seen = Set())[1]

function lifted(T)
  L = _union(T, lift(T))
  issubset(L, T) ? L : lifted(L)
end

recursive(T) = T

function recursive(T::Union{VPack,Onion,Recursive}, orig = T)
  R = reroll(lifted(T))
  issubset(R, T) ? R : recursive(R, orig)
end

# Union

finite(T) = T
finite(T::Onion) = onion(finite.(disjuncts(T))...)

function finite(T::Recursive)
  term = simple_unroll(⊥, T.type)
  if !(term isa Union{Onion,VPack}) || isdisjoint(term, T)
    term = simple_unroll(term, T.type)
  end
  return simple_unroll(term, T.type)
end

function _union(x, y; self = _union)
  max(typesize(x), typesize(y)) < 100 || throw(TypeError("size"))
  x, y = finite.((x, y))
  if x == ⊥
    return y
  elseif y == ⊥
    return x
  elseif x isa Onion || y isa Onion
    ys = []
    for x in (disjuncts(y)..., disjuncts(x)...)
      while true
        xs, ys = splitby(y -> overlapping(x, y), ys)
        isempty(xs) && break
        x = reduce((x, y) -> _union(x, y; self), xs, init = x)
      end
      append!(ys, disjuncts(x))
    end
    z = onion(ys...)
    return z
  elseif typekey(x) === typekey(y)
    if x isa Primitive && y isa Primitive
      return x == y ? x : x isa String ? RString() : typeof(x)
    elseif x isa Type
      return x
    elseif y isa Type
      return y
    elseif x isa VPack || y isa VPack || nparts(x) != nparts(y)
      t = self(tag(x), tag(y))
      t == ⊥ && return ⊥
      parts = self(partial_eltype(x; union = self),
                   partial_eltype(y; union = self))
      parts == ⊥ && return pack(t)
      z = VPack(t, parts)
      return z
    else
      return pack([self(part(x, i), part(y, i)) for i = 0:nparts(x)]...)
    end
  else
    return onion(x, y)
  end
end

union(x, y) = recursive(_union(x, y, self = union))

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
  print(io, "vpack(")
  vprint(io, s.tag)
  print(io, ", ")
  vprint(io, s.parts)
  print(io, " ...)")
end

vprint(io::IO, ::Type{T}) where T = print(io, "$T")

vprint(io::IO, x::AST.Expr) = print(io, "`", x, "`")

Base.show(io::IO, d::Pack) = vprint(io, d)
Base.show(io::IO, d::VPack) = vprint(io, d)

# Show inside AST
AST._show(io::AST.Ctx, x::Pack) = show(io.io, x)
