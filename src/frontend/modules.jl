struct Signature
  pattern
  args::Vector{Symbol}
  swap::Dict{Int,Symbol}
end

struct RMethod
  name::Symbol
  sig::Signature
  func
  partial::Bool
end

RMethod(name, pat, func) = RMethod(name, pat, func, false)

Base.show(io::IO, meth::RMethod) = print(io, "RMethod($(meth.name))")

function union(a::RMethod, b::RMethod)
  @assert a === b
  return a
end

struct RModule
  defs::Dict{Symbol,Any}
  methods::Dict{Tag,Vector{RMethod}}
end

RModule() = primitives!(RModule(Dict{Symbol,Any}(), Dict{Symbol,IR}()))

function method!(mod::RModule, name::Symbol, m::RMethod)
  mod.defs[name] = Tag(name)
  push!(get!(mod.methods, Tag(name), RMethod[]), m)
  return
end

@forward RModule.defs Base.getindex, Base.setindex!, Base.haskey, Base.get

struct FuncInfo
  name::Tag
  source::Union{Source,Nothing}
  trampoline::Bool
end

FuncInfo(name, source = nothing; trampoline = false) =
  FuncInfo(name, source, trampoline)
