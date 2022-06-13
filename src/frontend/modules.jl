struct Signature
  pattern
  args::Vector{Symbol}
end

struct RMethod
  name::Symbol
  sig::Signature
  func
  partial::Bool
end

RMethod(name, pat, func) = RMethod(name, pat, func, false)

Base.show(io::IO, meth::RMethod) = print(io, "RMethod($(meth.name))")

struct RModule
  defs::Dict{Symbol,Any}
  methods::Dict{Symbol,Vector{RMethod}}
end

RModule() = primitives!(RModule(Dict{Symbol,Any}(), Dict{Symbol,IR}()))

function method!(mod::RModule, name::Symbol, m::RMethod)
  mod.defs[name] = name
  push!(get!(mod.methods, name, RMethod[]), m)
  return
end

@forward RModule.defs Base.getindex, Base.setindex!, Base.haskey, Base.get
