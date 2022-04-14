struct RMethod
  name::Symbol
  pattern
  args
  func
  partial::Bool
end

RMethod(name, pat, args, func) = RMethod(name, pat, args, func, false)

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
