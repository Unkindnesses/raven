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

@forward RModule.defs Base.getindex, Base.setindex!, Base.haskey

isprimitive(x::T, ::Type{T}) where T = true
isprimitive(::Type{T}, ::Type{T}) where T = true
isprimitive(x, ::Type) = false

function primitives!(mod)
  mod[Symbol("false")] = Int32(0)
  mod[Symbol("true")] = Int32(1)
  method!(mod, :data, RMethod(:data, lowerpattern(rvx"args")..., args -> data(args.parts[2:end]...), true))
  method!(mod, :part, RMethod(:part, lowerpattern(rvx"(data, i)")..., part, true))
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
