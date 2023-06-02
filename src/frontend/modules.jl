struct Binding
  mod::Tag
  name::Symbol
end

struct Signature
  pattern
  args::Vector{Symbol}
  swap::Dict{Int,Symbol}
end

struct RMethod
  mod::Tag
  name::Tag
  sig::Signature
  func
  partial::Bool
end

RMethod(mod::Tag, name::Tag, pat, func) = RMethod(mod, name, pat, func, false)

RMethod(name::Tag, pat, func, partial = false) = RMethod(path(name), name, pat, func, partial)

Base.show(io::IO, meth::RMethod) = print(io, "RMethod($(string(meth.name)))")

function union(a::RMethod, b::RMethod)
  @assert a === b
  return a
end

struct Methods
  modules::Set{Tag}
  methods::Dict{Tag,Vector{RMethod}}
end

Methods(path) = Methods(Set([path]), Dict{Tag,Vector{RMethod}}())

Base.getindex(m::Methods, k::Tag) = m.methods[k]

method!(ms::Methods, m::RMethod) = push!(get!(ms.methods, m.name, RMethod[]), m)

function Base.merge!(a::Methods, b::Methods)
  imports = Set{Tag}()
  for (tag, meths) in b.methods, meth in meths
    if !(meth.mod in a.modules)
      method!(a, meth)
      push!(imports, meth.mod)
    end
  end
  union!(a.modules, imports)
  return a
end

struct RModule
  name::Tag
  defs::Dict{Symbol,Any}
  exports::Set{Symbol}
  methods::Methods
end

RModule(name) = RModule(name, Dict{Symbol,Any}(), Set{Symbol}(), Methods(name))

function method!(mod::RModule, m::RMethod)
  method!(mod.methods, m)
  return
end

function import!(mod::RModule, from::RModule, vars = [])
  from.name in mod.methods.modules || merge!(mod.methods, from.methods)
  for var in vars
    @assert var in from.exports
    # TODO should be a Binding, so it works with runtime values
    mod[var] = from[var]
  end
end

@forward RModule.defs Base.getindex, Base.setindex!, Base.haskey, Base.get, Base.get!

struct Compilation
  mods::Dict{Tag,RModule}
end

function module!(c::Compilation, mod)
  c.mods[mod.name] = mod
  return
end

@forward Compilation.mods Base.getindex

main(comp::Compilation) = comp[tag""]

function Compilation()
  c = Compilation(Dict())
  return c
end

# For debugging
struct FuncInfo
  name::Tag
  source::Union{Source,Nothing}
  trampoline::Bool
end

FuncInfo(name, source = nothing; trampoline = false) =
  FuncInfo(name, source, trampoline)
