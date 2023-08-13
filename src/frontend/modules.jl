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
    mod[var] = Binding(from.name, var)
  end
end

@forward RModule.defs Base.getindex, Base.setindex!, Base.haskey, Base.get, Base.get!

function pathtag(p)
  @assert endswith(p, ".rv")
  Tag(join(split(p[1:end-3], "/"), "."))
end

struct Compilation
  mods::Dict{Tag,RModule}
end

Compilation() = Compilation(Dict{Tag,RModule}())

module!(c::Compilation, mod::RModule) = c.mods[mod.name] = mod
module!(c::Compilation, mod::Tag) = get!(() -> RModule(mod), c.mods, mod)

@forward Compilation.mods Base.getindex

function resolve_static(cx::Compilation, b::Binding)
  val = cx.mods[b.mod][b.name]
  val isa Binding ? resolve_static(cx, val) : val
end

struct Definitions
  globals::Cache{Binding,Any}
  methods::Cache{Tag,Vector{RMethod}}
end

function Definitions(comp::Compilation)
  globals = Cache{Binding,Any}() do ch, b
    get(comp[b.mod].defs, b.name, ⊥)
  end
  methods = Cache{Tag,Vector{RMethod}}() do ch, name
    comp[tag""].methods[name]
  end
  Definitions(globals, methods)
end

function resolve_binding(cx::Definitions, b::Binding)
  val = cx.globals[b]
  val isa Binding ? resolve_binding(cx, val) : b
end

resolve_static(cx::Definitions, b::Binding) = cx.globals[resolve_binding(cx, b)]

startmethod(def::Definitions) = def.methods[tag"common.core.main"][1]

# For debugging
struct FuncInfo
  name::Tag
  source::Union{Source,Nothing}
  trampoline::Bool
end

FuncInfo(name, source = nothing; trampoline = false) =
  FuncInfo(name, source, trampoline)
