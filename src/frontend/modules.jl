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
  name::Tag
  sig::Signature
  func
  partial::Bool
end

RMethod(name::Tag, pat, func) = RMethod(name, pat, func, false)

Base.show(io::IO, meth::RMethod) = print(io, "RMethod($(string(meth.name)))")

function union(a::RMethod, b::RMethod)
  @assert a === b
  return a
end

struct Methods
  imports::Vector{Tag}
  methods::Dict{Tag,Vector{Union{RMethod,Tag}}}
end

Methods(path) = Methods([], Dict())

Base.getindex(m::Methods, k::Tag) = get(m.methods, k, m.imports)

method!(ms::Methods, m::RMethod) =
  push!(get!(ms.methods, m.name, Union{RMethod,Tag}[ms.imports...]), m)

function import!(ms::Methods, mod::Tag)
  mod in ms.imports && return
  push!(ms.imports, mod)
  for (k, v) in ms.methods
    push!(v, mod)
  end
end

function Base.empty!(ms::Methods)
  empty!(ms.imports)
  empty!(ms.methods)
end

struct RModule
  name::Tag
  defs::Caches.Dict{Symbol,Any}
  exports::Set{Symbol}
  methods::Methods
end

RModule(name) = RModule(name, Caches.Dict{Symbol,Any}(), Set{Symbol}(), Methods(name))

@forward RModule.defs Base.getindex, Base.setindex!, Base.haskey, Base.get, Base.get!

Caches.subcaches(m::RModule) = (m.defs,)

function method!(mod::RModule, m::RMethod)
  method!(mod.methods, m)
  return
end

function import!(mod::RModule, from::RModule, vars = [])
  import!(mod.methods, from.name)
  for var in vars
    @assert var in from.exports
    mod[var] = Binding(from.name, var)
  end
end

function Base.empty!(mod::RModule)
  empty!(mod.defs)
  empty!(mod.exports)
  empty!(mod.methods)
end

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

Caches.subcaches(c::Compilation) = values(c.mods)

function resolve_static(cx::Compilation, b::Binding)
  val = cx.mods[b.mod][b.name]
  val isa Binding ? resolve_static(cx, val) : val
end

function methods(cx::Compilation, name::Tag, mod::Tag = tag"", ms = RMethod[], seen = Set{Tag}())
  for m in cx[mod].methods[name]
    if m isa Tag
      m in seen && continue
      push!(seen, m)
      methods(cx, name, m, ms, seen)
    else
      push!(ms, m)
    end
  end
  return ms
end

struct Definitions
  globals::EagerCache{Binding,Any}
  methods::Cache{Tag,Vector{RMethod}}
end

function Definitions(comp::Compilation)
  globals = EagerCache{Binding,Any}() do ch, b
    get(comp[b.mod].defs, b.name, ⊥)
  end
  methods = Cache{Tag,Vector{RMethod}}() do ch, name
    Raven.methods(comp, name)
  end
  Definitions(globals, methods)
end

Caches.subcaches(ds::Definitions) = (ds.globals, ds.methods)

function resolve_binding(cx::Definitions, b::Binding)
  val = cx.globals[b]
  val isa Binding ? resolve_binding(cx, val) : b
end

resolve_static(cx::Definitions, b::Binding) = cx.globals[resolve_binding(cx, b)]

# For debugging
struct FuncInfo
  name::Tag
  source::Union{Source,Nothing}
  trampoline::Bool
end

FuncInfo(name, source = nothing; trampoline = false) =
  FuncInfo(name, source, trampoline)
