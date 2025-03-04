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

# TODO remove
function Base.union(a::RMethod, b::RMethod)
  @assert a === b
  return a
end

struct Methods
  imports::Caches.Ref{Vector{Tag}}
  methods::Caches.Dict{Tag,Vector{Union{RMethod,Tag}}}
end

Methods(path) = Methods(Caches.Ref(Tag[]), Caches.Dict{Tag,Vector{Union{RMethod,Tag}}}())

Base.getindex(m::Methods, k::Tag) = get(m.methods, k, m.imports[])

Caches.subcaches(m::Methods) = (m.imports, m.methods)

Base.copy(m::Methods) = Methods(copy(m.imports), copy(m.methods))

method!(ms::Methods, m::RMethod) =
  ms.methods[m.name] = Union{Tag,RMethod}[get(ms.methods, m.name, ms.imports[])..., m]

function import!(ms::Methods, mod::Tag)
  mod in ms.imports[] && return
  push!(ms.imports[], mod)
  for k in keys(ms.methods)
    ms.methods[k] = [ms.methods[k]..., mod]
  end
end

function Base.empty!(ms::Methods)
  ms.imports[] = Tag[]
  empty!(ms.methods)
end

Base.delete!(ms::Methods, k::Tag) = delete!(ms.methods, k)

struct RModule
  name::Tag
  defs::Caches.Dict{Symbol,Union{RAnno,Binding}}
  exports::Set{Symbol}
  methods::Methods
end

RModule(name) = RModule(name, Caches.Dict{Symbol,Union{RAnno,Binding}}(), Set{Symbol}(), Methods(name))

@forward RModule.defs Base.getindex, Base.setindex!, Base.haskey, Base.get, Base.get!

Caches.subcaches(m::RModule) = (m.defs,m.methods)

Base.copy(m::RModule) = RModule(m.name, copy(m.defs), copy(m.exports), copy(m.methods))

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

struct Modules
  mods::Dict{Tag,RModule}
end

Modules() = Modules(Dict{Tag,RModule}())

Base.copy(ms::Modules) = Modules(Dict(tag => copy(m) for (tag, m) in ms.mods))

module!(c::Modules, mod::RModule) = c.mods[mod.name] = mod
module!(c::Modules, mod::Tag) = get!(() -> RModule(mod), c.mods, mod)

@forward Modules.mods Base.getindex

Caches.subcaches(c::Modules) = values(c.mods)

Base.getindex(cx::Modules, b::Binding) = cx.mods[b.mod][b.name]

Base.setindex!(cx::Modules, T::RType, b::Binding) =
  cx.mods[b.mod][b.name] = T

function resolve_static(cx::Modules, b::Binding)
  val = cx[b]
  val isa Binding ? resolve_static(cx, val) : val
end

function methods(cx::Modules, name::Tag, mod::Tag = tag"", ms = RMethod[], seen = Set{Tag}())
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
  globals::Cache{Binding,Any}
  methods::EagerCache{Tag,Vector{RMethod}}
end

function Definitions(comp::Modules)
  globals = Cache{Binding,Any}() do b
    get(comp[b.mod].defs, b.name, ⊥)
  end
  methods = EagerCache{Tag,Vector{RMethod}}() do name
    Raven.methods(comp, name)
  end
  Definitions(globals, methods)
end

Caches.subcaches(ds::Definitions) = (ds.globals, ds.methods)

Base.getindex(d::Definitions, b::Binding) = d.globals[b]
Base.getindex(d::Definitions, m::Tag) = d.methods[m]

resolve_global(c, b) = b
resolve_global(c, b::Binding) = resolve_global(c, c[b])

# For debugging
struct FuncInfo
  name::Tag
  source::Union{Source,Nothing}
  trampoline::Bool
end

FuncInfo(name, source = nothing; trampoline = false) =
  FuncInfo(name, source, trampoline)
