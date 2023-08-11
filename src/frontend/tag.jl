struct Tag
  path::NTuple{N,Symbol} where N
end

Tag(t::Tag) = t
Tag(parts::Symbol...) = Tag((parts...,))
Tag(a::Tag, b::Tag) = Tag((a.path..., b.path...))
Tag(a, b) = Tag(Tag(a), Tag(b))

Base.string(id::Tag) = join(id.path, ".")

Base.Symbol(id::Tag) = Symbol(string(id))

function Base.show(io::IO, id::Tag)
  print(io, "tag\"")
  join(io, id.path, ".")
  print(io, "\"")
end

Tag(s::String) = Tag(Symbol.(split(s, ".", keepempty=false))...)

path(t::Tag) = Tag(t.path[1:end-1]...)
name(t::Tag) = t.path[end]

macro tag_str(ex)
  Tag(ex)
end

function modtag(mod::Tag, tag::String)
  prefix = startswith(tag, ".") ? mod : tag""
  return Tag(prefix, tag)
end
