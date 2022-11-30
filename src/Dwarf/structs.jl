struct DIE
  tag::Tag
  attrs::Vector{Pair{Attr,Any}}
  children::Vector{DIE}
end

struct Abbrev
  tag::Tag
  attrs::Vector{Pair{Attr,Form}}
  children::Bool
end

function abbrev(d::DIE)
  Abbrev(d.tag, [k => form(v) for (k, v) in d.attrs], !isempty(d.children))
end

const byteforms =
  Dict(1 => FORM_data1,
       2 => FORM_data2,
       4 => FORM_data4,
       8 => FORM_data8)

form(x::AbstractString) = FORM_string
form(x::Union{Enum,Integer}) = byteforms[sizeof(x)]
