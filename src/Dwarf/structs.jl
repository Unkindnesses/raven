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

const attrforms =
  Dict(AT_high_pc => (FORM_addr, UInt32),
       AT_low_pc  => (FORM_addr, UInt32),
       AT_stmt_list => (FORM_sec_offset, UInt32))

const byteforms =
  Dict(1 => FORM_data1,
       2 => FORM_data2,
       4 => FORM_data4,
       8 => FORM_data8)

form(x::AbstractString) = FORM_string
form(x::Union{Enum,Integer}) = byteforms[sizeof(x)]

function form(attr, v)
  haskey(attrforms, attr) || return form(v)
  f, T = attrforms[attr]
  @assert v isa T
  return f
end

function abbrev(d::DIE)
  Abbrev(d.tag, [k => form(k, v) for (k, v) in d.attrs], !isempty(d.children))
end

struct LineTable
  lines::Vector{Pair{UInt32,Union{Source,Nothing}}}
end

offset(lt::LineTable, δ) = LineTable([o-δ => s for (o, s) in lt.lines])
