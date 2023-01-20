struct DIE
  tag::Tag
  attrs::Vector{Pair{Attr,Any}}
  children::Vector{DIE}
end

DIE(tag::Tag, attrs = []) = DIE(tag, attrs, [])

struct Abbrev
  tag::Tag
  attrs::Vector{Pair{Attr,Form}}
  children::Bool
end

Base.hash(x::Abbrev, h::UInt) = Base.hash((x.tag, x.attrs, x.children), 0x0aa35d1caf09e6b0)

Base.:(==)(a::Abbrev, b::Abbrev) = (a.tag, a.attrs, a.children) == (b.tag, b.attrs, b.children)

const attrforms =
  Dict(AT_high_pc => (FORM_addr, UInt32),
       AT_low_pc  => (FORM_addr, UInt32),
       AT_stmt_list => (FORM_sec_offset, UInt32))

const byteforms =
  Dict(1 => FORM_data1,
       2 => FORM_data2,
       4 => FORM_data4,
       8 => FORM_data8)

form(x::Union{AbstractString,Symbol}) = FORM_string
form(x::Union{Enum,Integer}) = byteforms[sizeof(x)]
form(x::Bool) = FORM_flag

function form(attr, v)
  haskey(attrforms, attr) || return form(v)
  f, T = attrforms[attr]
  @assert v isa T
  return f
end

function abbrev(d::DIE)
  Abbrev(d.tag, [k => form(k, v) for (k, v) in d.attrs], !isempty(d.children))
end

function abbrevs!(die::DIE, as::Set{Abbrev})
  push!(as, abbrev(die))
  foreach(d -> abbrevs!(d, as), die.children)
  return as
end

abbrevs(die::DIE) = collect(abbrevs!(die, Set{Abbrev}()))

struct LineTable
  lines::Vector{Pair{UInt32,Union{Source,Nothing}}}
end

offset(lt::LineTable, δ) = LineTable([o-δ => s for (o, s) in lt.lines])
