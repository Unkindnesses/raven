using MacroTools
using MacroTools: @capture, @q

isbits(T) =
  T isa Union ? isbits(T.a) && isbits(T.b) :
  T.name == Tuple.name ? all(isbits, T.parameters) :
  isbitstype(T)

function fieldtype(ex)
  @capture(ex, _::T_ | _)
  return something(T, Any)
end

params(ex) = isexpr(ex, :curly) ? ex.args[2:end] : []

function freetypevars(ex)
  if isexpr(ex, Symbol)
    [ex]
  elseif !isexpr(ex)
    []
  elseif isexpr(ex, :(.))
    freetypevars(ex.args[1])
  elseif isexpr(ex, :curly, :tuple)
    reduce(vcat, freetypevars.(ex.args))
  else
    error("unrecognised type expression $ex")
  end
end

function ifelse(clauses, default=nothing)
  return foldr(((cond, body), els) -> Expr(:if, cond, body, els), clauses; init=default)
end

@noinline throw_not_set(::Type{T}, f) where {T} =
  error("Field $f is not active for type $(T)")
@noinline tag_not_found(::Type{T}, tag) where {T} =
  error("Malformed instance of union type $(T): Unexpected tag value: $(tag)")

function field end
function fields end
function fieldidx end
function unsafe_getproperty end
function dispatch end

macro union(ex)
  @capture(ex, struct T_ <: ParentType_
    fields__
  end | struct T_
    fields__
  end) || error("@union struct ...")
  types = Dict(namify(f) => fieldtype(f) for f in fields)
  fields = namify.(fields)
  if isnothing(ParentType)
    ParentType = Any
  end
  @assert length(fields) >= 1 || error("Union must have at least one field")
  forbidden = [namify(T), namify.(params(T))...]
  unboxed = filter(x -> isdisjoint(freetypevars(types[x]), forbidden), fields)
  type = @q struct $(esc(T)) <: $(esc(ParentType))
    tag::UInt8
    bits::Storage
    ptrs::P($(esc(namify(T))), $(esc.(params(T))...))
    $([
      @q function $(esc(T))(::Val{$(QuoteNode(f))}, value) where {$(esc.(params(T))...)}
        f_val = convert($(esc(types[f])), value)
        if $(f in unboxed) && isbits($(esc(types[f])))
          new($(UInt8(i)), f_val, nothing)
        else
          new($(UInt8(i)), nothing, f_val)
        end
      end
      for (i, f) in enumerate(fields)
    ]...)
  end
  :(let
    types = Dict($([:($(QuoteNode(f)) => $(esc(types[f]))) for f in unboxed]...))
    unboxed = [f for (f, T) in types if isbits(T)]
    Storage = Union{Nothing,[types[f] for f in unboxed]...}
    P($(esc(namify(T))), $(esc.(params(T))...)) = Union{Nothing,$([:($(QuoteNode(f)) in unboxed ? Union{} : $(esc(types[f]))) for f in fields]...)}
    $type
    $(esc(T))(; kw...) where {$(esc.(params(T))...)} = $(esc(T))(Val(only(kw)[1]), only(kw)[2])
    Unions.isunion(::Type{<:$(esc(namify(T)))}) = true
    Unions.fields(x::$(esc(T))) where {$(esc.(params(T))...)} = ($(QuoteNode.(fields)...),)
    Unions.fieldidx(::Type{<:$(esc(T))}, f::Symbol) where {$(esc.(params(T))...)} =
      getfield(($([:($f = $(UInt8(i))) for (i, f) in enumerate(namify.(fields))]...),), f)
    $([
      :(function Unions.unsafe_getproperty(x::$(esc(T)), ::Val{$(QuoteNode(f))}) where {$(esc.(params(T))...)}
        $(if f in unboxed
          :(if isbits($(esc(types[f])))
            return getfield(x, :bits)::$(esc(types[f]))
          end)
        end)
        getfield(x, :ptrs)::$(esc(types[f]))
      end)
      for f in fields
    ]...)
    @inline function Unions.dispatch(func::F, ::Type{T}, tag::UInt8) where {F, T<:$(esc(namify(T)))}
      return $(ifelse(
        [(:(tag === $(UInt8(i))), :(func(Val($(QuoteNode(f))))))
         for (i, f) in enumerate(namify.(fields))],
        :(tag_not_found(T, tag))))
    end
    Unions.dispatch(func::F, x::T) where {F,T<:$(esc(namify(T)))} = dispatch(func, T, getfield(x, :tag))
    Unions.peel(func::F, x::T) where {F,T<:$(esc(namify(T)))} = dispatch(f -> func(unsafe_getproperty(x, f)), T, getfield(x, :tag))
    function Base.getproperty(x::$(esc(T)), f::Symbol) where {$(esc.(params(T))...)}
      fieldidx(typeof(x), f) == getfield(x, :tag) ||
        Unions.throw_not_set(typeof(x), f)
      return unsafe_getproperty(x, Val(f))
    end
    Base.:(==)(a::$(esc(namify(T))), b::$(esc(namify(T)))) = ueq(a, b)
    function Base.show_default(io::IO, x::$(esc(T))) where {$(esc.(params(T))...)}
      show(io, typeof(x))
      print(io, "($(field(x)) = ")
      show(io, peel(x))
      print(io, ")")
      return
    end
    nothing
  end)
end

peel(x) = peel(identity, x)

function ueq(a::T, b::T) where {T}
  getfield(a, :tag) == getfield(b, :tag) &&
  dispatch(a) do f
    unsafe_getproperty(a, f) == unsafe_getproperty(b, f)
  end
end

field(x) = fields(x)[getfield(x, :tag)]

function isfield(x, f::Symbol)
  @assert f in fields(x)
  return fieldidx(typeof(x), f) === getfield(x, :tag)
end

isfield(x, f, g, h...) = isfield(x, f) || isfield(x, g, h...)
