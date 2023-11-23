struct Pipeline
  caches::Any
end

subcaches(p::Pipeline) = getfield(p, :caches)

function reset!(p::Pipeline; deps = [])
  print = copy(fingerprint(deps))
  for ch in subcaches(p)
    reset!(ch, deps = print)
    union!(print, fingerprint(ch))
  end
end

getindex(p::Pipeline, x) = subcaches(p)[end][x]

# Hack to prevent Juno completions error
Base.fieldnames(::Type{Pipeline}) = ()

macro Pipeline(exs...)
  @assert all(ex -> isexpr(ex, :(=)), exs)
  names = [@capture(ex, name_Symbol = val_) ? name : error("x = y")
           for ex in exs]
  :(let
    $(exs...)
    Pipeline(($([:($name = $name) for name in names]...),))
  end) |> esc
end

Base.getproperty(p::Pipeline, f::Symbol) = getproperty(subcaches(p), f)
