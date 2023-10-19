struct Pipeline
  caches::Vector{Any}
end

function reset!(p::Pipeline; deps = [])
  print = copy(fingerprint(deps))
  for ch in p.caches
    reset!(ch, deps = print)
    union!(print, fingerprint(ch))
  end
end

getindex(p::Pipeline, x) = p.caches[end][x]
