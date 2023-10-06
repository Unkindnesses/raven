struct Pipeline
  caches::Vector{Any}
end

function reset!(p::Pipeline)
  print = Set{NFT}()
  for ch in p.caches
    reset!(ch, deps = print)
    union!(print, fingerprint(ch))
  end
end

getindex(p::Pipeline, x) = p.caches[end][x]
