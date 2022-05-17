# HACK: belongs in IRTools.
function Base.replace!(pr::IRTools.Pipe, x, y)
  IRTools.substitute!(pr, x, IRTools.substitute(pr, y))
end
