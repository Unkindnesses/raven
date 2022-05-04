# After type inference, we lower to a form where reference and union types are
# represented with explicit pointers and tags, eg a `VData` becomes a (size,
# pointer) tuple. This allows us to compile methods like `part`, `datacat` and
# casts, whose implementation needs to access those internals.
#
# It makes sense for those methods to be represented as functions, to avoid
# code duplication and so that casting can be recursive. And they can still
# participate in optimisations (mainly inlining).
#
# After this lowering all code works with primitive values and does explicit
# memory management, so the job of the backend code generator is simple.

struct Compilation
  mod::RModule
  frames::IdDict{Any,IR}
end

Compilation(mod::RModule) = Compilation(mod, IdDict{Any,IR}())

function lowerir(inf::Inference)
  comp = Compilation(inf.mod)
  for (k, fr) in inf.frames
    comp.frames[k] = fr.ir
  end
  return comp
end
