Base.show(io::IO, i::Nop)         = print(io, "nop")
Base.show(io::IO, i::Const)       = print(io, WType(i), ".const ", i.val)
Base.show(io::IO, i::Local)       = print(io, "local.get ", i.id)
Base.show(io::IO, i::SetLocal)    = print(io, i.tee ? "local.tee " : "local.set ", i.id)
Base.show(io::IO, i::GetGlobal)   = print(io, "global.get ", i.id)
Base.show(io::IO, i::SetGlobal)   = print(io, "global.set ", i.id)
Base.show(io::IO, i::Op)          = print(io, i.name)
Base.show(io::IO, i::Call)        = print(io, "call \$", i.name)
Base.show(io::IO, i::Convert)     = print(io, i.to, ".", i.name, "/", i.from)
Base.show(io::IO, i::Drop)        = print(io, "drop")
Base.show(io::IO, i::Select)      = print(io, "select")
Base.show(io::IO, i::Branch)      = print(io, i.cond ? "br_if " : "br ", i.level)
Base.show(io::IO, i::Return)      = print(io, "return")
Base.show(io::IO, i::Unreachable) = print(io, "unreachable")

function Base.show(io::IO, i::CallIndirect)
  # TODO table idx
  print(io, "call_indirect")
  printvars(io, "param", i.sig.params)
  printvars(io, "result", i.sig.result)
end

printwasm(io, x, level) = show(io, x)

function showline(io::IO, li::LineInfo)
  printstyled(io, " ;; ", basename(li.src.file), ":", li.src.line, ":", li.src.col, color = 243)
  li.bp && printstyled(io, " 🔴", color = 243)
end

function printwasm_(io, xs, ss, level)
  @assert length(xs) == length(ss)
  for (x, s) in zip(xs, ss)
    print(io, "\n", "  "^(level))
    print(io, "(")
    printwasm(io, x, level)
    print(io, ")")
    s == nothing || showline(io, s)
  end
end

function printwasm(io, x::Block, level)
  print(io, "block")
  printwasm_(io, x.body, x.srcs, level+1)
end

function printwasm(io, x::Loop, level)
  print(io, "loop")
  printwasm_(io, x.body, x.srcs, level+1)
end

Base.show(io::IO, i::Union{Block,Loop}) = printwasm(io, i, 0)

function printwasm(io, x::Mem, level)
  print(io, "\n", "  "^(level))
  print(io, "(memory $(x.min))")    # TODO: add x.max
end

function printwasm(io, x::Data, level)
  print(io, "\n", "  "^(level))
  print(io, """(data (i32.const $(x.offset)) "$(String(x.data))"))""")
end

function printwasm(io, x::Export, level)
  print(io, "\n", "  "^(level))
  print(io, "(export \"$(x.as)\" (func \$$(x.name)))")
end

function printwasm(io, x::Import, level)
  print(io, "\n", "  "^(level))
  print(io, "(import \"$(x.mod)\" \"$(x.name)\" (func \$$(x.as)")
  printwasm(io, x.sig, level)
  print(io, "))")
end

function printwasm(io, x::Global, level)
  print(io, "\n", "  "^(level))
  print(io, "(global ")
  if x.mut
    print(io, "(mut ", x.type, ") ")
  else
    print(io, x.type, " ")
  end
  print(io, "(", x.init, "))")
end

function printvars(io, name, vs)
  if !isempty(vs)
    print(io, " (", name, " ")
    join(io, vs, " ")
    print(io, ")")
  end
end

function printwasm(io::IO, sig::Signature, level)
  printvars(io, "param", sig.params)
  printvars(io, "result", sig.result)
end

function printwasm(io::IO, f::Func, level)
  print(io, "\n", "  "^(level))
  print(io, "(func \$$(f.name)")
  printwasm(io, f.sig, level)
  !isempty(f.locals) && print(io, "\n", "  "^level, " ")
  printvars(io, "local", f.locals)
  printwasm_(io, f.body.body, f.body.srcs, level + 1)
  print(io, ")")
end

Base.show(io::IO, f::Func) = printwasm(io, f, 1)

function Base.show(io::IO, m::Module)
  print(io, "(module")
  foreach(p -> printwasm(io, p, 1), m.imports)
  foreach(p -> printwasm(io, p, 1), m.exports)
  foreach(p -> printwasm(io, p, 1), m.globals)
  foreach(p -> printwasm(io, p, 1), m.mems)
  foreach(p -> printwasm(io, p, 1), m.data)
  foreach(p -> printwasm(io, p, 1), m.funcs)
  println(io, ")")
end
