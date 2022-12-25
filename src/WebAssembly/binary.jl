using ..Raven.Dwarf: leb128, LineTable, offset

# https://webassembly.github.io/spec/core/binary/instructions.html

struct BinaryContext <: IO
  io::IO
  types::Dict{Signature,Int}
  funcs::Dict{Symbol,Int}
end

@forward BinaryContext.io Base.position, Base.seek

Base.write(cx::BinaryContext, x::UInt8) = write(cx.io, x)

BinaryContext(io::IO, cx::BinaryContext) =
  BinaryContext(io, cx.types, cx.funcs)

function BinaryContext(io::IO, m::Module)
  types = Dict{Signature,Int}()
  funcs = Dict{Symbol,Int}()
  i = 0 # assign ids to type signatures
  for f in vcat(m.imports, m.funcs)
    haskey(types, f.sig) && continue
    types[f.sig] = i
    i += 1
  end
  i = 0 # assign ids to function names
  for f in vcat(m.imports, m.funcs)
    name = f isa Func ? f.name : f.as
    @assert !haskey(funcs, name)
    funcs[name] = i
    i += 1
  end
  return BinaryContext(io, types, funcs)
end

# Numeric values

u32(io::IO, x) = leb128(io, UInt32(x))

function u32(x)
  io = IOBuffer()
  u32(io, x)
  return read(seek(io, 0))
end

function name(io::IO, x)
  u32(io, sizeof(x))
  write(io, x)
end

# Instruction / type assignments

numtypes = Dict(i32 => 0x7f, i64 => 0x7e, f32 => 0x7d, f64 => 0x7c)

include("opcodes.jl")

function instr(io::IO, ::Unreachable)
  write(io, 0x00)
end

function instr(io::IO, bl::Block, lt)
  write(io, 0x02)
  write(io, 0x40) # empty type
  for (i, s) in zip(bl.body, bl.srcs)
    s == nothing || push!(lt.lines, position(io) => s)
    instr(io, i, lt)
  end
  write(io, 0x0b) # end
end

function instr(io::IO, bl::Loop, lt)
  write(io, 0x03)
  write(io, 0x40) # empty type
  for (i, s) in zip(bl.body, bl.srcs)
    s == nothing || push!(lt.lines, position(io) => s)
    instr(io, i, lt)
  end
  write(io, 0x0b) # end
end

instr(io, x, lt) = instr(io, x)

function instr(io::IO, br::Branch)
  write(io, 0x0c + br.cond)
  u32(io, br.level)
end

function instr(io::IO, ::Return)
  write(io, 0x0f)
end

function instr(io::IO, c::Call)
  write(io, 0x10)
  u32(io, io.funcs[c.name])
end

function instr(io::IO, v::Local)
  write(io, 0x20)
  u32(io, v.id)
end

function instr(io::IO, v::SetLocal)
  write(io, 0x21 + v.tee)
  u32(io, v.id)
end

function instr(io::IO, x::GetGlobal)
  write(io, 0x23)
  u32(io, x.id)
end

function instr(io::IO, x::SetGlobal)
  write(io, 0x24)
  u32(io, x.id)
end

function instr(io::IO, x::Const)
  write(io, 0x41 + UInt8(WType(x)))
  if x.val isa Integer
    leb128(io, x.val)
  else
    write(io, x.val)
  end
end

function instr(io::IO, x::Op)
  write(io, opcodes[x])
end

# Modules

function header(io::IO)
  write(io, "\0asm")   # magic
  write(io, UInt32(1)) # version number
end

function withsize(f, io::BinaryContext)
  buf = BinaryContext(IOBuffer(), io)
  f(buf)
  size = position(buf)
  u32(io, size)
  write(io, read(seek(buf, 0)))
  return size
end

function typevec(io::IO, ts)
  u32(io, length(ts))
  for t in ts
    write(io, numtypes[t])
  end
end

function functype(io::IO, s::Signature)
  write(io, 0x60)
  typevec(io, s.params)
  typevec(io, s.result)
end

function custom(f, io::IO, nm)
  write(io, 0x00) # section id
  withsize(io) do io
    name(io, nm)
    f(io)
  end
end

function types(io::IO, m::Module)
  sigs = unique(f.sig for f in vcat(m.imports, m.funcs))
  isempty(sigs) && return
  write(io, 0x01) # section id
  withsize(io) do io
    u32(io, length(sigs))
    for s in sigs
      functype(io, s)
    end
  end
end

function imports(io::BinaryContext, imps)
  isempty(imps) && return
  write(io, 0x02) # section id
  withsize(io) do io
    u32(io, length(imps))
    for i in imps
      name(io, i.mod)
      name(io, i.name)
      write(io, 0x00) # func import
      u32(io, io.types[i.sig])
    end
  end
end

function functions(io::BinaryContext, funcs)
  isempty(funcs) && return
  write(io, 0x03) # section id
  withsize(io) do io
    u32(io, length(funcs))
    for f in funcs
      u32(io, io.types[f.sig])
    end
  end
end

function memories(io::IO, mems)
  isempty(mems) && return
  @assert length(mems) == 1
  m = mems[1]
  write(io, 0x05) # section id
  withsize(io) do io
    u32(io, length(mems))
    if m.max == nothing
      write(io, 0x00)
      u32(io, m.min)
    else
      write(io, 0x01)
      u32(io, m.min)
      u32(io, m.max)
    end
  end
end

function globals(io::BinaryContext, gs)
  isempty(gs) && return
  write(io, 0x06) # section id
  withsize(io) do io
    u32(io, length(gs))
    for g in gs
      write(io, numtypes[g.type])
      write(io, UInt8(g.mut))
      instr(io, g.init)
      write(io, 0x0B) # end
    end
  end
end

function exports(io::BinaryContext, exs)
  isempty(exs) && return
  write(io, 0x07)
  withsize(io) do io
    u32(io, length(exs))
    for ex in exs
      name(io, ex.as)
      write(io, 0x00) # func export
      u32(io, io.funcs[ex.name])
    end
  end
end

function func(io::IO, f)
  lt = LineTable([])
  push!(lt.lines, position(io) => f.meta)
  u32(io, length(f.locals))
  for t in f.locals
    u32(io, 1)
    write(io, numtypes[t])
  end
  for (i, s) in zip(f.body.body, f.body.srcs)
    s == nothing || push!(lt.lines, position(io) => s)
    instr(io, i, lt)
  end
  write(io, 0x0B)
  return lt
end

function code(io::IO, funcs)
  table = LineTable([])
  isempty(funcs) && return 0, table
  write(io, 0x0a) # section id
  sz = withsize(io) do io
    u32(io, length(funcs))
    for f in funcs
      lt = nothing
      sz = withsize(io) do io
        lt = func(io, f)
      end
      append!(table.lines, offset(lt, -(position(io)-sz)).lines)
    end
  end
  return sz, table
end

function names(io::IO, m)
  custom(io, "name") do io
    write(io, 0x01) # func map
    withsize(io) do io
      u32(io, length(m.imports) + length(m.funcs))
      for (i, f) in enumerate(vcat(m.imports, m.funcs))
        u32(io, i-1)
        name(io, f isa Func ? f.name : f.as)
      end
    end
  end
end

function debuginfo(sz)
  Dwarf.DIE(Dwarf.TAG_compile_unit,
            [Dwarf.AT_producer => "raven version 0.0.0",
             Dwarf.AT_language => Dwarf.LANG_C99,
             Dwarf.AT_stmt_list => UInt32(0),
             Dwarf.AT_low_pc => UInt32(0),
             Dwarf.AT_high_pc => UInt32(sz)],
            [])
end

function dwarf(io::IO, lt, sz)
  dbg = debuginfo(sz)
  custom(io, ".debug_info") do io
    Dwarf.debug_info(io, dbg)
  end
  custom(io, ".debug_abbrev") do io
    Dwarf.debug_abbrev(io, dbg)
  end
  custom(io, ".debug_line") do io
    Dwarf.debug_line(io, lt)
  end
end

function binary(io::IO, m::Module; path)
  cx = BinaryContext(io, m)
  header(cx)
  types(cx, m)
  imports(cx, m.imports)
  functions(cx, m.funcs)
  memories(cx, m.mems)
  globals(cx, m.globals)
  exports(cx, m.exports)
  sz, lt = code(cx, m.funcs)
  names(cx, m)
  dwarf(cx, lt, sz)
  return
end
