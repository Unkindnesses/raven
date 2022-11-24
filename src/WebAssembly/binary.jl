# https://webassembly.github.io/spec/core/binary/instructions.html

# TODO make this a struct
const Sig = Pair{Vector{WType},Vector{WType}}

struct BinaryContext <: IO
  io::IO
  types::Dict{Sig,Int}
  funcs::Dict{Symbol,Int}
end

@forward BinaryContext.io Base.position, Base.seek

Base.write(cx::BinaryContext, x::UInt8) = write(cx.io, x)

BinaryContext(io::IO, cx::BinaryContext) =
  BinaryContext(io, cx.types, cx.funcs)

function BinaryContext(io::IO, m::Module)
  types = Dict{Sig,Int}()
  funcs = Dict{Symbol,Int}()
  i = 0 # assign ids to type signatures
  for f in vcat(m.imports, m.funcs)
    type = f.params => f.result
    haskey(types, type) && continue
    types[type] = i
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

function leb128(io::IO, x::Unsigned)
  while true
    byte = (x % UInt8) & 0x7f
    x >>= 7
    x == 0 || (byte |= 0x80)
    write(io, byte)
    x == 0 && break
  end
end

function leb128(io::IO, x::Signed)
  while true
    byte = (x % UInt8) & 0x7f
    x >>= 7
    sign = (byte & 0x40) == 0
    if (x == 0 && sign) || (x == -1 && !sign)
      write(io, byte)
      break
    else
      write(io, byte | 0x80)
    end
  end
end

u32(io::IO, x) = leb128(io, UInt32(x))

function name(io::IO, x)
  u32(io, sizeof(x))
  write(io, x)
end

# Instruction / type assignments

numtypes = Dict(i32 => 0x7f, i64 => 0x7e, f32 => 0x7d, f64 => 0x7c)

function instr(io::IO, x::Const)
  write(io, 0x41 + UInt8(WType(x)))
  if x.val isa Integer
    leb128(io, x.val)
  else
    write(io, x.val)
  end
end

# Modules

function header(io::IO)
  write(io, "\0asm")   # magic
  write(io, UInt32(1)) # version number
end

function withsize(f, io::BinaryContext)
  buf = BinaryContext(IOBuffer(), io)
  f(buf)
  u32(io, position(buf))
  write(io, read(seek(buf, 0)))
  return
end

function typevec(io::IO, ts)
  u32(io, length(ts))
  for t in ts
    write(io, numtypes[t])
  end
end

function functype(io::IO, a, b)
  write(io, 0x60)
  typevec(io, a)
  typevec(io, b)
end

function types(io::IO, m::Module)
  sigs = unique(f.params => f.result for f in vcat(m.imports, m.funcs))
  isempty(sigs) && return
  write(io, 0x01) # section id
  withsize(io) do io
    u32(io, length(sigs))
    for s in sigs
      functype(io, s...)
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
      u32(io, io.types[i.params => i.result])
    end
  end
end

function functions(io::BinaryContext, funcs)
  isempty(funcs) && return
  write(io, 0x03) # section id
  withsize(io) do io
    u32(io, length(funcs))
    for f in funcs
      u32(io, io.types[f.params => f.result])
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
  u32(io, length(f.locals))
  for t in f.locals
    u32(io, 1)
    write(io, numtypes[t])
  end
  for i in f.body.body
    instr(io, i)
  end
  write(io, 0x0B)
end

function code(io::IO, funcs)
  isempty(funcs) && return
  write(io, 0x0a) # section id
  withsize(io) do io
    u32(io, length(funcs))
    for f in funcs
      withsize(io) do io
        func(io, f)
      end
    end
  end
end

function binary(io::IO, m::Module)
  cx = BinaryContext(io, m)
  header(cx)
  types(cx, m)
  imports(cx, m.imports)
  functions(cx, m.funcs)
  memories(cx, m.mems)
  exports(cx, m.exports)
  code(cx, m.funcs)
end
