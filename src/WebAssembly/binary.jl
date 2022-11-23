# https://webassembly.github.io/spec/core/binary/instructions.html

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

function withsize(f, io::IO)
  buf = IOBuffer()
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

function types(io::IO, funcs)
  isempty(funcs) && return
  sigs = [f.params => f.result for f in funcs] # TODO unique
  write(io, 0x01) # section id
  withsize(io) do io
    u32(io, length(sigs))
    for s in sigs
      functype(io, s...)
    end
  end
end

function functions(io::IO, funcs)
  isempty(funcs) && return
  write(io, 0x03) # section id
  withsize(io) do io
    u32(io, length(funcs))
    for (i, f) in enumerate(funcs)
      u32(io, i-1)
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
  header(io)
  types(io, m.funcs)
  functions(io, m.funcs)
  memories(io, m.mems)
  code(io, m.funcs)
end
