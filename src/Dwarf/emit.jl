# Values

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

leb128(io::IO, x::Enum) = leb128(io, UInt64(x))

function withsize(f, io::IO)
  buf = IOBuffer()
  f(buf)
  write(io, UInt32(position(buf)))
  write(io, read(seek(buf, 0)))
  return
end

function emit(io::IO, s::AbstractString)
  write(io, s)
  write(io, 0x00)
end

emit(io::IO, s::Union{Enum,Integer}) = write(io, s)

# Abbrev section

function debug_abbrev(io::IO, info::DIE)
  @assert isempty(info.children)
  x = abbrev(info)
  leb128(io, UInt32(1)) # abbreviation code
  leb128(io, x.tag) # tag_compile_unit
  write(io, x.children) # children
  for (k, v) in x.attrs
    leb128(io, k)
    leb128(io, v)
  end
  write(io, 0x00) # end attributes
end

function debug_info(io, info::DIE)
  @assert isempty(info.children)
  withsize(io) do io
    write(io, UInt16(4)) # DWARF version
    write(io, UInt32(0)) # debug_abbrev_offset
    write(io, 0x04) # address_size
    leb128(io, UInt32(1)) # compile unit abbrev
    for (k, v) in info.attrs
      emit(io, v)
    end
  end
end
