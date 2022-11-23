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
