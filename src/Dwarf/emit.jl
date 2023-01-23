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

emit(io::IO, s::Symbol) = emit(io, string(s))

emit(io::IO, s::Union{Enum,Integer}) = write(io, s)

function emit(io::IO, die::DIE, as)
  leb128(io, UInt32(as[abbrev(die)])) # abbreviation code
  for (k, v) in die.attrs
    emit(io, v) # attribute value
  end
  if !isempty(die.children)
    for child in die.children
      emit(io, child, as)
    end
    leb128(io, UInt32(0)) # NULL entry terminates children
  end
end

# Abbrev section

function debug_abbrev(io::IO, as)
  for (i, x) in enumerate(as)
    leb128(io, UInt32(i)) # abbreviation code
    leb128(io, x.tag)
    write(io, x.children)
    for (k, v) in x.attrs
      leb128(io, k)
      leb128(io, v)
    end
    write(io, 0x0000) # end attributes
  end
  write(io, 0x00) # end abbreviations
end

function debug_info(io, info::DIE, as)
  as = Dict(a => i for (i, a) in enumerate(as))
  withsize(io) do io
    write(io, UInt16(4)) # DWARF version
    write(io, UInt32(0)) # debug_abbrev_offset
    write(io, 0x04) # address_size
    emit(io, info, as)
  end
end

function ln_end_sequence(io)
  write(io, 0x00)
  leb128(io, 0x01)
  write(io, 0x01)
end

function debug_line(io, lt)
  files = unique(s.src.file for (o, s) in lt.lines if s != nothing)
  withsize(io) do io
    write(io, UInt16(4)) # version
    withsize(io) do io # header
      write(io, 0x01) # minimum_instruction_length
      write(io, 0x01) # maximum_operations_per_instruction
      write(io, false) # default_is_stmt
      write(io, Int8(-3)) # line_base
      write(io, Int8(12)) # line_range
      write(io, Int8(13)) # opcode_base
      # standard_opcode_lengths
      for i in Int8.([0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1])
        write(io, i)
      end
      # dirs go here
      write(io, 0x00)
      for file in files
        emit(io, isabspath(file) ? file : normpath(joinpath(pwd(), file)))
        leb128(io, UInt32(1)) # dir
        leb128(io, UInt32(0)) # last modified
        leb128(io, UInt32(0)) # file size
      end
      write(io, 0x00)
    end
    lines = [UInt32(0)=>nothing, lt.lines...]
    # TODO track offset separately, so the loop can `continue` without getting
    # confused
    for i = 2:length(lines)
      o, s = lines[i-1]
      o′, s′ = lines[i]
      s == nothing && ((o, s) = (UInt32(0), LineInfo(Source(files[1], 1, 0), false)))
      write(io, 0x02) # advance_pc
      leb128(io, UInt32(o′ - o))
      if s′ == nothing
        ln_end_sequence(io)
      else
        if s′.src.line != s.src.line
          write(io, 0x03) # advance_line
          leb128(io, s′.src.line - s.src.line)
        end
        if s′.src.file != s.src.file
          write(io, 0x04) # set_file
          leb128(io, UInt32(findfirst(==(s′.src.file), files)))
        end
        if s′.src.col != s.src.col
          write(io, 0x05) # set_column
          leb128(io, UInt32(s′.src.col))
        end
        if s′.bp != s.bp
          write(io, 0x06) # negate_stmt
        end
        write(io, 0x01) # copy
      end
    end
  end
end
