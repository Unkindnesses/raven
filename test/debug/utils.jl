struct LineInfo
  file::String
  line::Int
  column::Int
  bp::Bool
end

dwarfdump(f) = String(read(`llvm-dwarfdump $f`))

dwarf_verify(f) = String(read(`llvm-dwarfdump --verify $f`))

headers(f) = String(read(`wasm-objdump -h $f`))

disassembly(f) = String(read(`wasm-objdump -d $f`))

code_offset(f) =
  parse(UInt32, Base.match(r"Code start=([\w\d]+)", headers(f)).captures[1])

function callsites(wasm, func)
  dis = disassembly(wasm)
  ms = eachmatch(Regex("([\\w\\d]+):.*\\| call [\\d]+ <$func>"), dis)
  [parse(UInt32, "0x" * m.captures[1]) for m in ms]
end

function linetable(f)
  s = String(read(`llvm-dwarfdump --debug-line $f`))
  files = eachmatch(r"file_names\[[\s\d]+\]:\s+name: \"(.*)\"", s)
  files = [m.captures[1] for m in files]
  lines = eachmatch(r"(0x[\w\d]+)\s+(\d+)\s+(\d+)\s+(\d+)\s+\d+\s+\d+ *([\w ]*)\n", s)
  [parse(UInt32, m.captures[1]) =>
    occursin("end_sequence", m.captures[5]) ? nothing :
       LineInfo(files[parse(Int, m.captures[4])],
                parse(Int, m.captures[2]),
                parse(Int, m.captures[3]),
                occursin(m.captures[5], "is_stmt"))
   for m in lines]
end

function lineinfo(table, ip)
  for (ip′, li) in table
    if ip′ == ip
      return li
    elseif ip′ > ip
      error("not supported")
    end
  end
  return
end
