dwarfdump(f) = String(read(`llvm-dwarfdump $f`))

dwarf_verify(f) = String(read(`llvm-dwarfdump --verify $f`))
