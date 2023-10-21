using Test

function compile_tmp(src, args...)
  path = tempname()
  rv = path * ".rv"
  wasm = path * ".wasm"
  js = path * ".js"
  paths = (rv, wasm, js)
  try
    open(io -> write(io, src), rv, "w")
    compile(rv, args...)
    return paths
  catch e
    foreach(p -> isfile(p) && rm(p), paths)
    rethrow()
  end
end

function output(src, args...)
  paths = compile_tmp(src, args...)
  try
    io = IOBuffer()
    p = run(`node --experimental-wasm-stack-switching $(paths[3])`, stdin, io, io, wait = false)
    wait(p)
    return p.exitcode, String(read(seek(io, 0)))
  finally
    foreach(p -> isfile(p) && rm(p), paths)
  end
end

function test_rv(code; error = false, source = nothing,
                 output = nothing, options = Options())
  exit, out = Raven.output(code, options)
  if exit != 0 && !error
    Base.error(out)
  end
  @assert (exit != 0) == error
  if output != nothing
    result = occursin(output, out)
    Test.do_test(Test.Returned(result, nothing, source), :(occursin($output, $out)))
  elseif !error
    results = split(out, "\n", keepempty = false)
    @assert !isempty(results)
    for r in results
      r, code = match(r"(\w+): (.*)", r).captures
      Test.do_test(Test.Returned(r == "pass", nothing, source), code)
    end
  end
end

macro test_rv(code, kw...)
  quote
    test_rv($(esc(code)); $(esc.(kw)...), source = $(QuoteNode(__source__)))
  end
end

macro test_rv_str(code)
  quote
    test_rv($(esc(code)), source = $(QuoteNode(__source__)))
  end
end
