using Raven, Test
using Raven: @tag_str, @src_str, @rvx_str, Definitions, Inferred, Binding, rlist
using Raven.Caches: reset!, valueid, fingerprint

@testset "Globals" begin
  comp = Raven.load(src"foo = 1, bar = 1")
  defs = Definitions(comp)

  @test defs[Binding(tag"", :foo)] == 1
  @test defs[Binding(tag"", :bar)] == 1
  id_foo = valueid(defs.globals, Binding(tag"", :foo))
  id_bar = valueid(defs.globals, Binding(tag"", :bar))

  Raven.reload!(comp, src"foo = 1, bar = 2")
  reset!(defs, deps = comp)

  @test defs[Binding(tag"", :foo)] == 1
  @test defs[Binding(tag"", :bar)] == 2
  @test_broken id_foo == valueid(defs.globals, Binding(tag"", :foo))
  @test id_bar != valueid(defs.globals, Binding(tag"", :bar))
end

@testset "Methods" begin
  cx = Raven.load(src"fn foo(x) { x+1 }")
  defs = Definitions(cx)
  disps = Raven.dispatchers(defs)

  @test length(defs[tag"foo"]) == 1
  @test !isempty(defs[tag"common.core.main"])

  foo_id = valueid(defs.methods, tag"foo")
  main_id = valueid(defs.methods, tag"common.core.main")

  @test disps[(tag"foo", rlist(Int))] isa Raven.IR
  @test disps[(tag"common.+", rlist(Int, Int))] isa Raven.IR

  Raven.reload!(cx, src"fn foo(x) { x+2 }")
  reset!(defs, deps = cx)
  reset!(disps, deps = defs)

  @test length(defs[tag"foo"]) == 1

  @test foo_id != valueid(defs.methods, tag"foo")
  @test main_id == valueid(defs.methods, tag"common.core.main")

  @test !Caches.iscached(disps, (tag"foo", rlist(Int)))
  @test Caches.iscached(disps, (tag"common.+", rlist(Int, Int)))
end

@testset "Inference" begin
  cx = Raven.load(src"n = 1, fn foo(x) { x+n }, foo(5)")
  defs = Definitions(cx)
  inf = Inferred(defs)

  [inf[(m,)] for m in defs[tag"common.core.main"]]

  @test inf[(tag"foo", rlist(5))][2] == rlist(6)

  inf[(tag"common.+", rlist(Int, Int))]
  id_plus = Caches.valueid(inf.results, (tag"common.+", rlist(Int, Int)))

  Raven.reload!(cx, src"n = 2, fn foo(x) { x+n }, foo(5)")

  reset!(defs, deps = cx)
  reset!(inf, deps = defs)

  @test inf[(tag"foo", rlist(5))][2] == rlist(7)
  @test id_plus == Caches.valueid(inf.results, (tag"common.+", rlist(Int, Int)))
end

@testset "Compiler" begin
  compiler = Raven.Compiler()
  compiler.wasm[(tag"common.malloc!", rlist(Int32))]
  print = fingerprint(compiler.pipe)
  reset!(compiler.pipe)
  @test fingerprint(compiler.pipe) == print
end
