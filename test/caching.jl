using Raven, Test
using Raven: @tag_str, @src_str, @rvx_str, Definitions, Interpreter, Inferred, Binding, RType, rlist
using Raven.Caches: Caches, Pipeline, reset!, id, fingerprint

@testset "Globals" begin
  comp = Raven.load(src"foo = 1, bar = 1")
  defs = Definitions(comp)

  @test defs[Binding(tag"", :foo)] == RType(1)
  @test defs[Binding(tag"", :bar)] == RType(1)
  id_foo = id(defs.globals, Binding(tag"", :foo))
  id_bar = id(defs.globals, Binding(tag"", :bar))

  Raven.reload!(comp, src"foo = 1, bar = 2")
  reset!(defs, deps = comp)

  @test defs[Binding(tag"", :foo)] == RType(1)
  @test defs[Binding(tag"", :bar)] == RType(2)
  @test_broken id_foo == id(defs.globals, Binding(tag"", :foo))
  @test id_bar != id(defs.globals, Binding(tag"", :bar))
end

@testset "Methods" begin
  cx = Raven.load(src"fn foo(x) { x+1 }")
  defs = Definitions(cx)
  int = Interpreter(defs)
  pipe = Pipeline((cx, defs, int))

  @test length(defs[tag"foo"]) == 1
  @test !isempty(defs[tag"common.core.main"])

  foo_id = id(defs.methods, tag"foo")
  main_id = id(defs.methods, tag"common.core.main")

  Raven.reload!(cx, src"fn foo(x) { x+2 }")
  reset!(pipe)

  @test length(defs[tag"foo"]) == 1

  @test foo_id != id(defs.methods, tag"foo")
  @test main_id == id(defs.methods, tag"common.core.main")
end

@testset "Inference" begin
  cx = Raven.Compiler(src"n = 1, fn foo(x) { x+n }, foo(5)")
  defs = cx.pipe.defs
  inf = cx.pipe.inferred

  @test inf[(tag"foo", rlist(5))][2] == rlist(6)

  inf[(tag"common.+", rlist(RType(Int64), RType(Int64)))]
  id_plus = id(inf.results, (tag"common.+", rlist(RType(Int64), RType(Int64))))

  Raven.reload!(cx, src"n = 2, fn foo(x) { x+n }, foo(5)")

  @test inf[(tag"foo", rlist(5))][2] == rlist(7)
  @test id_plus == id(inf.results, (tag"common.+", rlist(RType(Int64), RType(Int64))))
end

@testset "Compiler" begin
  compiler = Raven.Compiler()
  @test copy(compiler.pipe.sources) isa Raven.Modules
  reset!(compiler.pipe)
  compiler.pipe[(tag"common.malloc!", rlist(Int32))]
  print = fingerprint(compiler.pipe)
  reset!(compiler.pipe)
  @test fingerprint(compiler.pipe) == print
end

@testset "Match methods" begin
  cx = Raven.Compiler(src"");
  sig = (tag"common.matchTrait", rlist(tag"common.Int64", RType(Int64)))
  @test !Caches.iscached(cx.pipe.inferred.results, sig)
  @test cx.pipe.inferred[sig] isa Pair
  Raven.reload!(cx, src"""
    extend fn matchTrait(tag"Lit", x: pack(tag"Lit", val)) { Some(x) }
    """)
  @test Caches.iscached(cx.pipe.inferred.results, sig)
end
