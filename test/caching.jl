using Raven, Test
using Raven: @tag_str, @src_str, @rvx_str, Definitions, Binding
using Raven.Caches: reset!, valueid

@testset "Globals" begin
  comp = Raven.load(src"foo = 1, bar = 1")
  defs = Definitions(comp)

  @test defs[Binding(tag"", :foo)] == 1
  @test defs[Binding(tag"", :bar)] == 1
  id_foo = valueid(defs.globals, Binding(tag"", :foo))
  id_bar = valueid(defs.globals, Binding(tag"", :bar))

  Raven.reload!(comp, src"foo = 1, bar = 2")
  reset!(defs, deps = [comp])

  @test defs[Binding(tag"", :foo)] == 1
  @test defs[Binding(tag"", :bar)] == 2
  @test id_foo == valueid(defs.globals, Binding(tag"", :foo))
  @test id_bar != valueid(defs.globals, Binding(tag"", :bar))
end

@testset "Methods" begin
  cx = Raven.load(src"fn foo(x) { x+1 }")
  defs = Definitions(cx)

  @test length(defs[tag"foo"]) == 1
  @test !isempty(defs[tag"common.core.main"])

  foo_id = valueid(defs.methods, tag"foo")
  main_id = valueid(defs.methods, tag"common.core.main")

  Raven.reload!(cx, src"fn foo(x) { x+2 }")
  reset!(defs, deps = [cx])

  @test length(defs[tag"foo"]) == 1

  @test foo_id != valueid(defs.methods, tag"foo")
  @test main_id == valueid(defs.methods, tag"common.core.main")
end
