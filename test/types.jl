using Raven, Test
using Raven: @tag_str, Onion, Recursive, Recur, VPack, pack, union, issubset, recursive, unroll

T1 = Recursive(Onion([pack(tag"Empty"), pack(tag"Prepend", Recur(), Int)]))
T2 = Recursive(Onion([pack(tag"Empty"), pack(tag"Prepend", Recur(), Onion([Int, String]))]))

@test issubset(T1, T1)
@test issubset(T2, T2)
@test issubset(T1, T2)
@test !issubset(T2, T1)

T1 = Onion([pack(tag"Empty"), pack(tag"Prepend", pack(tag"Empty"), Int64)])
T2 = pack(tag"Prepend", Onion([pack(tag"Empty"), pack(tag"Prepend", pack(tag"Empty"), Int64)]), Int64)

T = union(T1, T2)

@test T == Recursive(Onion([pack(tag"Empty"), pack(tag"Prepend", Recur(), Int)]))

@test union(T2, T1) == T

@test issubset(T1, T)
@test issubset(T2, T)
@test !issubset(T, T1)
@test issubset(T, T)

@test union(T, T) == T

Tw = pack(tag"Prepend", T, Int)

@test issubset(Tw, T)

@test issubset(unroll(T), T)
@test issubset(T, unroll(T))
@test recursive(unroll(T)) == T

struct Generator
  func
  depth::Int
end

Generator(f) = Generator(f, 10)

down(g::Generator) = Generator(g.func, g.depth-1)

(g::Generator)() = g.func(g)

oneof(fs) = g::Generator -> rand(fs)(g)

const_num(g::Generator) = rand([Int64,Int32,Float64,Float32])(rand(1:5))
const_tag(g::Generator) = rand([tag"a", tag"b", tag"c"])
constant = oneof([const_num, const_tag])
primitive(g::Generator) = rand([Int64,Int32,Float64,Float32])

_pack(g::Generator) = pack([g() for i = 1:rand(1:3)]...)
_vpack(g::Generator) = VPack(g(), g())
or(g::Generator) = union(g(), g())
nested(g::Generator) = g.depth == 0 ? g() : oneof([_pack, _vpack, or])(down(g))

Generator() = Generator(oneof([primitive, constant, nested]))

macro implies(a, b)
  :(!($(esc(a))) || $(esc(b)))
end

macro iff(a, b)
  :($(esc(a))::Bool == $(esc(b))::Bool)
end

using Random
Random.seed!(42)

Raven.enable_recursion[] = false

for i = 1:10
  g = Generator()
  A, B, C = g(), g(), g()
  @test union(A, A) === A
  @test @iff issubset(A, B) && issubset(B, A) A === B
  @test union(A, B) === union(B, A)
  @test union(union(A, B), C) === union(A, union(B, C))
  C = union(A, B)
  @test union(A, C) === C
  @test issubset(A, C)
  @test @implies issubset(A, B) C === B
  @test @iff issubset(A, B) issubset(C, B)
end

Raven.enable_recursion[] = true
