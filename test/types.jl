using Raven, Test
using Raven: @tag_str, Onion, onion, Primitive, Recursive, Recur, pack, vpack, union, issubset, recursive, unroll

let
  T1 = Onion([pack(tag"Empty"), pack(tag"Prepend", pack(tag"Empty"), Int64)])
  T2 = pack(tag"Prepend", Onion([pack(tag"Empty"), pack(tag"Prepend", pack(tag"Empty"), Int64)]), Int64)

  T = union(T1, T2)

  @test union(T2, T1) == T

  @test union(union(T1, tag"c"), T2) == union(T, tag"c")

  @test T == Recursive(Onion([pack(tag"Empty"), pack(tag"Prepend", Recur(), Int)]))

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
end

let
  T1 = Recursive(Onion([pack(tag"Empty1"), pack(tag"Prepend1", Recur(), Int)]))
  T2 = Recursive(Onion([pack(tag"Empty2"), pack(tag"Prepend2", Recur(), Int)]))
  @test union(T1, tag"b") == onion(T1, tag"b")
  @test union(T1, T2) == onion(T1, T2)
end

let
  T1 = Recursive(Onion([pack(tag"Empty"), pack(tag"Prepend", Recur(), Int)]))
  T2 = Recursive(Onion([pack(tag"Empty"), pack(tag"Prepend", Recur(), Float64)]))
  T3 = union(T1, T2)

  @test issubset(T1, T1)
  @test issubset(T3, T3)
  @test issubset(T1, T3)
  @test !issubset(T3, T1)
end

let
  T = Onion((Recursive(Onion((pack(tag"a", Recur()), tag"b"))), tag"c"))
  @test union(T, T) == T
  @test recursive(unroll(T)) == T
  @test union(T, pack(tag"a", tag"b")) == T
end

let
  T1 = Onion([pack(tag"Empty"), pack(tag"Prepend", pack(tag"Empty"), Int64)])
  T2 = pack(tag"Prepend", Onion([Float64, pack(tag"Empty"), pack(tag"Prepend", pack(tag"Empty"), Int64)]), Int64)

  T = union(T1, T2)
  @test T == Recursive(Onion([Float64, pack(tag"Empty"), pack(tag"Prepend", Recur(), Int)]))
end

let
  A = Recursive(Onion((vpack(tag"a", Recur()), vpack(tag"b", Recur()))))
  @test union(A, A) == A
end

let
  T = Onion((pack(tag"b", Recursive(Onion((Float64, Int64, pack(tag"a", Recur()))))), Int64, Float64))
  @test recursive(T) == Recursive(Onion((Float64, Int64, pack(tag"a", Recur()), pack(tag"b", Recur()))))

  T = onion(Float64, Int64, pack(tag"d", pack(tag"d", Recursive(onion(Int64, Float64, pack(tag"b", Recur()))))))
  @test recursive(T) == Recursive(onion(Float64, Int64, pack(tag"b", Recur()), pack(tag"d", Recur())))

  T = onion(Float64, Int64, pack(tag"a", onion(Float64, Int64, vpack(tag"c", Int32)), Int64), vpack(tag"c", onion(Float64, Int64, pack(tag"c"))))
  @test recursive(T) == Recursive(onion(Float64, Int32, Int64, pack(tag"a", Recur(), Int64), vpack(tag"c", Recur())))
end

let
  A = vpack(tag"a", Int)
  B = pack(tag"a", vpack(tag"a", Int))
  @test union(A, B) == Recursive(Onion((Int, vpack(tag"a", Recur()))))
end

let
  A = vpack(tag"a", vpack(tag"a", Int))
  B = Onion((1, vpack(tag"a", Int)))
  @test union(A, B) == Recursive(Onion((Int, vpack(tag"a", Recur()))))
end

let
  T = Onion((Int, pack(tag"a", Onion((Float64, Int)), Onion((Float64, Int, pack(tag"a", Int, Int))))))
  @test recursive(T) == Recursive(Onion((Float64, Int64, pack(tag"a", Recur(), Recur()))))
end

let
  A = pack(tag"a", pack(tag"a", Int64))
  B = pack(tag"a", Onion((Float64, pack(tag"a", Float64))))
  C = Float64
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  A = Onion((4, vpack(tag"a", Float64)))
  B = pack(tag"a", 4)
  C = pack(tag"a", 5)
  @test union(union(A, B), C) == union(A, union(B, C))

  A = pack(tag"c", tag"a", pack(tag"c"))
  B = pack(tag"c")
  C = pack(tag"c", pack(tag"c", tag"c"))
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  A = tag"b"
  B = vpack(tag"c", Onion((Int64, tag"b")))
  C = Recursive(vpack(tag"a", Onion((pack(tag"c", Recur()), tag"a"))))
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  A = vpack(tag"c", Int64)
  U = Recursive(onion(Float64, vpack(tag"b", Recur()), A))
  @test union(A, U) == U
end

let
  A = 1.0
  B = pack(tag"c")
  C = pack(tag"c", vpack(tag"c", Int))
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  A = pack(tag"c", onion(Int, pack(tag"c")), tag"b")
  B = 1
  C = tag"c"
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  A = pack(tag"c", onion(Int, pack(tag"c")), tag"b")
  @test union(A, 1) == onion(A, 1)
end

let
  A = pack(tag"b", Int, pack(tag"a", onion(Int, pack(tag"a", Int))))
  B = pack(tag"b")
  @test union(A, B) == vpack(tag"b", Recursive(onion(Int64, pack(tag"a", Recur()))))
end

# TODO overlapping recursion
# let
#   A = Recursive(vpack(tag"a", vpack(tag"b", Recur())))
#   @test recursive(vpack(tag"b", A)) == Recursive(vpack(tag"b", vpack(tag"a", Recur())))
# end

let
  A = vpack(tag"a", vpack(tag"b", Int64))
  B = Recursive(vpack(tag"a", vpack(tag"b", Recur())))
  C = Int64
  # TODO overlapping recursion
  # @test union(union(A, B), C) == Recursive(onion(Int, vpack(tag"a", vpack(tag"b", Recur()))))
  @test union(union(A, B), C) == Recursive(onion(Int64, vpack(tag"a", Recur()), vpack(tag"b", Recur())))
end

let
  A = pack(tag"a", vpack(tag"c", tag"b"), Int64)
  B = Int64
  C = pack(tag"a", vpack(tag"c", onion(Int64, Float64)), Int64)
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  A = pack(tag"b", Int)
  B = vpack(tag"b", pack(tag"a", vpack(tag"c", Float64)))
  C = vpack(tag"c", Int)
  @test union(A, union(B, C)) == union(union(A, B), C)
end

let
  A = pack(tag"b")
  B = pack(tag"b", pack(tag"a", onion(Int64, Float64)), vpack(tag"c", Float64))
  C = Float64
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  A = vpack(tag"c", 3)
  B = 3
  C = pack(tag"c", pack(tag"a", vpack(tag"c", Int)))
  @test union(A, union(B, C)) == union(union(A, B), C)
end

let
  A = Float64
  B = vpack(tag"c", Float64)
  C = vpack(tag"a", Recursive(onion(Float64, vpack(tag"b", pack(tag"c", Recur())))))
  @test union(A, union(B, C)) == union(union(A, B), C)
end

let
  T = pack(tag"a", Recursive(vpack(tag"b", Recur())))
  @test union(T, tag"b") == onion(T, tag"b")
end

let
  A = pack(tag"a", Int64, vpack(tag"a", Int64))
  B = onion(pack(tag"a", onion(Float64, Int64), Int64), Int64)
  C = Int64
  @test union(A, union(B, C)) == union(union(A, B), C)
end

let
  T = Recursive(vpack(tag"a", vpack(tag"c", onion(Int64, vpack(tag"c", Recur())))))
  @test union(T, Int64) == Recursive(onion(Int64, vpack(tag"a", Recur()), vpack(tag"c", Recur())))
end

let
  A = pack(tag"a", Int64)
  B = vpack(tag"a", pack(tag"c"))
  C = onion(pack(tag"a", pack(tag"a")), vpack(tag"c", Float64))
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  T = onion(Float64, Int64, pack(tag"c", vpack(tag"c", Recursive(onion(Float64, Int64, pack(tag"b", Recur()))))))
  @test recursive(T) == Recursive(onion(Float64, Int64, pack(tag"b", Recur()), vpack(tag"c", Recur())))
end

let
  A = pack(tag"d")
  B = pack(tag"a", onion(Float32, Float64, Int32), vpack(tag"c", onion(Int32, tag"c")))
  C = tag"c"
  @test union(union(A, B), C) == union(A, union(B, C))
end

# let
#   # overlapping recursion
#   A = pack(tag"b", vpack(tag"a", Recursive(onion(Int64, vpack(tag"d", Recur())))))
#   U = Recursive(onion(Int64, pack(tag"b", vpack(tag"a", Recur())), vpack(tag"d", Recur())))
#   @test union(A, U) == U
# end

let
  I = Recursive(onion(vpack(tag"a", Recur()), vpack(tag"b", Recur())))
  T = pack(tag"b", I, vpack(tag"b", Int32))
  @test recursive(T) == Recursive(onion(Int32, vpack(tag"a", Recur()), vpack(tag"b", Recur())))
end

let
  T = vpack(tag"d", vpack(tag"c", pack(tag"b", onion(Float64, pack(tag"d")))))
  R = recursive(T)
  @test recursive(unroll(R)) == R
end

struct Generator
  func
  depth::Int
end

Generator(f) = Generator(f, 10)

down(g::Generator) = Generator(g.func, g.depth-1)

(g::Generator)() = g.func(g)

oneof(fs) = g::Generator -> rand(fs)(g)

# TODO fuzz strings
const_num(g::Generator) = rand([Int64,Float64])(rand(1:5))
const_tag(g::Generator) = rand([tag"a", tag"b", tag"c", tag"d"])
constant = oneof([const_num, const_tag])
primitive(g::Generator) = rand([Int64,Float64,Float32,Int32])

# Assume tags of type `Tag` for now
# _pack(g::Generator) = recursive(pack([g() for i = 1:rand(1:3)]...))
# _vpack(g::Generator) = recursive(vpack(g(), g()))
_pack(g::Generator) = recursive(pack(const_tag(g), [g() for i = 1:rand(0:2)]...))
_vpack(g::Generator) = recursive(vpack(const_tag(g), g()))
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

for i = 1:10
  g = Generator()
  A, B, C = g(), g(), g()
  @assert @iff issubset(A, B) && issubset(B, A) A === B
  @assert recursive(unroll(A)) == A
  @assert union(A, A) === A
  @assert union(A, B) === union(B, A)
  @assert union(union(A, B), C) === union(A, union(B, C))
  U = union(A, B)
  @assert union(A, U) === U
  @assert issubset(A, U)
  @assert @implies issubset(A, B) U === B
  @assert @iff issubset(A, B) issubset(U, B)
end
