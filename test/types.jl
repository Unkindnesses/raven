using Raven, Test
using Raven: @tag_str, RType, Bits, onion, pack, vpack, recurrence, recursive, issubset, recursive, finite, unroll, recur

union(x, y) = Raven.union(RType(x), RType(y))
Onion(xs) = onion(xs...)

let
  T1 = Onion([pack(tag"Empty"), pack(tag"Prepend", pack(tag"Empty"), Float64)])
  T2 = pack(tag"Prepend", onion([pack(tag"Empty"), pack(tag"Prepend", pack(tag"Empty"), Float64)]), Float64)

  T = union(T1, T2)

  @test union(T2, T1) == T

  @test union(union(T1, tag"c"), T2) == union(T, tag"c")

  @test T == recursive(Onion([pack(tag"Empty"), pack(tag"Prepend", recurrence, Float64)]))

  @test issubset(T1, T)
  @test issubset(T2, T)
  @test !issubset(T, T1)
  @test issubset(T, T)

  @test union(T, T) == T

  Tw = pack(tag"Prepend", T, Float64)

  @test issubset(Tw, T)

  @test issubset(unroll(T), T)
  @test issubset(T, unroll(T))
  @test recur(unroll(T)) == T
end

let
  A = recursive(vpack(tag"a", vpack(tag"b", recurrence)))
  B = recursive(vpack(tag"b", vpack(tag"a", recurrence)))
  @test unroll(A) == vpack(tag"a", B)
end

let
  List(T) = recursive(onion(pack(tag"Empty"), pack(tag"Prepend", recurrence, T)))
  @test recur(finite(List(List(Float64)))) == List(List(Float64))
end

let
  T1 = recursive(Onion([pack(tag"Empty1"), pack(tag"Prepend1", recurrence, Bits{64})]))
  T2 = recursive(Onion([pack(tag"Empty2"), pack(tag"Prepend2", recurrence, Bits{64})]))
  @test union(T1, tag"b") == onion(T1, tag"b")
  @test union(T1, T2) == onion(T1, T2)
end

let
  T1 = recursive(Onion([pack(tag"Empty"), pack(tag"Prepend", recurrence, Bits{64})]))
  T2 = recursive(Onion([pack(tag"Empty"), pack(tag"Prepend", recurrence, Float64)]))
  T3 = union(T1, T2)

  @test issubset(T1, T1)
  @test issubset(T3, T3)
  @test issubset(T1, T3)
  @test !issubset(T3, T1)
end

let
  T = onion(recursive(onion(pack(tag"a", recurrence), tag"b")), tag"c")
  @test union(T, T) == T
  @test recur(unroll(T)) == T
  @test union(T, pack(tag"a", tag"b")) == T
end

let
  T1 = Onion([pack(tag"Empty"), pack(tag"Prepend", pack(tag"Empty"), Bits{64})])
  T2 = pack(tag"Prepend", onion(Float64, pack(tag"Empty"), pack(tag"Prepend", pack(tag"Empty"), Bits{64})), Bits{64})

  T = union(T1, T2)
  @test T == recursive(onion(Float64, pack(tag"Empty"), pack(tag"Prepend", recurrence, Bits{64})))
end

let
  A = recursive(onion(vpack(tag"a", recurrence), vpack(tag"b", recurrence)))
  @test union(A, A) == A
end

let
  T = onion(pack(tag"b", recursive(onion(Float64, Bits{64}, pack(tag"a", recurrence)))), Bits{64}, Float64)
  @test recur(T) == recursive(Onion((Float64, Bits{64}, pack(tag"a", recurrence), pack(tag"b", recurrence))))

  T = onion(Float64, Bits{64}, pack(tag"d", pack(tag"d", recursive(onion(Bits{64}, Float64, pack(tag"b", recurrence))))))
  @test recur(T) == recursive(onion(Float64, Bits{64}, pack(tag"b", recurrence), pack(tag"d", recurrence)))

  T = onion(Float64, Bits{64}, pack(tag"a", onion(Float64, Bits{64}, vpack(tag"c", Bits{32})), Bits{64}), vpack(tag"c", onion(Float64, Bits{64}, pack(tag"c"))))
  @test recur(T) == recursive(onion(Float64, Bits{32}, Bits{64}, pack(tag"a", recurrence, Bits{64}), vpack(tag"c", recurrence)))
end

let
  A = vpack(tag"a", Bits{64})
  B = pack(tag"a", vpack(tag"a", Bits{64}))
  @test union(A, B) == recursive(Onion((Bits{64}, vpack(tag"a", recurrence))))
end

let
  A = vpack(tag"a", vpack(tag"a", Bits{64}))
  B = Onion((Bits{64}(1), vpack(tag"a", Bits{64})))
  @test union(A, B) == recursive(Onion((Bits{64}, vpack(tag"a", recurrence))))
end

let
  T = Onion((Bits{64}, pack(tag"a", Onion((Float64, Bits{64})), Onion((Float64, Bits{64}, pack(tag"a", Bits{64}, Bits{64}))))))
  @test recur(T) == recursive(Onion((Float64, Bits{64}, pack(tag"a", recurrence, recurrence))))
end

let
  A = pack(tag"a", pack(tag"a", Bits{64}))
  B = pack(tag"a", Onion((Float64, pack(tag"a", Float64))))
  C = Float64
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  A = Onion((Bits{64}(4), vpack(tag"a", Float64)))
  B = pack(tag"a", Bits{64}(4))
  C = pack(tag"a", Bits{64}(5))
  @test union(union(A, B), C) == union(A, union(B, C))

  A = pack(tag"c", tag"a", pack(tag"c"))
  B = pack(tag"c")
  C = pack(tag"c", pack(tag"c", tag"c"))
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  A = tag"b"
  B = vpack(tag"c", Onion((Bits{64}, tag"b")))
  C = recursive(vpack(tag"a", Onion((pack(tag"c", recurrence), tag"a"))))
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  A = vpack(tag"c", Bits{64})
  U = recursive(onion(Float64, vpack(tag"b", recurrence), A))
  @test union(A, U) == U
end

let
  A = 1.0
  B = pack(tag"c")
  C = pack(tag"c", vpack(tag"c", Bits{64}))
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  A = pack(tag"c", onion(Bits{64}, pack(tag"c")), tag"b")
  B = Bits{64}(1)
  C = tag"c"
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  A = pack(tag"c", onion(Bits{64}, pack(tag"c")), tag"b")
  @test union(A, Bits{64}(1)) == onion(A, Bits{64}(1))
end

let
  A = pack(tag"b", Bits{64}, pack(tag"a", onion(Bits{64}, pack(tag"a", Bits{64}))))
  B = pack(tag"b")
  @test union(A, B) == vpack(tag"b", recursive(onion(Bits{64}, pack(tag"a", recurrence))))
end

let
  A = recursive(vpack(tag"a", vpack(tag"b", recurrence)))
  @test recur(vpack(tag"b", A)) == recursive(vpack(tag"b", vpack(tag"a", recurrence)))
end

let
  A = vpack(tag"a", vpack(tag"b", Bits{64}))
  B = recursive(vpack(tag"a", vpack(tag"b", recurrence)))
  C = Bits{64}
  @test union(union(A, B), C) == recursive(onion(Bits{64}, vpack(tag"a", vpack(tag"b", recurrence))))
end

let
  A = pack(tag"a", vpack(tag"c", tag"b"), Bits{64})
  B = Bits{64}
  C = pack(tag"a", vpack(tag"c", onion(Bits{64}, Float64)), Bits{64})
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  A = pack(tag"b", Bits{64})
  B = vpack(tag"b", pack(tag"a", vpack(tag"c", Float64)))
  C = vpack(tag"c", Bits{64})
  @test union(A, union(B, C)) == union(union(A, B), C)
end

let
  A = pack(tag"b")
  B = pack(tag"b", pack(tag"a", onion(Bits{64}, Float64)), vpack(tag"c", Float64))
  C = Float64
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  A = vpack(tag"c", Bits{64}(3))
  B = Bits{64}(3)
  C = pack(tag"c", pack(tag"a", vpack(tag"c", Bits{64})))
  @test union(A, union(B, C)) == union(union(A, B), C)
end

let
  A = Float64
  B = vpack(tag"c", Float64)
  C = vpack(tag"a", recursive(onion(Float64, vpack(tag"b", pack(tag"c", recurrence)))))
  @test union(A, union(B, C)) == union(union(A, B), C)
end

let
  T = pack(tag"a", recursive(vpack(tag"b", recurrence)))
  @test union(T, tag"b") == onion(T, tag"b")
end

let
  A = pack(tag"a", Bits{64}, vpack(tag"a", Bits{64}))
  B = onion(pack(tag"a", onion(Float64, Bits{64}), Bits{64}), Bits{64})
  C = Bits{64}
  @test union(A, union(B, C)) == union(union(A, B), C)
end

let
  A = pack(tag"a", Bits{64})
  B = vpack(tag"a", pack(tag"c"))
  C = onion(pack(tag"a", pack(tag"a")), vpack(tag"c", Float64))
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  T = onion(Float64, Bits{64}, pack(tag"c", vpack(tag"c", recursive(onion(Float64, Bits{64}, pack(tag"b", recurrence))))))
  @test recur(T) == recursive(onion(Float64, Bits{64}, pack(tag"b", recurrence), vpack(tag"c", recurrence)))
end

let
  A = pack(tag"d")
  B = pack(tag"a", onion(Float32, Float64, Bits{32}), vpack(tag"c", onion(Bits{32}, tag"c")))
  C = tag"c"
  @test union(union(A, B), C) == union(A, union(B, C))
end

let
  I = recursive(onion(vpack(tag"a", recurrence), vpack(tag"b", recurrence)))
  T = pack(tag"b", I, vpack(tag"b", Bits{32}))
  @test recur(T) == recursive(onion(Bits{32}, vpack(tag"a", recurrence), vpack(tag"b", recurrence)))
end

let
  T = vpack(tag"d", vpack(tag"c", pack(tag"b", onion(Float64, pack(tag"d")))))
  R = recur(T)
  @test recur(unroll(R)) == R
end

let
  T = onion(vpack(tag"a", pack(tag"b")), vpack(tag"b", Int), vpack(tag"d", vpack(tag"a", vpack(tag"b", Int))))
  @test recur(T) == recursive(onion(vpack(tag"a", recurrence), vpack(tag"b", Int64), vpack(tag"d", recurrence)))
end

let
  T = pack(tag"List", vpack(tag"List", vpack(tag"List", Float64)))
  @test recur(T) == T
end

struct Generator
  func
  depth::Int64
end

Generator(f) = Generator(f, 10)

down(g::Generator) = Generator(g.func, g.depth-1)

(g::Generator)() = g.func(g)

oneof(fs) = g::Generator -> rand(fs)(g)

# TODO fuzz strings
const_num(g::Generator) = RType(rand([Bits{64},Float64])(rand(1:5)))
const_tag(g::Generator) = RType(rand([tag"a", tag"b", tag"c", tag"d"]))
constant = oneof([const_num, const_tag])
primitive(g::Generator) = RType(rand([Bits{64},Bits{32},Float64,Float32]))

# Assume tags of type `Tag` for now
# _pack(g::Generator) = recur(pack([g() for i = 1:rand(1:3)]...))
# _vpack(g::Generator) = recur(vpack(g(), g()))
_pack(g::Generator) = recur(pack(const_tag(g), [g() for i = 1:rand(0:2)]...))
_vpack(g::Generator) = recur(vpack(const_tag(g), g()))
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
  @test @iff issubset(A, B) && issubset(B, A) A == B
  @test recur(unroll(A)) == A
  @test union(A, A) == A
  @test union(A, B) == union(B, A)
  @test union(union(A, B), C) == union(A, union(B, C))
  U = union(A, B)
  @test union(A, U) == U
  @test issubset(A, U)
  @test @implies issubset(A, B) U == B
  @test @iff issubset(A, B) issubset(U, B)
end
