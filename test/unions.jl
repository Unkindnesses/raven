using Raven.Unions, Test
using Raven.Unions: @union, field, peel

@union struct Num
  float::Float64
  int::Int64
end

Num(x::Float64) = Num(; float=x)
Num(x::Int64) = Num(; int=x)

Base.Float64(x::Num) = peel(Float64, x)

let x = Num(float=1)
  @test x.float === 1.0
  @test_throws "not active" x.int
end

@test_throws Exception Num(foo=1)
@test endswith(string(Num(; float=1)), "Num(float = 1.0)")

function test(x)
  x = Num(x)
  y = peel(x) do x
    return Num(x + 1)
  end
  return Float64(y) == Float64(x) + 1
end

@test test(1)
@test @allocated(test(1)) == 0
@test @allocated(test(1.0)) == 0

struct Foo{T}
  x::T
end

struct Bar
  x::Float64
  y::Float64
end

@union struct FooBar{T}
  foo::Foo{T}
  bar::Bar
end

function hypot(x::FooBar)
  if field(x) == :foo
    x.foo.x^2
  elseif field(x) == :bar
    x.bar.x * x.bar.y
  end
end

@test hypot(FooBar{Float64}(; foo=Foo(5.0))) == 25
@test hypot(FooBar{Float64}(; bar=Bar(2, 3))) == 6

@test @allocated(hypot(FooBar{Float64}(bar=Bar(2, 3)))) == 0

hypot2(x::Foo) = x.x^2
hypot2(x::Bar) = x.x * x.y
hypot2(x::FooBar) = peel(hypot2, x)

@test hypot2(FooBar{Float64}(; bar=Bar(2, 3))) == 6
@test @allocated(hypot2(FooBar{Float64}(bar=Bar(2, 3)))) == 0

@union struct SmallVector{T}
  one::Tuple{T}
  two::Tuple{T, T}
  three::Tuple{T, T, T}
  many::Vector{T}
end

SmallVector{T}(x) where T = SmallVector{T}(one = (x,))
SmallVector{T}(x, y) where T = SmallVector{T}(two = (x, y))
SmallVector{T}(x, y, z) where T = SmallVector{T}(three = (x, y, z))
SmallVector{T}(xs...) where T = SmallVector{T}(many = xs)

Base.length(xs::SmallVector) = peel(length, xs)
Base.getindex(xs::SmallVector, i...) = peel(x -> getindex(x, i...), xs)
Base.iterate(xs::SmallVector, st...) = peel(x -> iterate(x, st...), xs)

@test SmallVector{Int}(1, 2, 3)[1] == 1
@test @allocated(SmallVector{Int}(1, 2, 3)[1]) == 0
