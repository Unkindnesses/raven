using Raven.Caches, Test
using Raven.Caches: reset!

level1 = Caches.Dict{Int,Int}()

level1[1] = 5

@test level1[1] == 5
@test_throws KeyError level1[2]

cache_log = []

level2 = Cache{Int,String}() do self, i
  push!(cache_log, i)
  string(level1[i])
end

@test level2[1] == "5"
@test cache_log == [1]

empty!(cache_log)
reset!(level2, deps = level1)

@test level2[1] == "5"
@test cache_log == []

level1[1] = 6
reset!(level2, deps = level1)

@test level2[1] == "6"

level3 = Cache{Int,String}() do self, i
  level2[i] * "!"
end

@test level3[1] == "6!"

level1[1] = 7
reset!(Pipeline([level1, level2, level3]))

@test level3[1] == "7!"

optional = Cache{Int,String}() do self, i
  haskey(level1, i) ? string(level1[i]) : "default"
end

@test optional[1] == "7"
@test optional[5] == "default"
level1[5] = 13
reset!(optional, deps = level1)
@test optional[5] == "13"

@testset "Recursive Fibonacci" begin
  log = []
  init = Caches.Dict{Int,Int}()
  init[0] = 0; init[1] = 1

  fib = Cache{Int,Int}() do self, i
    push!(log, i)
    i <= 1 ? init[i] : self[i-1] + self[i-2]
  end

  @test fib[10] == 55
  @test fib[5] == 5
  @test log == 10:-1:0
  empty!(log)

  reset!(fib, deps = init)
  @test fib[10] == 55
  @test isempty(log)

  init[0] = 1
  reset!(fib, deps = init)
  @test fib[10] == 89
  @test fib[5] == 8
end

@testset "Eager cache" begin
  data = Caches.Dict{Int,Int}()
  data[1] = 2
  log = []

  level1 = EagerCache() do self, i
    push!(log, i)
    data[i]^2
  end

  level2 = Cache() do self, i
    push!(log, i)
    level1[i] + 1
  end

  pipe = Pipeline([data, level1, level2])

  @test level2[1] == 5
  @test log == [1, 1]
  empty!(log)

  data[1] = 2
  reset!(pipe)
  @test log == [1]

  @test level2[1] == 5
  @test log == [1]
  empty!(log)

  data[1] = 3
  reset!(pipe)
  @test log == [1]
  @test level2[1] == 10
  @test log == [1, 1]
end
