using Raven.Caches, Test
using Raven.Caches: reset!

level1 = Caches.Dict{Int,Int}()

level1[1] = 5

@test level1[1] == 5
@test_throws KeyError level1[2]

cache_log = []

level2 = Cache{Int,String}() do ch, i
  push!(cache_log, i)
  string(level1[i])
end

@test level2[1] == "5"
@test cache_log == [1]

empty!(cache_log)
reset!(level2, deps = [level1])

@test level2[1] == "5"
@test cache_log == []

level1[1] = 6
reset!(level2, deps = [level1])

@test level2[1] == "6"

level3 = Cache{Int,String}() do ch, i
  level2[i] * "!"
end

@test level3[1] == "6!"

level1[1] = 7
reset!(level2, deps = [level1])
reset!(level3, deps = [level1, level2])

@test level3[1] == "7!"

optional = Cache{Int,String}() do ch, i
  haskey(level1, i) ? string(level1[i]) : "default"
end

@test optional[1] == "7"
@test optional[5] == "default"
level1[5] = 13
reset!(optional, deps = [level1])
@test optional[5] == "13"

@testset "Recursive Fibonacci" begin
  init = Cache{Int,Int}()
  init[0] = 0; init[1] = 1

  fib = Cache{Int,Int}() do ch, i
    i <= 1 ? init[i] : ch[i-1] + ch[i-2]
  end

  @test fib[10] == 55
  @test fib[5] == 5

  init[0] = 1
  reset!(fib, deps = [init])
  @test fib[10] == 89
  @test fib[5] == 8
end

@testset "Iterative Fibonacci" begin
  init = Cache{Int,Int}()
  init[0] = 0; init[1] = 1

  fib = Cache{Int,Int}() do ch, i
    for i = 0:i
      ch[i] = i <= 1 ? init[i] : ch[i-1] + ch[i-2]
    end
    return ch[i]
  end

  @test fib[10] == 55
  @test fib[5] == 5

  init[0] = 1
  reset!(fib, deps = [init])
  @test fib[5] == 8
  @test fib[10] == 89
end
