using Raven.Caches, Test

level1 = Cache{Int,Int}()

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

@test level2[1] == "5"
@test cache_log == []

level1[1] = 6
@test level2[1] == "6"

level3 = Cache{Int,String}() do ch, i
  level2[i] * "!"
end

@test level3[1] == "6!"

level1[1] = 7

@test level3[1] == "7!"

optional = Cache{Int,String}() do ch, i
  haskey(level1, i) ? string(level1[i]) : "default"
end

@test optional[1] == "7"
@test optional[5] == "default"
level1[5] = 13
@test optional[5] == "13"
