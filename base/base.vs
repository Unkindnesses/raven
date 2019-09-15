fn pow(x::Int, n::Int):
  r = 1
  while n > 0:
    n = n - 1
    r = r * x
  return r
