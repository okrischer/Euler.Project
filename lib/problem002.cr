require "benchmark"

def fib_imp(limit : Int32) : Int32
  a = 1
  b = 1
  sum = 0
  while b <= limit
    sum += b if b.even?
    a, b = b, a + b
  end
  sum
end

def fib_opt(limit : Int32) : Int32
  a = 1
  b = 1
  c = 2
  sum = 0
  while c <= limit
    sum += c
    a = b + c
    b = a + c
    c = a + b
  end
  sum
end

Benchmark.ips do |bm|
  bm.report("fib_imp") do
    fib_imp(4_000_000)
  end
  bm.report("fib_opt") do
    fib_opt(4_000_000)
  end
end
