require "benchmark"

def factor_naive(n : Int64) : Int64
  factors = [] of Int64
  k = 2
  while k <= n
    while n % k == 0
      factors << k
      n //= k
    end
    k += 1
  end
  factors.max
end

def factor_opt(n : Int64)
  factor = 1
  k = 3
  while n % 2 == 0
    factor = 2
    n //= k
  end

  while k * k <= n && n > 1
    while n % k == 0
      factor = k
      n //= k
    end
    k += 2
  end

  n > 1 ? n : factor
end

Benchmark.ips do |bm|
  bm.report("factor_naive") do
    factor_naive(600851475143)
  end
  bm.report("factor_opt") do
    factor_opt(600851475143)
  end
end
