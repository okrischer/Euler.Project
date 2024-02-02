function largestPF(n)
  k = 2
  while k * k <= n
    while n % k == 0
      n ÷= k
    end
    n == 1 && return k
    k += 1
  end
  n
end
