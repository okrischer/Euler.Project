using BenchmarkTools

function sieve(n)
  isprime = trues(n+1)
  primes = Vector{Int64}()
  for i in 2:n
    if isprime[i]
      push!(primes, i)
      for j in i:n÷i
        isprime[i*j] = false
      end
    end
  end
  primes
end

@btime sieved = sieve(105000)
@show sieved[10001]
