\chapter{Problem 007: 10,001st Prime}

By listing the first six prime numbers: $2,3,5,7,11$, and $13$, we can see that the
$6$th prime is $13$.

\textbf{What is the $10,001$st prime number?}

\begin{code}
import Test.QuickCheck ( (==>), quickCheck, Property )
\end{code}

\section{Using a lazy list}

To solve the problem, we will use our well-known function \mintinline{haskell}{sieve},
which generates a lazy list of prime numbers:

\begin{code}
sieve :: [Int] -> [Int]
sieve (x:xs) = x:sieve [y | y <- xs, rem y x /= 0]

primes :: [Int]
primes = sieve [2..]
\end{code}

Now, we only need to get n'th one:

\begin{code}
solve :: Int -> Int
solve n = head $ take 1 $ drop (n-1) primes
\end{code}

\section{Using a real sieve}

When you're calling \mintinline{haskell}{solve} from the last section with an
input value of $10001$, you will notice some delay in returning the result.
This is because of the \mintinline{haskell}{sieve} function, which, according to its name,
is supposed to do some kind of sieving like the famous \emph{Sieve of Eratosthenes}
algorithm does.
But it does not; it's merely a generating function with a running time of $\mathcal{O}(n^2)$,
instead of the promised running time of $\mathcal{O} (n (\lg n))$ of the original sieve.

It's actually quite hard to implement that algorithm in a purely functional language like Haskell,
and we we'll see soon why.
So let's implement the original algorithm in julia:

\begin{jl}
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
\end{jl}

The idea of that algorithm is to create an array (or vector) of boolean values up to a given limit,
all initially set to true.
Then we iterate over all natural numbers from 2 to the limit, and if the number is a
prime number (initially all numbers are marked as prime), we append the number to a
second (initially empty) array of prime numbers.

So, the first number to be added is 2.
Then, if the number is prime, we mark all multiples of it as not prime.
In the result, only real prime numbers are added to the primes array, as all others have
already been sieved out.

Now we can see why this is not possible in a purely functional language:\\
first, we need a data structure with constant time access to arbitrary elements.
Second, we need to do a lot of in-place updates in that structure for marking numbers as
not prime.
Both is not possible in a language based on linked data strucures without mutable values.

The final result from the sieving algorithm can be retrieved like so:

\begin{jl}
sieved = sieve(105000)
sieved[10001]
\end{jl}

On my machine running the algorithm takes about $400 \mu s$, which is much faster than the
$\mathcal{O}(n^2)$ version in Haskell.


