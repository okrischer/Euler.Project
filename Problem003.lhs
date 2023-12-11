\chapter{Problem 003: Largest prime factor}
The prime factors of 13195 are 5, 7, 13 and 29.

\textbf{What is the largest prime factor of the number 600851475143?}

\begin{code}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Criterion.Main
import Test.QuickCheck ( (==>), quickCheck, Property )
import Test.QuickCheck.Text (paragraphs)
\end{code}

\section{Checking for Prime Numbers}

With this approach, we generate an infininte but lazy evaluated list of prime numbers, from which we take the potential prime factors.
The prime numbers are evaluated (if they divide the given number) until their square exceeds the number.

\begin{code}
checkPrimes :: Integer -> Integer
checkPrimes n = maximum (getPF n primes)

getPF :: Integer -> [Integer] -> [Integer]
getPF n primes@(p:ps)
    | p*p > n = [n]
    | rem n p == 0 = p:getPF (n `div` p) primes
    | otherwise = getPF n ps

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x:sieve [y | y <- xs, rem y x /= 0]
\end{code}

\section{Factor out all Factors}

The key to this solution is a simple idea:
\emph{instead of just checking if a number divides $n$ we actually divide $n$ by this number}.

Repeatedly dividing $n$ by its factors decreases n very fast, making early termination of the algorithm possible.

The algorithm works as follows:\
for each integer number $k \geq 2$, if $k$ is a factor of $n$, divide $n$ by $k$ and completely divide out each $k$ before moving to the next $k$.
When the next $k$ is a factor it will necessarily be prime, as all smaller factors have already been removed.
After dividing out all prime factors $n$ will equal to 1.

\begin{code}
factorOut :: Integer -> Integer
factorOut number
    | num == 1  = last
    | otherwise = num
    where
        (n,l,k) = factorize (number, 1, 2)
        (num, last, fact) = largest (n,l,3)

factorize (n,l,k) | rem n k == 0 = factorize (n `div` k, k, k)
                  | otherwise = (n,l,k)

largest (n,l,k) | n > 1 && k^2 <= n = largest (num, last, fact+2)
                | otherwise = (n,l,k)
                where (num, last, fact) = factorize (n,l,k)
\end{code}

The problem with this imperative solution in Haskell is that it is much slower than the solution based on lazy lists (about seven times slower).
So, we'll give it a try with Go:

\begin{go}
func factorNaive(n int) int {
  factors := []int{}
  k := 2
  for k <= n {
    for n%k == 0 {
      factors = append(factors, k)
      n /= k
    }
    k += 1
  }
  return slices.Max(factors)
}
\end{go}

That works quite well ($\approx 10 \mu s$ per call), but there's still room for improvement:
\begin{enumerate}
\item we don't need to create an array of factors, we just need the biggest one
\item we factor out all 2s at first and then increase k by 2, starting with k=3
\item the upper bound for k is $\sqrt{n}$, as there could be only one prime factor greater then $\sqrt{n}$; if n is greater than 1 after dividing out all factors, then n is the greatest factor. 
\end{enumerate}

\begin{go}
func factorOpt(n int) int {
  k := 3
  for n%2 == 0 {
    n /= 2
  }
  for k*k <= n && n > 1 {
    for n%k == 0 {
      n /= k
    }
    k += 2
  }
  if n > 1 {
    return n
  } else {
    return k
  }
}
\end{go}

This solution is about 10 times faster than the naive imperative solution.
Even more interesting, the solution in Haskell based on lazy lists is on par with this optimized solution ($\approx 2 \mu s$).

\section{Testing}

\begin{code}
factorOutDevidesN :: Integer -> Property
factorOutDevidesN n = n > 1 ==> rem n (factorOut n) == 0

checkPrimesDevidesN :: Integer -> Property
checkPrimesDevidesN n = n > 1 ==> rem n (checkPrimes n) == 0

equalResults :: Integer -> Property
equalResults n = n > 1 ==> checkPrimes n == factorOut n
\end{code}

\begin{code}
main = do
    quickCheck factorOutDevidesN
    quickCheck checkPrimesDevidesN
    quickCheck equalResults
\end{code}

\begin{spec}
-- alternative main for benchmarking
main = defaultMain [
  bgroup "lpf" [ bench "factor" $ whnf factorOut   600851475143
               , bench "check"  $ whnf checkPrimes 600851475143
               ]
  ]
\end{spec}
