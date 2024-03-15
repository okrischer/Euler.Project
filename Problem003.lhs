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

With this approach, we generate an infininte but lazy evaluated list of
prime numbers, from which we take the potential prime factors.
The prime numbers are evaluated (if they divide the given number) until
their square exceeds the number.

\begin{code}
checkPrimes :: Int -> Int
checkPrimes n = maximum (getPF n primes)

getPF :: Int -> [Int] -> [Int]
getPF n primes@(p:ps)
    | p*p > n = [n]
    | rem n p == 0 = p:getPF (n `div` p) primes
    | otherwise = getPF n ps

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (x:xs) = x:sieve [y | y <- xs, rem y x /= 0]
\end{code}

\section{Factor out all Factors}

The key to this solution is a simple idea:
\emph{instead of just checking if a number divides $n$ we actually divide $n$ by this number}.

Repeatedly dividing $n$ by its factors decreases n very fast, making early
termination of the algorithm possible.

The algorithm works as follows:\
for each integer number $k \geq 2$, if $k$ is a factor of $n$, divide $n$ by $k$
and completely divide out each $k$ before moving to the next $k$.
When the next $k$ is a factor it will necessarily be prime, as all smaller factors
have already been removed.
After dividing out all prime factors $n$ will equal to 1.

\begin{code}
factor :: Int -> Int
factor n = divide n 2
  where
    divide m k
      | m == 1 = k
      | k * k > m = m
      | m `rem` k == 0 = divide (m `div` k) k
      | otherwise = divide m (k+1)
\end{code}

\section{Testing}

\begin{code}
factorDevidesN :: Int -> Property
factorDevidesN n = n > 1 ==> rem n (factor n) == 0

checkPrimesDevidesN :: Int -> Property
checkPrimesDevidesN n = n > 1 ==> rem n (checkPrimes n) == 0

equalResults :: Int -> Property
equalResults n = n > 1 ==> checkPrimes n == factor n
\end{code}

\begin{code}
main = do
    quickCheck factorDevidesN
    quickCheck checkPrimesDevidesN
    quickCheck equalResults
\end{code}

\begin{spec}
-- alternative main for benchmarking
main = defaultMain [
  bgroup "lpf" [ bench "factor" $ whnf factor   600851475143
               , bench "check"  $ whnf checkPrimes 600851475143
               ]
  ]
\end{spec}
