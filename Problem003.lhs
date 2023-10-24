\chapter{Problem 003: Largest prime factor}
The prime factors of 13195 are 5, 7, 13 and 29.

\textbf{What is the largest prime factor of the number 600851475143?}

\begin{code}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Criterion.Main
import Test.QuickCheck ( (==>), quickCheck, Property )
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
    | p `divides` n = p:getPF (n `div` p) primes
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

This solution can be implemented like so:

\begin{code}
factorOut number
    | num == 1  = last
    | otherwise = num
    where
        (n,l,k) = factorize (number, 1, 2)
        (num, last, fact) = largest (n,l,3)

factorize (n,l,k) | k `divides` n = factorize (n `div` k, k, k)
                  | otherwise = (n,l,k)

largest (n,l,k) | n > 1 && k^2 <= n = largest (num, last, fact+2)
                | otherwise = (n,l,k)
                where (num, last, fact) = factorize (n,l,k)

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0
\end{code}

\section{Testing}

\begin{code}
factorOutDevidesN :: Integer -> Property
factorOutDevidesN n = n > 1 ==> factorOut n `divides` n

checkPrimesDevidesN :: Integer -> Property
checkPrimesDevidesN n = n > 1 ==> checkPrimes n `divides` n

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
