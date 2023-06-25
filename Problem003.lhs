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
checkPrimes n = maximum (listPF n 0)

listPF :: Integer -> Int -> [Integer]
listPF 0 _ = []
listPF n i
    | p*p > n = [n]
    | p `divides` n = p:listPF (n `div` p) 0
    | otherwise = listPF n (i+1)
    where p = primes!!i

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:ns) = p:sieve [n | n <- ns, rem n p /= 0]
\end{code}

\section{Factor out all Factors}

The key to this solution is a simple idea:
\emph{instead of just checking if a number divides $n$ we actually divide $n$ by this number}.

Repeatedly dividing $n$ by its factors decreases n very fast, making early termination of the algorithm possible.

The algoritm works as follows:\
for each integer number $k \geq 2$, if $k$ is a factor of $n$, divide $n$ by $k$ and completely divide out each $k$ before moving to the next $k$.
When the next $k$ is a factor it will necessarily be prime, as all smaller factors have already been removed.
After dividing out all prime factors $n$ will equal to 1.

A step by step description of the algorithm would be:
\begin{enumerate}
\item Start with $k=2$: while $n$ is divisible by 2, divide $n$ by 2. After this step $n$ is an odd number.
\item Iterate through the odd $k$s starting from 3 until $n = 1$ or $k^2 > n$: every number $n$ can at most have one prime factor greater than its square root. If $k^2$ exceeds $n$, the reamining number will be prime. For every $k$, completely factor out each $k$. 
\item Check if the remaining $n$ equals 1. If so, return the largest prime factor found, otherwise return $n$.
\end{enumerate}

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