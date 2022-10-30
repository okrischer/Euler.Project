\chapter{Problem 003: Largest prime factor}
The prime factors of 13195 are 5, 7, 13 and 29.

\textbf{What is the largest prime factor of the number 600851475143?}

\section{Functional Approach}

\begin{code}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Criterion.Main
import Test.QuickCheck ( (==>), quickCheck, Property )
\end{code}

With this approach, we generate an infininte but lazy evaluated list of prime numbers, from which we take the potential prime factors.
The factors are evaluated (if they divide the given number) until their square exceeds the number.
Therfore the list will not be evaluated to infinity.

\begin{code}
lpfFun :: Integer -> Integer
lpfFun n = maximum (listPF n 0)

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

\section{Imperative Approach}

The key to this solution is a simple idea:
\emph{instead of just checking if a number divides $n$ we actually divide $n$ by this number}.

Repeatedly dividing $n$ by its factors decreases n very fast, making early termination of the algorithm possible.

The algoritm works as follows:\
for each integer number $k \geq 2$, if $k$ is a factor of $n$, divide $n$ by $k$ and completely divide out each $k$ before moving to the next $k$.
When the next $k$ is a factor it will necessarily be prime, as all smaller factors have already been removed.
After dividing out all prime factors $n$ will equal to 1.

A step by step description of the algorithm would be:
\begin{enumerate}
\item Start with 2: while $n$ is divisible by 2, divide $n$ by 2. After this step $n$ is an odd number.
\item Set the upper limit of $k$ to $\sqrt{n}$. Every number $n$ can at most have one prime factor greater than its square root. If $k$ exceeds the square root, the reamining number will be prime.
\item Iterate through the odd $k$s starting from 3 until $n = 1$ or $k > \sqrt{n}$: for every $k$, completely factor out each $k$. Repeat step 2 for every $k$.
\item Check if the remaining $n$ is greater than 1. If so, return $n$, otherwise return the largest prime factor found. This follows from step 2.
\end{enumerate}

\begin{julia}
function largest_factor_opt(n)
    k = 3
    max_factor = 1
    
    while n % 2 == 0
        max_factor = 2
        n = n ÷ 2
    end
    
    while n > 1 && k^2 <= n
        while n % k == 0
            max_factor = k
            n = n ÷ k
        end
        k += 2
    end
    
    n > 1 ? n : max_factor
end
\end{julia}

The imperative solution could be implemented like so with Haskell:

\begin{code}
lpfImp :: Integer -> Integer
lpfImp number
    | num == 1  = last
    | otherwise = num
    where
        (n,l,f) = factorize (number, 1, 2)
        (num, last, fact) = largest (n,l,3)

factorize (n,l,f) | f `divides` n = factorize (n `div` f, f, f)
                  | otherwise     = (n,l,f)

largest (n,l,f) | n > 1 && f^2 <= n = largest (num, last, fact+2)
                | otherwise        = (n,l,f)
                where (num, last, fact) = factorize (n,l,f)

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0
\end{code}

\section{Testing}

\begin{code}
lpfImpDevidesN :: Integer -> Property
lpfImpDevidesN n = n > 1 ==> lpfImp n `divides` n

lpfFunDevidesN :: Integer -> Property
lpfFunDevidesN n = n > 1 ==> lpfFun n `divides` n

equalsImpFun :: Integer -> Property
equalsImpFun n = n > 1 ==> lpfFun n == lpfImp n
\end{code}

\begin{code}
main :: IO ()
main = do
    quickCheck lpfImpDevidesN
    quickCheck lpfFunDevidesN
    quickCheck equalsImpFun
\end{code}

\begin{spec}
-- alternative main for benchmarking
main = defaultMain [
  bgroup "lpf" [ bench "Imp"  $ whnf lpfImp 600851475143
               , bench "Fun"  $ whnf lpfFun 600851475143
               ]
  ]
\end{spec}