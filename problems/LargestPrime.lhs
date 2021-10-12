\section{Problem 003}
The prime factors of 13195 are 5, 7, 13 and 29.

\textbf{What is the largest prime factor of the number 600851475143?}

\subsection{Recursive Implementation}

\begin{code}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Criterion.Main
\end{code}

This recursive implementation has four parts:
\begin{itemize}
\item A function \texttt{factor} which finds the largest factor of a number
\item A function \texttt{isPrime} which checks for primality of a number
\item A helper function \texttt{divides}, implementing $k|n$
\item A function \texttt{largestPF} which just starts the computation
\end{itemize}

First of all, our entry-point: as we need to find the largest prime factor,
we start with an upper limit of $\sqrt{n}$ and iterate downwards:
\begin{code}
largestPF :: Integer -> Integer
largestPF number | even limit = factor number (limit-1)
                 | otherwise  = factor number limit
  where limit = truncate $ sqrt $ fromIntegral number :: Integer
\end{code}
Next, we find the first factor of n (which is the largest) and check for primality.
As even numbers cannot be prime, we start with an odd one and skip every second number:
\begin{code}
factor :: Integer -> Integer -> Integer
factor n k | k `divides` n && isPrime k = k
           | otherwise = factor n (k-2)

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0
\end{code}
Now, to the heart of our algorithm: we check for primality by iterating through all numbers
from 2 to $\sqrt{n}$, this time in ascending order, and check if $k$ divides $n$ ($k|n$):
\begin{code}
isPrime :: Integer -> Bool
isPrime number = solve number 2 limit
  where
    limit = truncate $ sqrt $ fromIntegral number :: Integer
    solve n k l | k > l         = True
                | k `divides` n = False
                | otherwise     = solve n (k+1) l
\end{code}
The crucial part of this algorithm is testing for primality with \texttt{isPrime},
therefor we try to find a better (more efficient) algorithm for testing.

\subsubsection{Using a function \texttt{ld} for finding the least divisor}
The idea is to find the least divisor (except 1) of a number and check it against the number itself:
if $ld(n)=n$ then $n$ is prime. With the follwing implementation we get rid of taking the
\emph{squareroot} as upper limit and thus obtain a more readable code. This code will be slightly less 
performant as the naive version, but it prepares for the next step of optimizing:
\begin{code}
isPrime' :: Integer -> Bool
isPrime' n = ld 2 n == n

ld :: Integer -> Integer -> Integer
ld k n | k `divides` n = k
       | k^2 > n       = n
       | otherwise     = ld (k+1) n
\end{code}

\subsubsection{Making \texttt{ld} more efficient}
With this invariant of \emph{least divisor} we are checking only against prime numbers
like this: check $p|n$ for \emph{primes p} with $2 \leq p \leq \sqrt{n}$.
Observe that the function \texttt{primes} generates an infinite list of prime numbers, which will
be only evaluated when needed. This is possible due to \emph{Haskell's} lazy computing model
and the way we are calling it: \texttt{primes} and \texttt{isPrime} are mutually defined recursive 
functions, thus prime numbers are only generated up to $n$.
The first call to \texttt{isPrime} using \texttt{ldp} takes much more time than subsequent calls,
as the list of primes hast to be generated in advance. But every subsequent call will be 
about ten times faster than a call to \texttt{ld}
(see section \ref{sec:bench}).

![benchmark results of prime tests](img/bench003) \

\begin{code}
isPrime'' :: Integer -> Bool
isPrime'' n = ldp primes n == n

ldp :: [Integer] -> Integer -> Integer
ldp (p:ps) n | p `divides` n = p
             | p^2 > n       = n
             | otherwise     = ldp ps n

primes :: [Integer]
primes = 2 : filter isPrime'' [3..]
\end{code}

\subsection{Benchmarking the solutions} \label{sec:bench}

\begin{code}
main :: IO ()
main = defaultMain [
  bgroup "primeTest" [ bench "naive" $ whnf isPrime   44560482149
                     , bench "least" $ whnf isPrime'  44560482149
                     , bench "prime" $ whnf isPrime'' 44560482149
                     ]
                   ]
\end{code}
