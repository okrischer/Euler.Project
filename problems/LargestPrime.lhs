\section{Problem 003}
The prime factors of 13195 are 5, 7, 13 and 29.

\textbf{What is the largest prime factor of the number 600851475143?}

\subsection{Naive functional Approach}

\begin{code}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Control.Monad ( replicateM_ )
import Control.Monad.ST ( runST, ST )
import Data.STRef ( newSTRef, readSTRef, writeSTRef )
\end{code}

This recursive implementation has four parts:
\begin{itemize}
\item A function \texttt{factor} which finds the largest factor of a number
\item A function \texttt{isPrime} which checks for primality of a number
\item A helper function \texttt{divides}, implementing $k|n$
\item A function \texttt{largestPF} which just starts the computation
\end{itemize}

First of all, our entry-point: as we need to find the largest prime factor, we start with an upper limit of $\sqrt{n}$ and iterate downwards:
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
factor n k | k `divides` n && isPrime'' k = k
           | otherwise = factor n (k-2)

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0
\end{code}
Now, to the heart of our algorithm: we check for primality by iterating through all numbers from 2 to $\sqrt{n}$, this time in ascending order, and check if $k$ divides $n$ ($k|n$):
\begin{code}
isPrime :: Integer -> Bool
isPrime number = solve number 2 limit
  where
    limit = truncate $ sqrt $ fromIntegral number :: Integer
    solve n k l | k > l         = True
                | k `divides` n = False
                | otherwise     = solve n (k+1) l
\end{code}
The crucial part of this algorithm is testing for primality with \texttt{isPrime}, therefor we try to find a better (more efficient) algorithm for testing.

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
With this invariant of \emph{least divisor} we are checking only against prime numbers like this: check $p|n$ for \emph{primes p} with $2 \leq p \leq \sqrt{n}$.
Observe that the function \texttt{primes} generates an infinite list of prime numbers, which will be only evaluated when needed.
This is possible due to \emph{Haskell's} lazy computing model and the way we are calling it: \texttt{primes} and \texttt{isPrime} are mutually recursive functions, thus prime numbers are only generated up to $n$.
The first call to \texttt{isPrime} using \texttt{ldp} takes much more time than subsequent calls, as the list of primes hast to be generated in advance. 
But every subsequent call will be about ten times faster than a call to \texttt{ld} (for generating the benchmarks see section \ref{sec:bench}).
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

\subsection{Imperative Approach}
As we have already seen, it is sometimes convinient to write code inspired by imperative programming.
Not only makes this the code more compact, but also in many cases more efficient.
But be aware that it is not possible to write \emph{real} imperative code in Haskell.
Haskell is a pure functional language, not allowing to re-assingn values to variables which have already been bound to a value.
In order to "re-assign" values, we have to pass them as arguments to recursive function calls (unless we use monads in Haskell as in section \ref{sec:monads}).
The resulting code is still pure functional code, but inspired by the idea of mutable values.
\begin{code}
largestPF' :: Integer -> Integer
largestPF' number =
  let (num, last, fact) = largest (n,l,3) in
    if num == 1 then last else num
  where (n,l,f) = factorize (number, 1, 2)

factorize :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
factorize (n,l,f) | f `divides` n = factorize (n `div` f, f, f)
                  | otherwise     = (n,l,f)

largest :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
largest (n,l,f) | n > 1 && f^2 < n = largest (num, last, fact+2)
                | otherwise        = (n,l,f)
                where (num, last, fact) = factorize (n,l,f)
\end{code}

This imperative solution is much more efficient than our naive appoach (about 200 times faster).
Without going into details of asymptotic calculation, we can easily see why:
where our naive solution had to iterate through every second element in the search space, a call to \texttt{factorize} in the imperative solution reduces the search space by factor \texttt{f}. 
And there is even no need of explicit primality checking, because every new found factor must be prime, as all lower factors have already been removed by earlyer calls to \texttt{factorize}.
\subsection{Using Monads in Haskell} \label{sec:monads}
Here is an example of \emph{real} imperative programming in Haskell using the state-thread monad \texttt{Control.Monad.ST} to calculate Fibonacci numbers.
As a reference, here is an efficient functional approach:
\begin{code}
fibImp :: Int -> Integer 
fibImp n = fst (run n)
  where 
    run 0 = (0,1)
    run n = (b, a+b) where (a,b) = run (n-1)
\end{code}
And here the real imperative version with mutable references:
\begin{code}
fibMut :: Int -> Integer 
fibMut n = runST (fibST n)

fibST :: Int -> ST s Integer
fibST n = do
  a <- newSTRef 0
  b <- newSTRef 1
  replicateM_ n (do 
    x <- readSTRef a
    y <- readSTRef b
    writeSTRef a y
    writeSTRef b $! (x+y))
  readSTRef a
\end{code}
