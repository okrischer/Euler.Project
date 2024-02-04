\chapter{Problem 005: Smallest Multiple}
2520 is the smallest number that can be divided by each of the numbers from 1 to 10
without any remainder.

\textbf{What is the smallest positive number that is evenly divisible by all of the
numbers from 1 to 20?}

\begin{code}
import Test.QuickCheck ( (==>), quickCheck, Property )
\end{code}

\section{Prime Factorization}

We are searching for the least commom multiple (\emph{lcm}) for all numbers in the range
$[2, 20]$.
While there is a closed formula to compute the \emph{lcm} of two numbers
\begin{equation*}
lcm(a, b) = \frac{ab}{gcd(a, b)}
\end{equation*}
where \emph{gcd(a,b)} is the greatest common divisor of a and b, this formula is not suitable for
the given problem, as we'd have to do this calculation for every pair of numbers in the given range.

Instead, we'll use \emph{prime factorization}:\\
Factor each number in the range and express it as a product of prime number powers. 
The \emph{lcm} will be the product of the highest powers of each prime factor.
For $n = 10$ as upper limit we'll get these highest powers of prime factors:

\begin{align*}
2^3 &= 8 \\
3^2 &= 9 \\
5^1 &= 5 \\
7^1 &= 7
\end{align*}

The result can now be computed easily:

\begin{spec}
result :: Int
result = product [8, 9, 5, 7]
\end{spec}

which will yield 2520, as specified in the problem statement. \\
Now, we could continue the sequence for the prime factors 11, 13, 17, and 19
to solve for $n = 20$: all those factors occur only with their highest power beeing 1.\\
But beware! the highest power for factor 2 is now 16, as $2^4 = 16$ is still less than 20.
The computation

\begin{spec}
result :: Int
result = product [16, 9, 5, 7, 11, 13, 17, 19]
\end{spec}

will yield the correct solution for the problem.
But, we want to go a step further and compute the result for any given upper limit of
the range.
For that, the first thing we need, is a list of prime numbers up to the limit:

\begin{code}
primes :: Int -> [Int]
primes n = takeWhile (<= n) $ sieve [2..]

sieve :: [Int] -> [Int]
sieve (x:xs) = x:sieve [y | y <- xs, rem y x /= 0]
\end{code}

Next we're traversing the list of prime factors, get the highest exponent for each and
compute the power:

\begin{code}
hp :: Int -> [Int] -> [Int]
hp _ [] = []
hp n (f:fs) = check f 1 : hp n fs
  where
    check f e
      | f^(e+1) <= n = check f (e+1)
      | otherwise = f^e
\end{code}

Solving for a given upper limit n is now easy:

\begin{code}
solve :: Int -> Int
solve n = product $ hp n $ primes n
\end{code}

\section{Testing}

For testing, we just check wether every number in the range $[2, n]$ actually does
divide the computed result evenly.

\begin{code}
divides :: Int -> Bool
divides n = test [2..n]
  where 
    test [] = True
    test (x:xs) = result `rem` x == 0 && test xs
    result = solve n

eachDivides :: Int -> Property
eachDivides n = n > 1 && n <= 30 ==> divides n
\end{code}

\begin{code}
main = do
  quickCheck eachDivides
\end{code}
