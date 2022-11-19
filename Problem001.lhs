\chapter{Problem 001: Multiples of 3 or 5}

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

\textbf{Find the sum of all the multiples of 3 or 5 below 1000.}

\begin{code}
import Test.QuickCheck ( (==>), quickCheck, Property )
\end{code}

\section{Naive solution based on list comprehension}

\begin{code}
sumMultiplesNaive :: Int -> Int
sumMultiplesNaive n = 
    sum [x | x <- [1..n-1], x `rem` 3 == 0 || x `rem` 5 == 0]
\end{code}

The runtime complexity of this algorithm is linear to the input size $n$, thus $\Theta (n)$.

\section{Improved solution using triangular numbers}

The starting point for developing an efficient solution is the following idea:
instead of checking if the target value is divisible by 3 or 5, we can check separately for division of 3 and 5 and then add the results.
But then we have to subtract the sum of numbers divisible by 15 $(= 3 * 5)$, as we have added them twice in the first step.
If we define a function \mintinline{haskell}{sumMultiplesOf :: Int -> Int -> Int}, we can express the result like so:

\begin{code}
sumMultiplesOpt :: Int -> Int
sumMultiplesOpt n = multOf3 + multOf5 - multOf15
    where   multOf3  = sumMultiplesOf 3 n
            multOf5  = sumMultiplesOf 5 n
            multOf15 = sumMultiplesOf 15 n
\end{code}

If we would apply our naive implementation to \mintinline{haskell}{sumMultiplesOf} for 3 and 5 we get:

\begin{align*}
3 + 6 + 9 + \cdots + 999 &= 3*(1 + 2 + 3 +\cdots + 333) \\
5 + 10 + 15 + \cdots + 995 &= 5*(1 + 2 + 3 + \cdots + 199)
\end{align*}

Thus, we can apply the equation for \emph{Triangular Numbers}

\begin{equation*}
T_n = \sum_{k=1}^n k = 1+2+3+\cdots+n = \frac{n(n+1)}{2} 
\end{equation*}
on our function and we get:

\begin{code}
sumMultiplesOf :: Int -> Int -> Int
sumMultiplesOf factor limit = 
    let n = (limit - 1) `div` factor
    in factor * (n * (n+1) `div` 2)
\end{code}

Since \texttt{sumDivisibleBy} represents a closed formula, the runtime complexity of this algorithm is constant, thus $\Theta(1)$.

\section{Testing}

We will test our functions with \mintinline{haskell}{QuickCheck} by comparing their results for a generated range of input values.

\begin{code}
equalsOptNaive :: Int -> Property
equalsOptNaive n =
    n > 9 && n < 1000 ==> sumMultiplesNaive n == sumMultiplesOpt n
\end{code}

Executing the tests with \mintinline{haskell}{main}:

\begin{code}
main :: IO ()
main = quickCheck equalsOptNaive
\end{code}