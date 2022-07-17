\chapter{Problem 001: Multiples of 3 or 5}

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

\textbf{Find the sum of all the multiples of 3 or 5 below 1000.}

\section{Naive solution based on list comprehension}

\begin{code}
import Control.Exception (assert)
import Test.QuickCheck ( (==>), quickCheck, Property )

sumMultiplesNaive :: Integer -> Integer
sumMultiplesNaive n = 
    sum [x | x <- [1..n-1], x `rem` 3 == 0 || x `rem` 5 == 0]
\end{code}

The runtime complexity of this algorithm is linear to the input size $n$, thus $\Theta (n)$.

\section{Improved solution using triangular numbers}

The starting point for developing an efficient solution is the following idea:
instead of checking if the target value is divisible by 3 or 5, we can check separately for division of 3 and 5 and then add the results.
But then we have to subtract the sum of numbers divisible by 15 $(= 3 * 5)$, as we have counted them twice in the first step.
If we define a function \mintinline{haskell}{sumDivisibleBy :: Integer -> Integer -> Integer}, we can express the result like so:

\begin{code}
sumMultiplesOptim :: Integer -> Integer
sumMultiplesOptim n = divBy3 + divBy5 - divBy15
    where   divBy3  = sumDivisibleBy 3 n
            divBy5  = sumDivisibleBy 5 n
            divBy15 = sumDivisibleBy 15 n
\end{code}

If we would apply our naive implementation to \mintinline{haskell}{sumDivisibleBy} for 3 and 5 we get:

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
sumDivisibleBy :: Integer -> Integer -> Integer
sumDivisibleBy factor limit = 
    let n = (limit - 1) `div` factor
    in factor * (n * (n+1) `div` 2)
\end{code}

Since \texttt{sumDivisibleBy} represents a closed formula, the runtime complexity of this algorithm is constant, thus $\Theta(1)$.

\section{Testing}

Besides checking the result with \mintinline{haskell}{assert}, we will test our functions with \mintinline{haskell}{QuickCheck}, which is especially helpful if we have multiple versions of the solution.

\begin{code}
sumMultiplesProp :: Integer -> Property
sumMultiplesProp n = 
    n >= 1 ==> sumMultiplesNaive n == sumMultiplesOptim n
\end{code}

Executing the tests within \mintinline{haskell}{main}:

\begin{code}
main :: IO ()
main = do
    assert (sumMultiplesNaive 1000 == 233168)
        putStrLn "+++ OK, result is correct."
    quickCheck sumMultiplesProp
\end{code}