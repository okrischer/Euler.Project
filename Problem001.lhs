\chapter{Problem 001: Multiples of 3 or 5}

If we list all the natural numbers below 10 that are multiples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these multiples is 23.

\textbf{Find the sum of all the multiples of 3 or 5 below 1000.}

\begin{code}
import Criterion.Main
import Test.QuickCheck ( (==>), quickCheck, Property )
import GHC.StgToCmm.ExtCode (code)
\end{code}

\section{Naive solution based on list comprehension}

\begin{code}
filterMultiples :: Int -> Int
filterMultiples n = 
    sum [x | x <- [1..n-1], x `rem` 3 == 0 || x `rem` 5 == 0]
\end{code}

The runtime complexity of this algorithm is linear to the input size $n$, thus $\Theta(n)$.

\section{Improved solution using triangular numbers}

The starting point for developing an efficient solution is the following idea:
instead of checking if the target value is divisible by 3 or 5, we can check
separately for division of 3 and 5 and then add the results.
But then we have to subtract the sum of numbers divisible by 15 $(= 3 * 5)$,
as we have added them twice in the first step.
If we define a function \mintinline{haskell}{triangular :: Int -> Int -> Int},
we can express the result like so:

\begin{code}
closedFormula :: Int -> Int
closedFormula n = m3 + m5 - m15
    where   m3  = triangular 3 n
            m5  = triangular 5 n
            m15 = triangular 15 n
\end{code}

If we would apply our naive implementation for 3 and 5 we'd get:

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
triangular :: Int -> Int -> Int
triangular factor limit = 
    let n = (limit - 1) `div` factor
    in factor * (n * (n+1) `div` 2)
\end{code}

Since \mintinline{haskell}{triangular} represents a closed formula,
the runtime complexity of this algorithm is constant, thus $\Theta(1)$.

\section{Testing}

We will test our functions with \mintinline{haskell}{QuickCheck} by comparing
their results for a generated range of input values.

\begin{code}
testProp :: Int -> Property
testProp n = n > 9 ==> filterMultiples n == closedFormula n
\end{code}

Executing the tests with \mintinline{haskell}{main}:

\begin{code}
main = quickCheck testProp
\end{code}

\begin{spec}
-- alternative main for benchmarking
main = defaultMain [
  bgroup "multiples"  [ bench "filter"  $ whnf filterMultiples 1000
                      , bench "closed"  $ whnf closedFormula 1000
                      ]
  ]
\end{spec}

If you want to get a feeling about the difference between a runtime complexity
of $\Theta(1)$ and $\Theta(n)$, hava a look at the benchmark results for problem001
in the \texttt{bench} folder of the root directory.
