\chapter{Problem 006: Sum Square Differnce}

The sum of the squares of the first ten natural numbers is
$$1^2 + 2^2 + \dots + 10^2 = 385.$$
The square of the sum of the first ten natural numbers is
$$(1 + 2 + \dots + 10)^2 = 55^2 = 3025.$$

Hence the difference between the sum of the squares of the first ten natural numbers and the
square of the sum is $3025 - 385 = 2640$.

\textbf{Find the difference between the sum of the squares of the first one hundred natural numbers
and the square of the sum.}

\begin{code}
import Test.QuickCheck ( (==>), quickCheck, Property )
\end{code}

\section{Iterative Solution}

We can use higher-order functions, which will iterate over all elements in a list.
Thus, runtime complexity is $\Theta(n)$.

\begin{code}
squareOfSum :: Int -> Int
squareOfSum n = sum [1..n] ^ 2

sumOfSquare :: Int -> Int
sumOfSquare n = sum $ map (^2) [1..n]

iterResult :: Int -> Int
iterResult n = squareOfSum n - sumOfSquare n
\end{code}

\section{Solving with closed Formula}

We can apply the formula for \emph{Triangular Numbers} to compute the square of the sum:
$$T_n = \sum_{k=1}^n k = 1+2+3+\cdots+n = \frac{n(n+1)}{2}.$$
There's also a formula for computing the sum of squares:
$$\frac{n (n+1) (2n + 1)}{6}.$$
Thus, runtime complexity is $\Theta(1)$.

\begin{code}
sqSumClosed :: Int -> Int
sqSumClosed n = (n * (n+1) `div` 2) ^ 2

sumSqClosed :: Int -> Int
sumSqClosed n = (n * (n+1) * (2*n + 1)) `div` 6

closedResult :: Int -> Int
closedResult n = sqSumClosed n - sumSqClosed n
\end{code}

\section{Testing}

\begin{code}
iterEqClosed :: Int -> Property
iterEqClosed n = n > 1 ==> iterResult n == closedResult n

main = do
  quickCheck iterEqClosed
\end{code}
