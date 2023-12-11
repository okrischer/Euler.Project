\chapter{Problem 002: Even Fibonacci Numbers}
Each new term in the Fibonacci sequence is generated by adding the previous two terms.
By starting with 1 and 2, the first 10 terms will be:

\begin{equation*}
1, 2, 3, 5, 8, 13, 21, 34, 55, 89, \ldots
\end{equation*}

\textbf{By considering the terms in the Fibonacci sequence whose values do not
exceed four million, find the sum of the even-valued terms.}

\begin{code}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Criterion.Main
import Test.QuickCheck ( (==>), quickCheck, Property )
\end{code}

\section{Recursive Implementation with Memoization}

The following implementation with memoization ist substancially faster then a
naive recursive implementation, which would follow the mathematical rule:

\begin{equation*}
fib(n) = fib(n-1) + fib(n-2).
\end{equation*}

\begin{code}
fibMem :: Integer -> Integer
fibMem limit = sum $ filter even $ run limit [1,1]
    where run limit memo@(a:b:_)
            | next > limit = memo
            | otherwise = run limit (next:memo)
            where next = a + b
\end{code}

\section{Functional approach}

Using lazy list evaluation and higher order functions we can implement a more idiomatic Haskell solution:

\begin{code}
fibFun :: Integer -> Integer
fibFun limit = sum $ takeWhile (<= limit) $ filter even $ fibs 1 2
    where fibs a b = a : fibs b (a + b)
\end{code}

\section{Imperative Implementation}

While the recursive and functional implementations were based on working with lists in Haskell, the following implementation gives an imperative solution in Go:

\begin{go}
func fibImp(limit int) int {
  a := 1
  b := 1
  s := 0
  for b <= limit {
    if b%2 == 0 {
      s += b
    }
    a, b = b, a+b
  }
  return s
}
\end{go}

\section{Further Improving}

Looking at the Fibonacci sequence

\begin{equation*}
1, 1, \textbf{2}, 3, 5, \textbf{8}, 13, 21, \textbf{34}, 55, 89, \textbf{144}, \ldots
\end{equation*}

we can easily see that every third Fibonacci number is even.
Thus, we can get rid of the test for \mintinline{go}{b%2 == 0} like this:

\begin{go}
func fibOpt(limit int) int {
  a := 1
  b := 1
  c := 2
  s := 0
  for c <= limit {
    s += c
    a = b + c
    b = a + c
    c = a + b
  }
  return s
}
\end{go}

\section{Testing}

\begin{code}
equalsMemFun :: Integer -> Property
equalsMemFun n = n > 0 ==> fibMem n == fibFun n

main = do
  quickCheck equalsMemFun
\end{code}

\section{Benchmark}

\begin{spec}
-- alternative main for benchmarking
main = defaultMain [
  bgroup "fib" [ bench "Mem"  $ whnf fibMem 4000000
               , bench "Fun"  $ whnf fibFun 4000000
               ]
  ]
\end{spec}

From this benchmark we see that \mintinline{haskell}{fibFun} is about twice as fast as \mintinline{haskell}{fibMem}.

A benchmark for Go can be generated inside a testfile like so:

\begin{go}
func BenchmarkProblem002FibImp(b *testing.B) {
  for i := 0; i < b.N; i++ {
  fibImp(4000000)
  }
}
\end{go}

From that, we see that \mintinline{go}{fibOpt} is about three times faster than \mintinline{go}{fibImp}.

