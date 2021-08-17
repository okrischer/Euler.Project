\documentclass[12pt]{article}
\usepackage{mathtools}
%include polycode.fmt
\title{Multiples of 3 and 5}
\author{Oliver Krischer}
\begin{document}
\maketitle
\begin{code}
module Main where

import Criterion.Main

main = defaultMain [ 
  bgroup "naive"
      [ bench "100"  $ whnf sumMultiplesNaive 100 ],
  bgroup "optim"
      [ bench "1000"  $ whnf sumMultiplesOptim 1000
      , bench "1000000"  $ whnf sumMultiplesOptim 1000000
      , bench "1000000000"  $ whnf sumMultiplesOptim 1000000000 ]
  ]
\end{code}
\section{Problem}
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

\textbf{Find the sum of all the multiples of 3 or 5 below 1000!}
\section{Solution}
\subsection{Naive Solution Approach}
\begin{code}
sumMultiplesNaive :: Integer -> Integer
sumMultiplesNaive n = 
    sum [x | x <- [1..n - 1], x `rem` 3 == 0 || x `rem` 5 == 0]
\end{code}
\subsection{Developing an efficient solution}
The starting point for developing an efficient solution is the following idea:
instaed of checking if the target value is divisible by 3 and 5, we can check separately for division of |3 and 5| and add the results. But then we have to subtract the sum of numbers divisible by |15 (= 3 * 5)|, as we have counted them twice in the first step.
When we define a function |sumDivisibleBy :: Int -> Int -> Int| we can express the result like so:
\begin{code}
sumMultiplesOptim :: Integer -> Integer
sumMultiplesOptim n = 
  sumDivisibleBy3 n + 
  sumDivisibleBy5 n - 
  sumDivisibleBy15 n
\end{code}
If we apply our naive implemantation on |sumDivisibleBy| for 3 and 5 we would get:
\[3+6+9+12+\cdots+999 = 3*(1+2+3+4+\cdots+333)\]
\[5+10+15+\cdots+995 = 5*(1+2+3+\cdots+199)\]
Thus, we can apply the equation for |Triangular Numbers|
\[T_n = \sum_{k=1}^n k = 1+2+3+\cdots+n = \frac{n*(n+1)}{2}\]
on our function and we get:
\begin{code}
sumDivisibleBy :: Integer -> Integer -> Integer
sumDivisibleBy denom limit = 
  let 
      n = (limit - 1) `div` denom
  in 
      denom * (n * (n + 1)) `div` 2

sumDivisibleBy3 = sumDivisibleBy 3
sumDivisibleBy5 = sumDivisibleBy 5
sumDivisibleBy15 = sumDivisibleBy 15
\end{code}
\subsection{Complexity Analysis}
The naive implementation has a runtime complexity of $\mathcal{O}(n)$, as the algorithm has to iterate through the entire list of numbers from 1 up to n. So, with growing n, the runtime ist growing proportional to n (i.e. if n is doubled, the runtime will double as well).
The optimized version uses a simple formula, which is independent of the growth of n. Thus, the complexity here is constant, or $\mathcal{O}(1)$.
\end{document}