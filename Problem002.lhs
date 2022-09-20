\chapter{Problem 002: Even Fibonacci Numbers}
Each new term in the Fibonacci sequence is generated by adding the previous two terms.
By starting with 1 and 2, the first 10 terms will be:

\begin{equation*}
1, 2, 3, 5, 8, 13, 21, 34, 55, 89, \ldots
\end{equation*}

\textbf{By considering the terms in the Fibonacci sequence whose values do not
exceed four million, find the sum of the even-valued terms.}

\section{Recursive Implementation with Memoization}

\begin{code}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Problem002 where
import Test.QuickCheck ( (==>), quickCheck, Property )
\end{code}

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

\section{Imperative Implementation}

While the recursive implementation was based on working with lists, the following implementation mimics an imperative solution in which the current values and the accumulated \texttt{sum} are modified and passed to the next recursive call:

\begin{code}
fibImp :: Integer -> Integer
fibImp limit = run limit (1,1) 0
    where run limit (a,b) sum
            | c > limit = sum
            | even c = run limit (b,c) (sum+c)
            | otherwise = run limit (b,c) sum
            where c = a + b
\end{code}

\section{Further Improving}

Looking at the Fibonacci sequence

\begin{equation*}
1, 1, \textbf{2}, 3, 5, \textbf{8}, 13, 21, \textbf{34}, 55, 89, \textbf{144}, \ldots
\end{equation*}

we can easily see that every third Fibonacci number is even.
Thus, we can get rid of the test for \mintinline{haskell}{even} like this:

\begin{code}
fibOpt :: Integer -> Integer
fibOpt limit = run limit (1,1,2) 0
    where run limit (a, b, c) sum 
            | c > limit = sum
            | otherwise = run limit (a', b', c') (sum+c)
            where 
                a' = c  + b
                b' = a' + c
                c' = a' + b'
\end{code}

\section{Functional approach}

Using lazy list evaluation and higher order functions we can implement a more idiomatic Haskell solution:

\begin{code}
fibFun :: Integer -> Integer
fibFun limit = sum $ takeWhile (<= limit) $ filter even fib
    where fib = 1:1:zipWith (+) fib (tail fib)
\end{code}

\section{Testing}

\begin{code}

equalsImpMem :: Integer -> Property
equalsImpMem n = n > 0 ==> fibImp n == fibMem n

equalsImpOpt :: Integer -> Property
equalsImpOpt n = n > 0 ==> fibImp n == fibOpt n

equalsFunOpt :: Integer -> Property
equalsFunOpt n = n > 0 ==> fibFun n == fibOpt n

test :: IO ()
test = do
    putStrLn "------- testing Problem002"
    if fibMem 4000000 == 4613732
        then putStrLn "+++ OK, result is correct."
        else error "result is wrong."
    quickCheck equalsImpMem
    quickCheck equalsImpOpt
    quickCheck equalsFunOpt
\end{code}