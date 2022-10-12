\chapter{Problem 004: Largest palindrome product}

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is $9009 = 91 \times 99$.

\textbf{Find the largest palindrome made from the product of two 3-digit numbers.}

\section{Naive solution with exhaustive search}

\begin{code}
module Problem004 where
\end{code}

Multiplying two 3-digits numbers gives numbers with at most six digits ($999 \cdot 999 = 998001$).
Thus, we are searching for a 6-digit number, having a `9' in the first place (most significand digit). \\
The smallest of these numbers is 900900, beeing the product $910 \times 990$.
Applying \emph{exhaustive search}, we first generate a list of all products from the intervals $[910, 999]$ and $[990, 999]$ and then filter out the ones that are palindromes.

\begin{code}
largPalNaive :: String
largPalNaive = maximum $ filter (\x -> x == reverse x)
               [show (m * n) | m <- [910..999], n <- [990..999]]
\end{code}

\section{Constraining the generating function}
The solution from the last section was fast but naive: we only generated $90 \cdot 10 = 900$ possible solutions, which were easy to filter; but we made the (possibly) wrong assumption that there will be a solution in the interval $[900900, 999999]$, which is actually hard to prove.
In fact, we were just lucky to find the correct result.

If we choose the complete interval $[100, 999]$ of 3-digit numbers for both numbers, we get $900 \cdot 900 = 810,000$ candidates, which will take some time to filter for palindromes.
So we have to constrain the generated candidates to be \emph{good} candidates.
Good in this context means that the candidates are more likely to be palindromes in fact.

The first thing to do is to take care that candidates are not generated twice; therefore we constrain $n$ to be greater or equal to $m$.
This reduces candidates from 810,000 to 405,450.

Second, we check if the candidates could be a palindrome by testing if they are divisible by 11.
Since 11 is a prime number, one of the factors $m$ or $n$ must be divisible by 11.
This reduces candidates to 69,660, which is less than a tenth of the original list; so we are done here.

\begin{code}
candidates :: [Int]
candidates = 
    [m * n | m <- [100..999], n <- [100..999],
    n >= m, m `rem` 11 == 0 || n `rem` 11 == 0]
\end{code}

\begin{code}
reverseNum :: Int -> Int
reverseNum n = run n 0
    where
    run 0 r = r
    run n r = run (n `div` 10) (10 * r + n `rem` 10)
\end{code}

\begin{code}
largPalOpt :: Int
largPalOpt = maximum $ filter (\x -> x == reverseNum x) candidates
\end{code}

\section{Testing}

\begin{code}
main :: IO ()
main = if read largPalNaive == largPalOpt
    then putStrLn "+++ OK, naive equals opt."
    else putStrLn "--- ER, naive and opt differ."
\end{code}