\section{Problem 001}

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
\textbf{Find the sum of all the multiples of 3 or 5 below 1000!}

\subsection{Naive solution based on list comprehension}

\begin{code}
sumMultiplesNaive :: Integer -> Integer
sumMultiplesNaive n = 
    sum [x | x <- [1..n-1], x `rem` 3 == 0 || x `rem` 5 == 0]
\end{code}

The runtime complexity of this algorithm is linear to the input size $n$, thus $\mathcal{O}(n)$.

\subsection{Improved solution using triangular numbers}

The starting point for developing an efficient solution is the following idea: instead of checking if the target value is divisible by 3 and 5, we can check separately for division of 3 and 5 and add the results. But then we have to subtract the sum of numbers divisible by 15 $(= 3 * 5)$, as we have counted them twice in the first step. When we define a function \texttt{sumDivisibleBy :: Int -> Int -> Int}, we can express the result like so:

\begin{code}
sumMultiplesOptim :: Integer -> Integer
sumMultiplesOptim  n = divBy3 + divBy5 - divBy15
  where divBy3  = sumDivisibleBy 3 n 
        divBy5  = sumDivisibleBy 5 n
        divBy15 = sumDivisibleBy 15 n
\end{code}

If we apply our naive implementation on \texttt{sumDivisibleBy} for 3 and 5 we would then get:
\begin{align*}
3+6+9+12+\cdots+999 &= 3*(1+2+3+4+\cdots+333) \\
5+10+15+\cdots+995 &= 5*(1+2+3+\cdots+199)
\end{align*}

Thus, we can apply the equation for \emph{Triangular Numbers}
\begin{equation*}
T_n = \sum_{k=1}^n k = 1+2+3+\cdots+n = \frac{n*(n+1)}{2} 
\end{equation*}
on our function and we get:

\begin{code}
sumDivisibleBy :: Integer -> Integer -> Integer
sumDivisibleBy factor limit = 
  let n = (limit - 1) `div` factor
  in factor * (n*(n+1)) `div` 2
\end{code}

Since \texttt{sumDivisibleBy} represents a closed formula, the runtime complexity of this algorithm is constant, thus $\mathcal{O}(1)$.

\subsection{Triangular Numbers}
\subsubsection{Theorem}

\begin{equation} \label{eq:triangular}
T_n = \sum_{k=1}^n k = \frac{n(n+1)}{2}
\end{equation}

\subsubsection{Proof}
Triangular numbers are formed by stacking rows of the first n integers, creating a \emph{triangular geometric pattern}, e.g. for n=10:

\includegraphics{img/triangular}

It's easy to see that the number of elements in such a triangle is the sum of the integers from 1 to n. If we now geometrically combine two copies of $T_n$, the resulting rectancle will have side lengths of n and n+1:

\includegraphics{img/oblong}

Thus, the number of elements in this rectangle is $n(n+1)$.
Since such a rectangle has the double size of the underlying triangular number, the size of the triangular number is: 
\begin{equation*}
\frac{n(n+1)}{2} \blacksquare
\end{equation*}
