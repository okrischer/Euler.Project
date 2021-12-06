\section{Problem 002}
Each new term in the Fibonacci sequence is generated by adding the previous two terms.
By starting with 1 and 2, the first 10 terms will be:
\begin{equation*}
1, 2, 3, 5, 8, 13, 21, 34, 55, 89, \ldots
\end{equation*}
\textbf{By considering the terms in the Fibonacci sequence whose values do not
exceed four million, find the sum of the even-valued terms!}

\subsection{Recursive Implementation with Memoization}

The following implementation with memoization ist substancially faster then a
naive recursive implementation, which would follow the mathematical rule:

\begin{equation*}
fib (n) = fib (n-1) + fib (n-2)
\end{equation*}

\begin{code}
fibMem :: Int -> Int
fibMem limit = sum $ filter even $ run limit [1,1]
  where 
  run limit memo@(n1:n2:_) 
    | next > limit = memo
    | otherwise = run limit (next:memo) 
    where next = n1 + n2
\end{code}

\subsection{Imperative Implementation}

While the recursive implementation was based on working with lists, the following
implementation mimics an imperative solution in which the values of
$(a=n-2)$ and $(b=n-1)$ and the accumulated \texttt{sum} are passed to the next
recursive call:
\begin{code}
fibImp :: Int -> Int
fibImp limit = run limit (1,1) 0
  where
  run limit (a,b) sum 
    | c > limit = sum
    | otherwise = 
      if even c then run limit (b,c) (sum+c)
      else run limit (b,c) sum
    where c = a + b
\end{code}

\subsection{Further Improving}

Looking at the Fibonacci sequence
\begin{equation*}
1, 1, \textbf{2}, 3, 5, \textbf{8}, 13, 21, \textbf{34}, 55, 89, \textbf{144}, \ldots
\end{equation*}
we can easily see that every third Fibonacci number is even.
If this holds true for all Fibonacci numbers, we can get rid of the test for \texttt{even} like this:

\begin{code}
fibOpt :: Int -> Int
fibOpt limit = run limit (1,1,2) 0
  where
  run limit (a, b, c) sum 
    | c > limit = sum
    | otherwise = run limit (a', b', c') (sum + c)
    where 
    a' = c  + b
    b' = a' + c
    c' = a' + b'
\end{code}

\subsection{Fibonacci Numbers}
\subsubsection{Theorem}
Every third Fibonacci number is even.

\subsubsection{Proof}

Following the rule for Fibonacci numbers that every next number is the sum of it's two predecessors or more rigourus

\begin{equation*}
fib (n) = fib (n-1) + fib (n-2), where fib\{0,1\} = 1
\end{equation*}

we get an \emph{even} number if both preceeding numbers are odd,
and an \emph{odd} number if only one of the predecessors is odd.
Given the starting values of $fib(n)$ with $\{1,1\}$ (both \emph{odd}),
we get \texttt{2} as the first successor, which is \emph{even}. 
We now have a sequence of $\{1,1,2\}$, which is $\{odd, odd, even\}$.
Given this sequence of the first three Fibonacci numbers $\{a,b,c\}$,
we can show that every following sequence of three numbers $\{a', b', c'\}$
must also be $\{odd, odd, even\}$:

\begin{subequations}
\begin{equation}
\{a,b,c\}=\{odd,odd,even\} \implies \{a',b',c'\}=\{odd,odd,even\}
\end{equation}
\begin{align}
\{a',b',c'\}&=\{(c+b), (a'+c), (a'+b')\} \label{eq:odd1} \\
&=\{(even+odd), (a'+c), (a'+b')\} \\
&=\{odd, (odd+even), (a'+b')\} \\
&=\{odd, odd, (odd+odd)\} \\
&=\{odd, odd, even\}
\end{align}
\end{subequations}
