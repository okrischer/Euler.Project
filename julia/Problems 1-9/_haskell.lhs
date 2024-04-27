\section{Problem 2}

Create the \emph{Fibonacci sequence} with an infinite laszy list.

\begin{code}
evenFibSum :: Int -> Int
evenFibSum n = sum $ takeWhile (<= n) $ filter even $ fibSeq 1 2
  where fibSeq a b = a:fibSeq b (a+b)
\end{code}

\section{Problem 3}

\begin{code}
factorize :: Int -> Int
factorize n = divide n
  where
  divide n
    | even n = divide (n `div` 2)
    | otherwise = factor n 3
  factor m k
    | m == 1 = k
    | k * k > m = m
    | m `rem` k == 0 = factor (m `div` k) k
    | otherwise = factor m (k+2)
\end{code}