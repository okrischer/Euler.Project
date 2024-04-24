\section{Problem 2}

\begin{code}
evenFibSum :: Int -> Int
evenFibSum n = sum $ takeWhile (<= n) $ filter even $ fibSeq 1 2
  where fibSeq a b = a:fibSeq b (a+b)
\end{code}