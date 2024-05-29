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

\section{Problem 5}
Compute the product of the highest powers of prime factors up to a given limit.

\begin{code}
solve :: Int -> Int
solve n = prod n $ hp n $ getPF [2..n]
  where
    getPF [] = []
    getPF (x:xs) = pf x (2:[3,5..n]) : getPF xs

hp :: Int -> [[Int]] -> [Int]
hp 1 _ = []
hp n f = maximum (map (length . filter (==n)) f) : hp (n-1) f

prod :: Int -> [Int] -> Int
prod n [] = 1
prod n (x:xs)
  | x == 0 = prod (n-1) xs
  | otherwise = n^x * prod (n-1) xs

pf :: Int -> [Int] -> [Int]
pf n nums@(x:xs)
  | x*x > n = [n]
  | rem n x == 0 = x : pf (div n x) nums
  | otherwise = pf n xs
\end{code}

