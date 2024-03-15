\chapter{Problem 007: 10,001st Prime}

By listing the first six prime numbers: $2,3,5,7,11$, and $13$, we can see that the
$6$th prime is $13$.

\textbf{What is the $10,001$st prime number?}

\begin{code}
import Test.QuickCheck ( (==>), quickCheck, Property )
\end{code}

\section{Using a lazy list}

To solve the problem, we will use our well-known function \mintinline{haskell}{sieve},
which generates a lazy list of prime numbers:

\begin{code}
sieve :: [Int] -> [Int]
sieve (x:xs) = x:sieve [y | y <- xs, rem y x /= 0]

primes :: [Int]
primes = sieve [2..]
\end{code}

Now, we only need to get n'th one:

\begin{code}
solve :: Int -> Int
solve n = head $ take 1 $ drop (n-1) primes
\end{code}

\section{Using a real sieve}

When you're calling \mintinline{haskell}{solve} from the last section with an
input value of $10001$, you will notice some delay in returning the result.
This is because of the \mintinline{haskell}{sieve} function, which, according to its name,
is supposed to do some kind of sieving like the famous \emph{Sieve of Eratosthenes}
algorithm does.
But it does not; it's merely a generating function with a running time of $\mathcal{O}(n^2)$,
instead of the promised running time of $\mathcal{O} (n (\lg n))$ of the original sieve.

\section{Prime number theorem}

The \emph{prime number theorem} states that the prime counting function $\pi(n)$ (the
number of primes not greater than n) is asymptotic to $n / \log n$, which can be
denoted as
$$\pi(n) \sim \frac{n}{\log n}.$$
Setting $\pi(n) = 10,000$ leads to $n \sim 10,000 \log n$.

Since $\log 110,000 \approx 11$, we can assume that $n \approx 10,000 * 11 \approx 110,000$,
and the above similarity still holds:
$$10,000 \approx \frac{110,000}{\log{110,000}}.$$