\chapter{Problem 003: Largest prime factor}
The prime factors of 13195 are 5, 7, 13 and 29.

\textbf{What is the largest prime factor of the number 600851475143?}

\section{Imperative Approach}

\begin{code}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Problem003 where
import Test.QuickCheck ( (==>), quickCheck, Property )
\end{code}

As we have already seen, it is sometimes convenient to write code inspired by imperative programming.

\begin{code}
lpfImp :: Integer -> Integer
lpfImp number
    | num == 1  = last
    | otherwise = num
    where
        (n,l,f) = factorize (number, 1, 2)
        (num, last, fact) = largest (n,l,3)

factorize :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
factorize (n,l,f) | f `divides` n = factorize (n `div` f, f, f)
                  | otherwise     = (n,l,f)

largest :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
largest (n,l,f) | n > 1 && f^2 <= n = largest (num, last, fact+2)
                | otherwise        = (n,l,f)
                where (num, last, fact) = factorize (n,l,f)

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0
\end{code}

Although this definition is not very easy to understand, it is nonetheless worthwile to explore.
In particular, there is no need of explicit primality checking, because every new found factor must be prime, as all lower factors have already been removed by earlyer calls to \mintinline{haskell}{factorize}.
Hence, this solution is quite fast, even in a functional setting.

\section{Functional Approach}

\begin{code}
lpfFun :: Integer -> Integer
lpfFun n = maximum (listPF n 0)

listPF :: Integer -> Int -> [Integer]
listPF 0 _ = []
listPF n i
    | p*p > n = [n]
    | p `divides` n = p:listPF (n `div` p) 0
    | otherwise = listPF n (i+1)
    where p = primes!!i

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:ns) = p:sieve [n | n <- ns, rem n p /= 0]
\end{code}

With this approach, we generate an infininte but lazy evaluated list of prime numbers, from which we take the potential prime factors.
The factors are evaluated (if they divide the given number) until their square exceeds the number, no matter how big the number is.
Therfore the list will not be evaluated to infinity.
For primes lower than 10,000 this is reasonable fast, but not comparable to the imperative approach.
On the other hand, the code is quite compact and (in my opinion) easy to understand.
This is one one of the main advantages of the functional paradigm.

\section{Testing}

\begin{code}
lpfImpDevidesN :: Integer -> Property
lpfImpDevidesN n = n > 1 ==> lpfImp n `divides` n

lpfFunDevidesN :: Integer -> Property
lpfFunDevidesN n = n > 1 ==> lpfFun n `divides` n

equalsImpFun :: Integer -> Property
equalsImpFun n = n > 1 ==> lpfFun n == lpfImp n

main :: IO ()
main = do
    quickCheck lpfImpDevidesN
    quickCheck lpfFunDevidesN
    quickCheck equalsImpFun
\end{code}