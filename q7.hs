{-

  10001st prime
  https://projecteuler.net/problem=7

  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
  What is the 10 001st prime number?

  最初の6つの素数：2,3,5,7,11,13を列挙すると、6番目の素数は13であることが分かります。
  10番目の素数は何ですか？

-}

-- fib = 0 : 1 : zipWith (+) fib (tail fib)
-- primes = 2:3:[n | n <- [5,7..], all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p * p <= n) primes]
-- primes = 2:3:[n | n <- [5,7..], all (\p -> n `mod` p /= 0) $ takeWhile (\x -> x^2 <= n) primes]
--
import Data.List

mod6 = mapAccumL (\acc x -> (acc + x, acc + x)) 1 (cycle[4,2])
(a1, a2) = mod6
primes = 2:3:[n | n <- [5,7..], all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p * p <= n) primes]

main = print $ primes !! 10000
