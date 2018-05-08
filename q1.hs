{-

  https://projecteuler.net/problem=1

  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.

-}

multiples :: Integer -> Integer -> Bool
multiples n x = 0 == mod n x

anyMultiples :: [Integer] -> Integer -> Bool
anyMultiples xs n = any (multiples n) xs

by3or5Multiples = anyMultiples [3,5]

answer :: Integer -> Integer
answer n = sum (filter by3or5Multiples [1..(n-1)])

main = print $ answer 1000

