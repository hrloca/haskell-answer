{-

  https://projecteuler.net/problem=5

  2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
  What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

  2520は、1から10までの各数値で余りなく分けることができる最小の数です。
  1から20までのすべての数字で均等に割り切れる最小の正の数値は何ですか？

-}


ans = foldr1 (lcm) [1..20]

main = print ans
