{-

  Sum square difference
  https://projecteuler.net/problem=6

  The sum of the squares of the first ten natural numbers is,
  1² + 2² + ... + 10² = 385
  The square of the sum of the first ten natural numbers is,
  (1 + 2 + ... + 10)² = 552 = 3025
  Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
  Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

  最初の10個の自然数の平方和は、
  1² + 2² + ... + 10² = 385
  最初の10個の自然数の和の2乗は、
  （1 + 2 + ... + 10）² = 55² = 3025
  したがって、最初の10個の自然数の平方和と合計の2乗の差は3025 - 385 = 2640です。
  最初の100個の自然数の平方和と和の平方和の差を求めます。

-}

sumOfsquare n = sum [x**2 | x <- [1..n]]
squareOfSum n = (sum [1..n]) ** 2

answer n = round $ (squareOfSum n) - (sumOfsquare n)

main = print $ answer 100
