{-

  Special Pythagorean triplet
  https://projecteuler.net/problem=9

  A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

  a² + b² = c²
  For example, 3² + 4² = 9 + 16 = 25 = 5².

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.

  ピタゴラスの三つ組は、3つの自然数の集合であり、a < b < cであり、

  a² + b² = c²
  例えば、 3² + 4² = 9 + 16 = 25 = 5².

  厳密に1つのピタゴラスの三つ組が存在し、a + b + c = 1000である。
  製品abcを見つける。

-}
import Data.List

-- https://ja.wikipedia.org/wiki/%E3%83%94%E3%82%BF%E3%82%B4%E3%83%A9%E3%82%B9%E3%81%AE%E5%AE%9A%E7%90%86
pythagoras = [(m^2-n^2, 2*m*n, m^2+n^2)| m <- [1..], n <- [1..m], odd (m-n), m>n]

ans = productVector3 $ filter (compareSumAnd 1000) pythagoras !! 0
        where
          productVector3 (a,b,c) = a*b*c
          sumVector3 (a,b,c) = a+b+c
          compareSumAnd n vector3 = sumVector3 vector3 == n


main = print ans
