{-

  https://projecteuler.net/problem=4

  A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
  Find the largest palindrome made from the product of two 3-digit numbers.

  パリンドロームの数字は同じように両方を読みます。2つの2桁の数字の積から作られる最大の回文は、9009 = 91×99です。
  2つの3桁の数字の積から作られた最大のパリンドロームを探します。

-}

import Data.Char

isEvenDigit :: Int -> Bool
isEvenDigit = even . length . show

isPalindrome :: Int -> Bool
isPalindrome n = (isEvenDigit n) && ((\(x,y) -> x == (reverse y)) $ splitAt ((length (show n)) `div` 2) (show n))

maxDigits :: Int -> Int
maxDigits n = read $ (take n (repeat '9')) :: Int

minDigits :: Int -> Int
minDigits n = read $ '1' : (take (n-1) (repeat '0')) :: Int

minmax min max = (min, max)

digit n = minmax (minDigits n) (maxDigits n)

reverseListWithRange (min, max)
  | min == max = [min]
  | otherwise = max : reverseListWithRange (min, (max-1))

reverseListDigitsOf = reverseListWithRange . digit

productList [] = []
productList all@(x:xs) = map (*x) all : productList xs

maxPalindromeInListDigitOf n = productList $ reverseListDigitsOf n

answer = (maxPalindromeInListDigitOf 9) !! 0 !! 0

main = print answer

-- productList [] = []
-- productList all@(x:xs) = map (*x) all : productList xs
-- reverseListDigits n = [(minDigits n)..(maxDigits n)]

-- 1  2  3  4  5  6
-- 2  4  6  8 10 12
-- 3  /  9 12 15 18
-- 4  /  / 16 20 24
-- 5  /  /  / 25 30
-- 6  /  /  /  / 36

