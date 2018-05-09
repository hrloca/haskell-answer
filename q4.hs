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

productLists :: Int -> [Int]
productLists n = concat [[y*x | y <- [1..(maxDigits n)]] | x <- [1..(maxDigits n)]]

maxPalindromeNumbers :: Int -> Int
maxPalindromeNumbers n = maximum $ filter isPalindrome (productLists n)

answer = maxPalindromeNumbers 3

main = print $ answer
