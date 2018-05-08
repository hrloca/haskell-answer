import Data.Typeable


-- [ add ] Original

add' :: Int -> Int -> Int
add' x y = x + y



-- [ sum ] Re-implementation

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = add' x (sum' xs)



-- [ reverse ] Re-implementation

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]



-- [ repeat ] Re-implementation

repeat' :: a -> [a]
repeat' x = x : repeat' x



-- [ map ] Re-implementation

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs



-- [ take ] Re-implementation

take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ []  = []
take' n (x:xs) = x : take' (n-1) xs



-- [ replicate ] Re-implementation

replicate' :: Int -> a -> [a]
replicate' n x 
  | n <= 0 = []
  | otherwise = x : replicate' (n-1) x



-- [ elem ] Re-implementation

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise =  elem' a xs



-- [ filter ] Re-implementation
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise =  filter' p xs


------------------------------------------------------------

chanin :: Integer -> [Integer]
chanin 1 = [1]
chanin n
 | even n = n : chanin (n `div` 2)
 | odd n = n : chanin (n * 3 + 1)

------------------------------------------------------------

-- isMultiplesOf :: Integer -> Integer -> Bool
-- isMultiplesOf n x = 0 == mod n x
-- 
-- isAnyMultiples :: [Integer] -> Integer -> Bool
-- isAnyMultiples [] _ = False
-- isAnyMultiples (x:xs) n =  n `isMultiplesOf` x || isAnyMultiples xs n
-- 
-- by3or5Multiples = isAnyMultiples [3,5]
-- 
-- answer :: Integer -> Integer
-- answer n = sum (filter by3or5Multiples [1..(n-1)])

fib = 0 : 1 : zipWith (+) fib (tail fib)

filterByEven = filter even
filterByNotOver4Mil = filter (\n -> n < 4000000)

main = print $ filterByNotOver4Mil fib
