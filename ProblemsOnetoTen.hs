module ProblemsOnetoTen where

import Data.List (union,(\\))

multiple3or5 :: [Int]
multiple3or5 = ([i | i <- [0 .. 1000], i `mod` 3 == 0] `union` [i |i <- [0 .. 1000], i `mod` 5 == 0])


isPrime :: Integer -> Bool
isPrime x = null $ filter (\i -> x `mod` i == 0) [2 .. (x - 1)]

allPrimesUpTo :: Integer -> [Integer]
allPrimesUpTo n = [x | x <- [1 .. n], (isPrime x == True)]

maxPrime :: Integer -> Integer
maxPrime n = head $ fastReverse (allPrimesUpTo n)

fastReverse :: [a] -> [a]
fastReverse list = case list of
    [] -> []
    x:xs -> foldl (\list' a -> a:list') [] list
