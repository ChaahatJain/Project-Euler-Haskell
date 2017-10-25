module ProblemsOnetoTen where

import Data.List

multiple3or5 :: Int
multiple3or5 = sum ([i | i <- [0 .. 1000], i `mod` 3 == 0] `union` [i |i <- [0 .. 999], i `mod` 5 == 0])
-------------------------
fibList :: (Int,Int) -> [Int]
fibList (x,y) = x : fibList (y,x+y)

getList (x,y) = sum (filter (even )(takeWhile (< 4000000) (fibList (x,y))))

-------------------------
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
------------------------
isPalindrome :: Int -> Bool
isPalindrome int = list == reverse list
    where
    list = show int

palList :: Int
palList =  head $ fastReverse $ sort [a*b | a <- [999,998..100], b <- [999,998..100], (isPalindrome (a*b))]
------------------------
minDiv20 :: Int
minDiv20 = foldl lcm 1 [1..20]
-----------------------
sumSquareDiff :: Int -> Int
sumSquareDiff n = sumSquare n - squareSum n

sumSquare :: Int -> Int
sumSquare n = sum [a^2 | a <- [1..n]]

squareSum :: Int -> Int
squareSum n = (sum [a | a <- [1..n]])^2
-----------------------
list = head $[(a,b,c) | a<-[100..500],b<-[100..500],c <- [100..500], a+b+c ==1000,a^2 + b^2 == c^2]