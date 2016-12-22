{-# OPTIONS_GHC -Wall #-}
module Lib where

import Data.List

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
 | even n = n + fun2 (n `div` 2)
 | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum .
        filter even .
        takeWhile (>1) .
        iterate (\x -> if even x then x `div` 2 else 3 * x + 1)


-- Exercise 2
data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldl append Leaf

append :: Tree a -> a -> Tree a
append Leaf a = Node 0 Leaf a Leaf
append (Node i left x right) a
  | depth left < depth right = Node i (append left a) x right
  | otherwise                = let newRight = append right a in
                                   Node (depth newRight + 1) left x newRight

depth :: Tree a -> Integer
depth (Node i _ _ _) = i
depth _              = -1

-- Exercise 3
xor :: [Bool] -> Bool
xor xs  = (length . filter (== True)) xs `mod` 2 /= 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a acc -> f a : acc) ([]::[b])

-- Exercise 4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let m = n `div`2 - 1 in
                      map ((+1) . (*2)) $ [1..m] \\ sieve m

sieve :: Integer -> [Integer]
sieve n = takeWhile (<=n)
                $ map (\(i, j) -> i+j+2*i*j)
                $ filter (uncurry (<=))
                $ cartProd [1..] [1..]

isPrime :: Integer -> Bool
isPrime n = go 2
  where
    go d
      | d*d > n        = True
      | n `rem` d == 0 = False
      | otherwise      = go (d+1)

primes :: Integer -> [Integer]
primes n = takeWhile (<=n) $ filter isPrime [2 .. ]
