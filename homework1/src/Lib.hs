{-# OPTIONS_GHC -Wall #-}
module Lib where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Exercise 2 -----------------------------------------

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (dropLastDigit n) ++ [lastDigit n]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = lastDigit n : toDigitsRev (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherFromLeft (reverse xs))

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft (x:y:xs) = x : 2 * y : doubleEveryOtherFromLeft xs
doubleEveryOtherFromLeft xs = xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sum . toDigits) 0


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = (==0) . lastDigit . sumDigits . doubleEveryOther . toDigits

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n p1 p2 p3 = hanoi (n-1) p1 p3 p2 ++ [(p1, p2)] ++ hanoi (n-1) p3 p2 p1
