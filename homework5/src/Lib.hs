{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Lib where

import ExprT
import Parser
import StackVM

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit i)  = i
eval (ExprT.Add e1 e2) = eval e1 + eval e2
eval (ExprT.Mul e1 e2) = eval e1 * eval e2

-- Exercise 2
parseStr :: String -> Maybe ExprT
parseStr = parseExp Lit ExprT.Add ExprT.Mul

evalStr :: String -> Maybe Integer
evalStr s = fmap eval (parseStr s)

-- Exercise 3
class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

instance Expr ExprT where
  lit = Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
instance Expr Integer where
  lit i = i
  add i1 i2 = i1 + i2
  mul i1 i2 = i1 * i2

instance Expr Bool where
  lit i
    | i > 0     = True
    | otherwise = False
  add e1 e2 = e1 || e2
  mul e1 e2 = e1 && e2

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax e1) (MinMax e2) = MinMax $ max e1 e2
  mul (MinMax e1) (MinMax e2) = MinMax $ min e1 e2

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 e1) (Mod7 e2) = Mod7 $ (e1 + e2) `mod` 7
  mul (Mod7 e1) (Mod7 e2) = Mod7 $ (e1 * e2) `mod` 7

-- Tests
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

-- Exercise 5
instance Expr StackVM.Program where
  lit x   = [StackVM.PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile s = case (parseExp lit add mul s) of
  Nothing -> Nothing
  (Just program) -> Just program

testCompile :: String -> Maybe (Either String StackVM.StackVal)
testCompile s = case (compile s) of
  Nothing -> Nothing
  (Just program) -> Just (StackVM.stackVM program)
