{-# LANGUAGE FlexibleInstances #-} -- Exercise 4
{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized
import Scrabble
import Buffer

-- Exercise 1
data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (mappend (tag x) (tag y)) x y

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

instance Monoid m => Monoid (JoinList m a) where
  mempty  = Empty
  mappend = (+++)

-- Exercise 2
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJSlow :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJSlow i jl = jlToList jl !!? i

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Single _ a)
  | i == 0    = Just a
  | otherwise = Nothing
indexJ i (Append m jl1 jl2)
  | i < 0 || i > size0 = Nothing
  | i < size1 = indexJ i jl1
  | otherwise = indexJ (i-size1) jl2
  where size0 = getSize . size $ m
        size1 = getSize . size. tag $ jl1
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i jl | i <= 0 = jl
dropJ _ (Single _ _) = Empty
dropJ i (Append m jl1 jl2)
  | i >= size0 = Empty
  | i < size1 = dropJ i jl1 +++ jl2
  | otherwise = dropJ (i - size1) jl2
  where size0 = getSize . size $ m
        size1 = getSize . size. tag $ jl1
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0      = Empty
takeJ _ Empty           = Empty
takeJ _ jl@(Single _ _) = jl
takeJ n jl@(Append m jl1 jl2)
  | n >= size0 = jl
  | n <= size1 = takeJ n jl1
  | otherwise  = jl1 +++ takeJ (n - size1) jl2
  where size0 = getSize . size $ m
        size1 = getSize . size. tag $ jl1

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = mconcat . fmap scoreWord $ words s

scoreWord :: String -> JoinList Score String
scoreWord "" = Empty
scoreWord w = Single (scoreString w) w

scoreAndSizeWord :: String -> JoinList (Score, Size) String
scoreAndSizeWord "" = Empty
scoreAndSizeWord w = Single (scoreString w, Size 1) w

-- Exercise 4
scoreAndSize :: String -> JoinList (Score, Size) String
scoreAndSize s = mconcat . fmap scoreAndSizeWord $ words s


instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ jl1 jl2) = toString jl1 ++ toString jl2

  fromString = scoreAndSize

  line = indexJ

  replaceLine i s l = takeJ i l +++ fromString s +++ dropJ (i + 1) l

  numLines = getSize . size . tag

  value = getScore . fst . tag
