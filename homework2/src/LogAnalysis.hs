{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Text.Read
import Data.Maybe

-- Exercise 1
isNumeric :: String -> Bool
isNumeric s = isJust (readMaybe s :: Maybe Int)

parseMessage :: String -> LogMessage
parseMessage str = parseWordList (words str)

parseWordList :: [String] -> LogMessage
parseWordList ws@("I":ts:msg)
  | isNumeric ts = LogMessage Info (read ts::Int) (unwords msg)
  | otherwise    = Unknown (unwords ws)
parseWordList ws@("W":ts:msg)
  | isNumeric ts = LogMessage Warning (read ts::Int) (unwords msg)
  | otherwise    = Unknown (unwords ws)
parseWordList ws@("E":ec:ts:msg)
  | isNumeric ec && isNumeric ts = LogMessage (Error (read ec::Int)) (read ts::Int) (unwords msg)
  | otherwise                    = Unknown (unwords ws)
parseWordList _ = Unknown ""

parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert msg@LogMessage{} Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ ts1 _) (Node left msg2@(LogMessage _ ts2 _) right)
  | ts1 < ts2 = Node (insert msg1 left) msg2 right
  | otherwise = Node left msg2 (insert msg2 right)
insert _ tree = tree

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . filter (severe 50)

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown msg) = msg

severe :: Int -> LogMessage -> Bool
severe minLvl (LogMessage (Error lvl) _ _) = lvl >= minLvl
severe _ _ = False
