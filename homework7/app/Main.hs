module Main where

import JoinList
import Buffer
import Scrabble
import Sized
import Editor

main :: IO ()
main = let asJoinList s = fromString s :: JoinList (Score, Size) String
       in runEditor editor . asJoinList $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
