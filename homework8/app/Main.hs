module Main where

import Lib
import System.Environment

main :: IO ()
main = getArgs
    >>= readFile . head
    >>= putStrLn . parse . maxFun . read
