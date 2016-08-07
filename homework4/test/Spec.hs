import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Lib

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "fun1Property" fun1Property
       --, testProperty "fun2Property" fun2Property
       , testCase "xorTest1" (xor [False, True, False] @?= True)
       , testCase "xorTest1" (xor [False, True, False, True] @?= False)
       , testCase "xorTest1" (xor [True, True, True] @?= True)
       ] mempty

fun1Property :: [Integer] -> Bool
fun1Property xs = fun1 xs == fun1' xs

fun2Property :: Integer -> Bool
fun2Property x = fun2 x == fun2' x
