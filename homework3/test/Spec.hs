import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Golf

main :: IO ()
main = defaultMainWithOpts
       [ testCase "test1" test1
       , testCase "test2" test2
       , testCase "test3" test3
       , testCase "test4" test4
       , testCase "test5" test5
       , testCase "test6" test6
       , testCase "test7" test7
       , testCase "test8" test8
       , testCase "test9" test9
       ] mempty

-- Exercise 1
test1 :: Assertion
test1 = skips "ABCD" @?= ["ABCD", "BD", "C", "D"]

test2 :: Assertion
test2 = skips "hello!" @?= ["hello!", "el!", "l!", "l", "o", "!"]

test3 :: Assertion
test3 = skips [1] @?= [[1]]

test4 :: Assertion
test4 = skips [True,False] @?=  [[True,False], [False]]

emptyList :: [Int]
emptyList = []

doubleEmptyList :: [[Int]]
doubleEmptyList = []

test5 :: Assertion
test5 =
  let e = []::[Int]
      ee = []::[[Int]]
  in skips e @?= ee

  -- Exercise 2
test6 :: Assertion
test6 = localMaxima [2,9,5,6,1] @?= [9,6]

test7 :: Assertion
test7 = localMaxima [2,3,4,1,5] @?= [4]

test8 :: Assertion
test8 = localMaxima [1,2,3,4,5] @?= []

-- Exercise 3
test9 :: Assertion
test9 = histogram [3, 5] @?= "   * *    \n==========\n0123456789\n"
