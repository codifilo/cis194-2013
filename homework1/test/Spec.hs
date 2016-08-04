import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Lib

main :: IO ()
main = defaultMainWithOpts
       [ testCase "toDigits" testToDigits
       , testProperty "toDigitsProperty" toDigitsProperty
       , testProperty "toDigitsEmptyProperty" toDigitsEmptyProperty
       , testCase "doubleEveryOtherTest1" doubleEveryOtherTest1
       , testCase "doubleEveryOtherTest2" doubleEveryOtherTest2
       , testCase "sumDigitsTest1" sumDigitsTest1
       , testCase "validateTestTrue" validateTestTrue
       , testCase "validateTestFalse" validateTestFalse
       , testCase "hanoiTest" hanoiTest
       ] mempty

-- Exercise 1
testNumber = 123456

testToDigits :: Assertion
testToDigits = toDigits testNumber @?= [1, 2, 3, 4, 5, 6]

toDigitsProperty :: Integer -> Bool
toDigitsProperty n = reverse (toDigits n) == toDigitsRev n

toDigitsEmptyProperty :: Integer -> Property
toDigitsEmptyProperty n = n <= 0 ==> null (toDigits n)

-- Exercise 2
doubleEveryOtherTest1 :: Assertion
doubleEveryOtherTest1 = doubleEveryOther [8,7,6,5] @?= [16,7,12,5]

doubleEveryOtherTest2 :: Assertion
doubleEveryOtherTest2 = doubleEveryOther [1,2,3] @?= [1,4,3]

-- Exercise 3
sumDigitsTest1 :: Assertion
sumDigitsTest1 = sumDigits [16,7,12,5] @?= 22

-- Exercise 4
validateTestTrue :: Assertion
validateTestTrue = luhn 4012888888881881 @?= True

validateTestFalse :: Assertion
validateTestFalse = luhn 4012888888881882 @?= False

-- Exercise 5
hanoiTest :: Assertion
hanoiTest = hanoi 2 "a" "b" "c" @?= [("a","c"), ("a","b"), ("c","b")]
