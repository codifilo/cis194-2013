import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Lib
import ExprT

main :: IO ()
main = defaultMainWithOpts
       [ testCase "testEval" testEval
       , testCase "testEvalStr" testEvalStr
       ] mempty

-- Exercise 1
testEval :: Assertion
testEval = eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) @?= 20

testEvalStr :: Assertion
testEvalStr = evalStr "(2+3)*4" @?= Just 20

testEvalStr2 :: Assertion
testEvalStr2 = evalStr "(2+r3)*4" @?= Nothing
