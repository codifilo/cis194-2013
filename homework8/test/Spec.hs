import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Lib
import Employee

main :: IO ()
main = defaultMainWithOpts
       [
       ] mempty

-- Exercise 1
