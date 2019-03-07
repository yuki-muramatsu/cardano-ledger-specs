
import Test.Tasty (TestTree, defaultMain, testGroup, localOption)
import Test.Tasty.Hedgehog (testProperty)

import Control.State.Transition.Examples.Sum (prop_Bounded)

main :: IO ()
main = defaultMain tests
  where
    tests = testGroup "Sum" [testProperty "False" prop_Bounded]
