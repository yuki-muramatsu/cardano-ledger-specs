
import Test.Tasty (TestTree, defaultMain, testGroup)

import Control.State.Transition.Examples.CompVendingMachine

main :: IO ()
main = defaultMain tests
 where
  tests :: TestTree
  tests = testGroup "Examples"
    [ accntExamples ]
