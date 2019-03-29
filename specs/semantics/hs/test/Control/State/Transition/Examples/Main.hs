import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests
 where
  tests :: TestTree
  tests = testGroup "Examples"
    [ ]
