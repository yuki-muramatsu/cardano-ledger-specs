import Control.Monad (void)

import qualified Control.State.Transition.Examples.FileSystem.Model as Filesystem


main :: IO ()
main = void $ Filesystem.tests
