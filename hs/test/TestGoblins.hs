{-# LANGUAGE TypeApplications #-}
import Control.State.Transition.Goblin.BreedingPit (breedStsGoblins)
import Control.State.Transition.Goblin.Explainer
import Control.State.Transition.Goblin
import Data.TreeDiff.Pretty
import           Generator
import LedgerState
import Slot
import qualified Data.TypeRepMap as TM
import Control.Lens (_1, _3, view)
import Text.PrettyPrint (render)

main :: IO ()
main = do
    pop <- breedStsGoblins jcGen (UTXOFailure [InsuffientWitnesses])
    let (Just diff) = explainGoblinGen @UTXOW (fmap (view _3) jcGen) (spawnGoblin (view _1 $ head pop) TM.empty)
    putStrLn $ render $ prettyEditExpr diff
  where
    jcGen = do
      (_, steps, _, sig, ls) <- genValidStateTx
      return (Slot steps, ls, sig)
