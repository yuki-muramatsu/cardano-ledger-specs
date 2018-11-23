import Control.State.Transition.Goblin.BreedingPit (breedStsGoblins)
import           Generator
import LedgerState
import Slot

main :: IO ()
main = breedStsGoblins jcGen (UTXOFailure [IncreasedTotalBalance])
  where
    jcGen = do
      (_, steps, _, sig, ls) <- genValidStateTx
      return (Slot steps, ls, sig)
