{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

-- | Simple example of a transition system whose states contain the sum of the
-- integers seen in the signals.
--
module Control.State.Transition.Examples.Sum where

import Hedgehog (Property, forAll, property, withTests, assert)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.State.Transition
import Control.State.Transition.Generator
import Control.State.Transition.Trace


data SUM

instance STS SUM where

  type Environment SUM = ()

  type State SUM = Int

  type Signal SUM = [Int]

  data PredicateFailure SUM = NoFailure deriving (Eq, Show)

  initialRules = [pure 0]

  transitionRules =
    [ do
        TRC ((), st, xs) <- judgmentContext
        return $! st + sum xs
    ]

instance HasTrace SUM where
  initEnvGen = pure ()

  sigGen _ _ =
    Gen.list (Range.constant 1 10) (Gen.integral (Range.constant (-3) 3))

-- | Which counterexamples do we get if we write a property on traces that
-- always fail?
prop_Bounded :: Property
prop_Bounded = withTests 1000 $ property $ do
  tr <- forAll (trace @SUM)
  assert (lastState tr < 10)
