{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}

-- | A vending machine specified in a compositional way.
--
-- This example shows how transition systems can be composed, using a simple
-- vending machine as running example.
--
module Control.State.Transition.Examples.CompVendingMachine where


import Data.Map (Map)
import qualified Data.Map as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Control.State.Transition
import Control.State.Transition.Trace


--------------------------------------------------------------------------------
-- ACCNT transition system
--------------------------------------------------------------------------------

data ACCNT

data Item = Soda | Chocolate deriving (Show, Eq, Ord)

instance STS ACCNT where

  type Environment ACCNT = Map Item Int

  type State ACCNT = Int

  type Signal ACCNT = Item

  data PredicateFailure ACCNT
    = InsufficientMoney { balance :: Int, itemPrice :: Int }
    | ItemNotFound Item
    deriving (Eq, Show)


  initialRules = []

  transitionRules =
    [ do
        TRC (prices, tokens, item) <- judgmentContext
        case Map.lookup item prices of
          Nothing -> do
            failBecause $ ItemNotFound item -- We might need to extend `Clause`  with a function
            return tokens
          Just price  -> do
            price <= tokens ?! InsufficientMoney tokens price
            return $! tokens - price
    ]

accntExamples :: TestTree
accntExamples
  = testGroup "Accounting"
    [ testCase "Example 0" $ checkTrace @ACCNT [] $
      pure 0
    , testCase "Example 1" $ checkTrace @ACCNT [(Soda, 7)] $
      pure 20 .- Soda .-> 13
    ]
