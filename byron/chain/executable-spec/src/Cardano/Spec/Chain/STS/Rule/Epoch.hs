{-# LANGUAGE TypeFamilies #-}

module Cardano.Spec.Chain.STS.Rule.Epoch where

import Control.Lens ((^.), _2)
import Control.State.Transition
import Data.Map.Strict (Map, insert)
import Ledger.Core
import Ledger.Update

import Cardano.Spec.Chain.STS.Block

data EPOCH

instance STS EPOCH where
  type Environment EPOCH = Map VKeyGenesis VKey
  type State EPOCH =
    ( Map Epoch SlotCount
    , Epoch
    , UPIState
    )
  type Signal EPOCH = Slot
  data PredicateFailure EPOCH =
    NotAfterCurrentEpoch Epoch Epoch deriving (Eq, Show)

  initialRules = []

  -- In the LaTeX counterpart there is a transition rule that does not
  -- change the state at all so it is left out here.
  transitionRules =
    [ do
        TRC (_dms, (elens, e_c, us), s) <- judgmentContext
        let
          e_n = sEpoch s elens
          pps = snd (us ^. _2)
          es = pps ^. bkSlotsPerEpoch
        e_c < e_n ?! NotAfterCurrentEpoch e_c e_n
        -- TODO(md): to be called once the EPOCH STS is fixed
        -- us' <- trans @UPIEC $ TRC ((e_c, s, dms), us, e_n)
        let us' = undefined :: UPIState
        return $! (insert e_n es elens, e_n, us')
    ]
