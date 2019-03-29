{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Spec.Chain.STS.Rule.Pbft where

import Control.Lens ((^.))
import Data.Sequence (Seq)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Control.State.Transition

import Ledger.Core
import Ledger.Delegation (DIState)
import Ledger.Signatures
import Ledger.Update

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Rule.SigCnt

data PBFT

data PBFTState = MkPBFTState
  { h   :: Hash
  , sgs :: Seq VKeyGenesis
  }

instance STS PBFT where
  type Environment PBFT =
    ( PParams
    , Map VKeyGenesis VKey
    , Slot
    , Slot
    )

  type State PBFT = PBFTState

  type Signal PBFT = BlockHeader

  data PredicateFailure PBFT
    = SlotNotAfterLastBlock Slot Slot
    | SlotInTheFuture Slot Slot
    | PrevHashNotMaching Hash Hash
    | InvalidHeaderSignature VKey (Sig VKey)
    | SigCountFailure (PredicateFailure SIGCNT)
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ((pps, ds, sLast, sNow), st, bh) <- judgmentContext
        let
          vkd = bh ^. bhIssuer
          s = bh ^. bhSlot
        s > sLast ?! SlotNotAfterLastBlock s sLast
        s <= sNow ?! SlotInTheFuture s sNow
        (bh ^. bhPrevHash) == (h st) ?! PrevHashNotMaching (bh ^. bhPrevHash) (h st)
        verify vkd (bhToSign bh) (bh ^. bhSig) ?! InvalidHeaderSignature vkd (bh ^. bhSig)
        sgs' <- trans @SIGCNT $ TRC ((pps, ds), (sgs st), vkd)
        return $! sgs'
    ]

instance Embed SIGCNT PBFT where
  wrapFailed = SigCountFailure
