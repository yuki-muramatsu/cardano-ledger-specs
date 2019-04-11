{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Spec.Chain.STS.Rule.BBody where

import Control.Lens ((^.))
import Data.Set (Set)

import Control.State.Transition
  ( Embed
  , Environment
  , PredicateFailure
  , STS
  , Signal
  , State
  , TRC(TRC)
  , (?!)
  , initialRules
  , judgmentContext
  , trans
  , transitionRules
  , wrapFailed
  )
import Ledger.Core
  (Epoch, Slot, VKeyGenesis)
import Ledger.Delegation
  ( DELEG
  , DIState
  , DSEnv(DSEnv)
  , _dSEnvAllowedDelegators
  , _dSEnvEpoch
  , _dSEnvSlot
  , _dSEnvStableAfter
  )
import Ledger.Update (PParams, maxBkSz, stableAfter, UPIState)
import Ledger.UTxO (UTxO, TxId)

import Cardano.Spec.Chain.STS.Block

data BBODY

instance STS BBODY where
  type Environment BBODY =
    ( PParams
    , Epoch
    )
--     = ( Epoch
--       , Slot
--       , PParams
--       , Set VKeyGenesis
--       )
-- -
  type State BBODY =
    ( UTxO TxId
    , UPIState
    , DIState
    )

    -- DIState

  type Signal BBODY = Block

  data PredicateFailure BBODY
    = InvalidBlockSize
    | DelegationFailure (PredicateFailure DELEG)
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ((pps, e_n), (utxo, us, ds), b) <- judgmentContext
        let bMax = pps ^. maxBkSz
        bSize b <= bMax ?! InvalidBlockSize
        let
          bh = b ^. bHeader
          vkd = bh ^. bhIssuer
        -- TODO(md): three verify calls have to be performed here
        return $! undefined
    ]
    -- [ do
    --     TRC ((e, s, pps, gks), ds, b) <- judgmentContext
    --     bSize b <= pps ^. maxBkSz ?! InvalidBlockSize
    --     let diEnv
    --           = DSEnv
    --           { _dSEnvAllowedDelegators = gks
    --           , _dSEnvEpoch = e
    --           , _dSEnvSlot = s
    --           , _dSEnvStableAfter = pps ^. stableAfter
    --           }
    --     ds' <- trans @DELEG
    --                  $ TRC (diEnv, ds, b ^. bBody . bDCerts)
    --     return $! ds'
    -- ]

instance Embed DELEG BBODY where
  wrapFailed = DelegationFailure
