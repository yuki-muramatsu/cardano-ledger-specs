{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Spec.Chain.STS.Rule.Chain where

import Control.Lens ((^.), _2) --, _1, _5, Lens')
import qualified Crypto.Hash
import Data.Bits (shift)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
import Numeric.Natural (Natural)

import Control.State.Transition
-- import Control.State.Transition.Generator
import Ledger.Core
-- import Ledger.Core.Generator
import Ledger.Delegation
import Ledger.Update
import Ledger.UTxO (UTxO, TxId)

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Rule.BHead
import Cardano.Spec.Chain.STS.Rule.BBody
import Cardano.Spec.Chain.STS.Rule.Pbft


data BlockType = Main | EBB deriving (Eq, Show)


data CHAIN

instance STS CHAIN where
  type Environment CHAIN = Slot            -- Current slot

  type State CHAIN =
    ( Slot
    , Map Epoch SlotCount
    , Seq VKeyGenesis
    , Hash
    , UTxO TxId
    , UPIState
    , DIState
    )

  type Signal CHAIN = Block

  data PredicateFailure CHAIN
    = BHeadFailure (PredicateFailure BHEAD)
    | BBodyFailure (PredicateFailure BBODY)
    | PBFTFailure (PredicateFailure PBFT)
    | EBBCheckFailure BlockType
    | MaximumBlockSize Natural Natural
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [ isEBBRule, notEBBRule ] where
    isEBBRule :: TransitionRule CHAIN
    isEBBRule = do
      TRC (_sNow, (sLast, elens, sgs, _, utxo, us, ds), b) <- judgmentContext
      bIsEBB b ?! EBBCheckFailure Main -- this is not an EBB
      bSize b <= (2 `shift` 21) ?! MaximumBlockSize (bSize b) (2 `shift` 21)
      let h' = bhHash (b ^. bHeader)
      return $! (sLast, elens, sgs, h', utxo, us, ds)

    notEBBRule :: TransitionRule CHAIN
    notEBBRule = do
      TRC (sNow, (sLast, elens, sgs, h, utxo, us, ds), b) <- judgmentContext
      not (bIsEBB b) ?! EBBCheckFailure EBB -- this is an EBB

      (us', elens') <- trans @BHEAD $ TRC (sLast, (us, elens), b ^. bHeader)
      let ppsUs' = snd (us' ^. _2)
      let dm = _dIStateDelegationMap ds :: Map VKeyGenesis VKey
      (h', sgs') <- trans @PBFT $ TRC ((ppsUs', dm, sLast, sNow), (h, sgs), b ^. bHeader)
      -- TODO(md): make a transition with the BBODY STS
      return $! (b ^. bHeader ^. bhSlot, elens', sgs', h', utxo, us, ds)

    -- [ do
    --     TRC
    --       ( (sNow, gks, _)
    --       , (eLast, sLast, hLast, sgs, ds, us)
    --       , b ) <- judgmentContext

    --     (eNext, sNext, h', sgs', us') <- trans @BHEAD $ TRC ( (sNow, ds ^. dms)
    --                                                         , (eLast, sLast, hLast, sgs, us)
    --                                                         , b ^. bHeader )

    --     ds' <- trans @BBODY $ TRC ( (eNext, sNext, us, gks)
    --                               , ds
    --                               , b )

    --     return $! (eNext, sNext, h', sgs', ds', us')
    -- ]

-- instance Embed DELEG CHAIN where
--   wrapFailed = LedgerFailure

instance Embed BHEAD CHAIN where
  wrapFailed = BHeadFailure

instance Embed BBODY CHAIN where
  wrapFailed = BBodyFailure

instance Embed PBFT CHAIN where
  wrapFailed = PBFTFailure

genesisHash :: Hash
-- Not sure we need a concrete hash in the specs ...
genesisHash = Crypto.Hash.hash ("" :: ByteString)

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- instance HasTrace CHAIN where
--   initEnvGen = undefined
    -- do
    -- -- In mainet the maximum header size is set to 2000000 and the maximum
    -- -- block size is also set to 2000000, so we have to make sure we cover
    -- -- those values here. The upper bound is arbitrary though.
    -- mHSz <- Gen.integral (Range.constant 0 4000000)
    -- mBSz <- Gen.integral (Range.constant 0 4000000)

    -- mTxSz <- Gen.integral (Range.constant 0 4000000)
    -- mPSz <- Gen.integral (Range.constant 0 4000000)

    -- -- Chain stability. We set this to an integer between 1 and twice the value
    -- -- chosen for the Byron release.
    -- k <- Gen.integral (Range.constant 1 (2160 * 2))

    -- -- The percentage of the slots will typically be between 1/5 and 1/4,
    -- -- however we want to stretch that range a bit for testing purposes.
    -- t <- pure (1/5) -- Gen.double (Range.constant (1/6) (1/3))
    -- -- TODO: we make this a constant till we solve the problem with the number
    -- -- of byzantineNodes being a constant in the implementation.

    -- -- The number of slots per epoch is computed from 'k':
    -- -- slots per-epoch = k * 10
    -- spe <- pure $! SlotCount $ fromIntegral $ k * 10
    -- -- Update TTL
    -- uttl <- SlotCount <$> Gen.integral (Range.linear 1 100)
    -- -- Confirmation threshold
    -- ct <- Gen.integral (Range.linear 1 7)
    -- -- Update adoption threshold
    -- uat <- Gen.integral (Range.linear 1 7)
    -- -- let initPPs
    -- --       = PParams
    -- --       { _maxHdrSz = mHSz
    -- --       , _maxBkSz = mBSz
    -- --       , _maxTxSz = mTxSz
    -- --       , _maxPropSz = mPSz
    -- --       , _bkSgnCntT = t
    -- --       , _bkSlotsPerEpoch = spe
    -- --       , _upTtl = uttl
    -- --       , _scriptVersion = 1
    -- --       , _cfmThd = ct
    -- --       , _upAdptThd = uat
    -- --       , _stableAfter = BlockCount k
    -- --       }
    -- initGKeys <- Gen.set (Range.constant 1 30) vkgenesisGen
    -- -- If we want to generate large traces, we need to set up the value of the
    -- -- "clock-slot" to a sufficiently large value.
    -- clockSlot <- Slot <$>
    --   Gen.integral (Range.constant 32768 2147483648)
    -- return clockSlot --, initGKeys, initPPs)

  -- sigGen _ (e, Slot s, h, _sgs, ds, us) = do
  --   -- We'd expect the slot increment to be close to 1, even for large Gen's
  --   -- size numbers.
  --   slotInc <- Gen.integral (Range.exponential 0 10)
  --   -- Get some random issuer from the delegates of the delegation map.
  --   vkI <- Gen.element $ Map.elems (ds ^. dms)
  --   let dsEnv
  --         = DSEnv
  --         { _dSEnvAllowedDelegators = undefined
  --         , _dSEnvEpoch = e
  --         , _dSEnvSlot = Slot s
  --         , _dSEnvStableAfter = us ^. Ledger.Update.stableAfter }
  --   dCerts <- dcertsGen dsEnv
  --   let bh
  --         = MkBlockHeader
  --         { _bhPrevHash = h
  --         , _bhSlot = Slot (s + slotInc)
  --         , _bhIssuer = vkI
  --         , _bhSig = Sig vkI (owner vkI)
  --         }
  --       bb
  --         = BlockBody
  --         { _bDCerts = dCerts }
  --   return $ Block bh bb
