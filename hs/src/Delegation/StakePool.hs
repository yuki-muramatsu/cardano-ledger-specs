{-# LANGUAGE DeriveGeneric #-}
module Delegation.StakePool
  ( StakePool(..)
  , Delegation(..)
  ) where

import           Data.Map        (Map)
import           Data.Ratio
import           Numeric.Natural

import           Coin            (Coin)
import           Keys
import GHC.Generics (Generic)

-- |A stake pool.
data StakePool = StakePool
                   { poolPubKey  :: VKey
                   , poolPledges :: Map VKey Coin -- TODO not updated currently
                   , poolCost    :: Coin
                   , poolMargin  :: Ratio Natural
                   , poolAltAcnt :: Maybe HashKey
                   } deriving (Show, Eq, Generic, Ord)

-- |The delegation of one stake key to another.
data Delegation = Delegation { delegator :: VKey
                             , delegatee :: VKey }
                             deriving (Show, Eq, Generic, Ord)
