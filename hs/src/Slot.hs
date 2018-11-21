{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module Slot
  ( Slot(..)
  , Epoch(..)
  ) where

import           Data.Monoid             (Sum(..))
import           Numeric.Natural         (Natural)
import GHC.Generics (Generic)

-- |A Slot
newtype Slot = Slot Natural
  deriving (Show, Eq, Generic, Ord)
  deriving (Semigroup, Monoid) via (Sum Natural)

-- |An Epoch
newtype Epoch = Epoch Natural
  deriving (Show, Eq, Generic, Ord)
  deriving (Semigroup, Monoid) via (Sum Natural)

