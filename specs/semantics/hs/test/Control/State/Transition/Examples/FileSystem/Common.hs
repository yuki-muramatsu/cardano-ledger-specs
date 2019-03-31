-- | Common definitions used by the STS and SUT.

module Control.State.Transition.Examples.FileSystem.Common where

newtype Dir = Dir [String]
  deriving (Eq, Ord, Show)

data File
  = File
  { directory :: Dir
  , name :: String
  }
  deriving (Eq, Ord, Show)
