{-# LANGUAGE NamedFieldPuns #-}

-- | System under test: A pure file-system mock.
--
-- The implementation of the system under test will mimic closely the
-- corresponding state transition system. The sole purpose of this mock is to
-- test how good shrinking we can get with hedgehog.
module Control.State.Transition.Examples.FileSystem.SUT where

import Control.Monad (when)
import Data.Set (Set)
import qualified Data.Set as Set

import Control.State.Transition.Examples.FileSystem.Common

data Error
  = SomeError
  | DirectoryExists Dir State
  deriving (Show)

data State
  = State
  { dirs :: Set Dir
  -- ^ Directories known to the system.
  , opened :: Set File
  -- ^ Open files.
  }
  deriving (Show)

initSt :: State
initSt = State (Set.singleton $ Dir []) Set.empty

mkdir :: State -> Dir -> Either Error State
mkdir st@State{dirs} d
  | d `Set.member` dirs = Left $ DirectoryExists d st
  | otherwise           = Right $ st { dirs = Set.insert d dirs }

open :: State -> File -> Either Error State
open st@State{ dirs, opened } f@(File d _) = do
  when (f `Set.member` opened) $ Left SomeError
  when (d `Set.notMember` dirs) $ Left SomeError
  pure $! st { opened = Set.insert f opened }
  -- TODO: make this fail after opening two directories.
