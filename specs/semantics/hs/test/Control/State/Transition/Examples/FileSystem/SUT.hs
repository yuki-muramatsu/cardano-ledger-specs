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
  | TooManyOpenFiles (Set File)
  | FileDoesNotExist File
  | Busy File
  deriving (Show)

data State
  = State
  { dirs :: Set Dir
  -- ^ Directories known to the system.
  , files :: Set File
  -- ^ Files known to the system.
  , opened :: Set File
  -- ^ Open files.
  }
  deriving (Show)

initSt :: State
initSt
  = State
  { dirs = Set.singleton $ Dir []
  , files = Set.empty
  , opened = Set.empty
  }


mkdir :: State -> Dir -> Either Error State
mkdir st@State{dirs} d
  | d `Set.member` dirs = Left $ DirectoryExists d st
  | otherwise           = Right $ st { dirs = Set.insert d dirs }

open :: State -> File -> Either Error State
open st@State{ dirs, files, opened } f@(File d _) = do
--  when (0 < Set.size opened) $ Left (TooManyOpenFiles opened) -- Try commenting this out.
  when (f `Set.member` opened) $ Left SomeError
  when (d `Set.notMember` dirs) $ Left SomeError
  pure $! st { files = Set.insert f files
             , opened = Set.insert f opened
             }
  -- TODO: make this fail after opening two directories.

close :: State -> File -> Either Error State
close st@State{ opened } f =
  pure $! st { opened = Set.delete f opened }

read :: State -> File -> Either Error State
read st@State { files, opened } f = do
  when (f `Set.notMember` files) $ Left $ FileDoesNotExist f
  when (f `Set.member` opened) $ Left $ Busy f
  -- Uncomment the line below for giving a counter-example of a sucessful read
  Left SomeError
  pure $! st { opened = Set.insert f opened }
