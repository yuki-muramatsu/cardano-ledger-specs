{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.State.Transition.Examples.FileSystem.STS2 where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Hedgehog as H

import Control.State.Transition

import Control.State.Transition.Examples.FileSystem.Common


data SFile dir
  = SFile
  { directory :: dir
  , name :: String
  }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- OPEN transition system
--------------------------------------------------------------------------------

data OPEN dir

instance (Ord dir, Eq dir, Show dir) => STS (OPEN dir) where

  type Environment (OPEN dir) = Set dir

  type State (OPEN dir)
    = ( Set (SFile dir) -- Opened files
      , Set (SFile dir) -- Existing files
      )

  type Signal (OPEN dir) = SFile dir

  data PredicateFailure (OPEN dir)
    = DirectoryDoesNotExist dir
    | Busy (SFile dir)
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC (dirs, (ofs, efs), f@(SFile d _)) <- judgmentContext
        d `Set.member` dirs ?! DirectoryDoesNotExist d
        f `Set.notMember` ofs ?! Busy f
        pure $! (Set.insert f ofs, Set.insert f efs)
    ]

--------------------------------------------------------------------------------
-- MKDIR transition system
--------------------------------------------------------------------------------

data MKDIR dir

instance (Ord dir, Eq dir, Show dir) => STS (MKDIR dir) where

  type Environment (MKDIR dir) = ()

  type State (MKDIR dir) = Set dir

  type Signal (MKDIR dir) = dir

  data PredicateFailure (MKDIR dir)
    = DirAlreadyExists dir
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ((), dirs, d) <- judgmentContext
        d `Set.notMember` dirs ?! DirAlreadyExists d
        pure $! Set.insert d dirs
    ]

--------------------------------------------------------------------------------
-- FS transition system
--------------------------------------------------------------------------------

data FS dir

data Cmd dir -- dir here would be the reference to a step in the symbolic case,
             -- or the actual directory in the concrete case.
  = MkDir dir
  | Open (SFile dir)
  deriving (Eq, Show)

instance (Ord dir, Eq dir, Show dir) => STS (FS dir) where

  type Environment (FS dir) = ()

  type State (FS dir)
    = ( Set dir         -- Existing directories
      , Set (SFile dir) -- Opened files
      , Set (SFile dir) -- Existing files
      )

  type Signal (FS dir) = Cmd dir

  data PredicateFailure (FS dir)
    = MkDirFailed (PredicateFailure (MKDIR dir))
    | OpenFailed (PredicateFailure (OPEN dir))
    deriving (Eq, Show)

  initialRules = [ pure $! initSt ]

  transitionRules =
    [ do
        TRC ((), (dirs, ofs, efs), cmd) <- judgmentContext
        case cmd of
          MkDir d -> do
            dirs' <- trans @(MKDIR dir) $ TRC ((), dirs, d)
            pure $! (dirs', ofs, efs)
          Open f -> do
            (ofs', efs') <- trans @(OPEN dir) $ TRC (dirs, (ofs, efs), f)
            pure $! (dirs, ofs', efs')
    ]

instance (Ord dir, Eq dir, Show dir) => Embed (MKDIR dir) (FS dir) where
  wrapFailed = MkDirFailed

instance (Ord dir, Eq dir, Show dir) => Embed (OPEN dir) (FS dir) where
  wrapFailed = OpenFailed

initSt :: Ord dir => State (FS dir)
-- Now I cannot simply write Dir [], since I don't know what dir will be. So
-- we'll have to insert the initial directory in the model, or just modify the
-- initial state of the SUT to not include 'Dir []'.
initSt = (Set.empty, Set.empty, Set.empty)
