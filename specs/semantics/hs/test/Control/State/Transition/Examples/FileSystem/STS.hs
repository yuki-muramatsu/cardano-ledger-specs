{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}

module Control.State.Transition.Examples.FileSystem.STS where

import Data.Set (Set)
import qualified Data.Set as Set

import Control.State.Transition

import Control.State.Transition.Examples.FileSystem.Common

--------------------------------------------------------------------------------
-- MKDIR transition system
--------------------------------------------------------------------------------

data MKDIR

instance STS MKDIR where

  type Environment MKDIR = ()

  type State MKDIR = Set Dir

  type Signal MKDIR = Dir

  data PredicateFailure MKDIR
    = DirAlreadyExists Dir
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ((), dirs, d) <- judgmentContext
        d `Set.notMember` dirs ?! DirAlreadyExists d
        pure $! Set.insert d dirs
    ]

--------------------------------------------------------------------------------
-- OPEN transition system
--------------------------------------------------------------------------------

data OPEN

instance STS OPEN where

  type Environment OPEN = Set Dir

  type State OPEN = Set File

  type Signal OPEN = File

  data PredicateFailure OPEN
    = DirectoryDoesNotExist Dir
    | Busy File (Set Dir) (Set File)
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC (dirs, ofs, f@(File d _)) <- judgmentContext
        d `Set.member` dirs ?! DirectoryDoesNotExist d
        f `Set.notMember` ofs ?! Busy f dirs ofs
        pure $! Set.insert f ofs
    ]

--------------------------------------------------------------------------------
-- FS transition system
--------------------------------------------------------------------------------

data FS

data Cmd
  = MkDir Dir
  | Open File
  deriving (Eq, Show)

instance STS FS where

  type Environment FS = ()

  type State FS = (Set Dir, Set File)

  type Signal FS = Cmd

  data PredicateFailure FS
    = MkDirFailed (PredicateFailure MKDIR)
    | OpenFailed (PredicateFailure OPEN)
    deriving (Eq, Show)

  initialRules = [pure $! initSt ]

  transitionRules =
    [ do
        TRC ((), (dirs, ofs), cmd) <- judgmentContext
        case cmd of
          MkDir d -> do
            dirs' <- trans @MKDIR $ TRC ((), dirs, d)
            pure $! (dirs', ofs)
          Open f -> do
            ofs' <- trans @OPEN $ TRC (dirs, ofs, f)
            pure $! (dirs, ofs')
    ]

instance Embed MKDIR FS where
  wrapFailed = MkDirFailed

instance Embed OPEN FS where
  wrapFailed = OpenFailed

initSt :: State FS
initSt = (Set.singleton $ Dir [], Set.empty)
