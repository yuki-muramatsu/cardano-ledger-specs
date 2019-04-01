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

  type State OPEN
    = ( Set File -- Opened files
      , Set File -- Existing files
      )

  type Signal OPEN = File

  data PredicateFailure OPEN
    = DirectoryDoesNotExist Dir
    | Busy File (Set Dir) (Set File)
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC (dirs, (ofs, efs), f@(File d _)) <- judgmentContext
        d `Set.member` dirs ?! DirectoryDoesNotExist d
        f `Set.notMember` ofs ?! Busy f dirs ofs
        pure $! (Set.insert f ofs, Set.insert f efs)
    ]

--------------------------------------------------------------------------------
-- CLOSE transition system
--------------------------------------------------------------------------------

data CLOSE

instance STS CLOSE where

  type Environment CLOSE = ()

  type State CLOSE = Set File

  type Signal CLOSE = File

  data PredicateFailure CLOSE = NoFailure
    deriving (Show, Eq)

  initialRules = []

  transitionRules =
    [ do
        TRC ((), ofs, f) <- judgmentContext
        pure $! Set.delete f ofs
    ]

--------------------------------------------------------------------------------
-- READ transition system
--------------------------------------------------------------------------------

data READ

instance STS READ where

  type Environment READ = Set File -- Existing files

  type State READ = Set File -- Opened files

  type Signal READ = File

  data PredicateFailure READ
    = FileDoesNotExist File
    | ReadFileBusy File
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC (efs, ofs, f) <- judgmentContext
        f `Set.member` efs ?! FileDoesNotExist f
        f `Set.notMember` ofs ?! ReadFileBusy f
        pure $! Set.insert f ofs
    ]

--------------------------------------------------------------------------------
-- FS transition system
--------------------------------------------------------------------------------

data FS

data Cmd
  = MkDir Dir
  | Open File
  | Close File
  | Read File
  deriving (Eq, Show)

instance STS FS where

  type Environment FS = ()

  type State FS
    = ( Set Dir  -- Existing directories
      , Set File -- Opened files
      , Set File -- Existing files
      )

  type Signal FS = Cmd

  data PredicateFailure FS
    = MkDirFailed (PredicateFailure MKDIR)
    | OpenFailed (PredicateFailure OPEN)
    | CloseFailed (PredicateFailure CLOSE)
    | ReadFailed (PredicateFailure READ)
    deriving (Eq, Show)

  initialRules = [ pure $! initSt ]

  transitionRules =
    [ do
        TRC ((), (dirs, ofs, efs), cmd) <- judgmentContext
        case cmd of
          MkDir d -> do
            dirs' <- trans @MKDIR $ TRC ((), dirs, d)
            pure $! (dirs', ofs, efs)
          Open f -> do
            (ofs', efs') <- trans @OPEN $ TRC (dirs, (ofs, efs), f)
            pure $! (dirs, ofs', efs')
          Close f -> do
            ofs' <- trans @CLOSE $ TRC ((), ofs, f)
            pure $! (dirs, ofs', efs)
          Read f -> do
            ofs' <- trans @READ $ TRC (efs, ofs, f)
            pure $! (dirs, ofs', efs)
    ]

instance Embed MKDIR FS where
  wrapFailed = MkDirFailed

instance Embed OPEN FS where
  wrapFailed = OpenFailed

instance Embed CLOSE FS where
  wrapFailed = CloseFailed

instance Embed READ FS where
  wrapFailed = ReadFailed

initSt :: State FS
initSt = (Set.singleton $ Dir [], Set.empty, Set.empty)
