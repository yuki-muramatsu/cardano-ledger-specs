{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Control.State.Transition.Examples.FileSystem.Model2 where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad (mzero)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef, writeIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import Hedgehog
  ( Callback
  , Callback(Ensure, Require, Update)
  , Command(Command)
  , Concrete
  , Gen
  , HTraversable
  , MonadTest
  , Property
  , Symbolic
  , Test
  , Var(Var)
  , (===)
  , annotate
  , assert
  , checkParallel
  , concrete
  , discover
  , evalEither
  , evalIO
  , executeSequential
  , failure
  , forAll
  , htraverse
  , property
  , success
  , withTests
  , Ord1, Show1, Eq1
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.State.Transition

import Control.State.Transition.Examples.FileSystem.Common
import qualified Control.State.Transition.Examples.FileSystem.SUT as SUT
import qualified Control.State.Transition.Examples.FileSystem.STS2 as STS

data Cmd (v :: * -> *)
  = AddDir Dir -- Register a directory in the abstract state, so that we can
               -- use it later in mkdir, or even in open to be able to open
               -- files in non-existing directories!
--  | MkDir (Var Dir v) -- So all this nonsense needs to be a response!!!! Everything becomes un-typed
  | MkDir (Var SUTRsp v)
  | Open (Var SUTRsp v) String
  deriving (Show)


instance HTraversable Cmd where
  htraverse
    :: Applicative f
    => (forall a. g a -> f (h a))
    -> Cmd g
    -> f (Cmd h)
  htraverse f (AddDir d) = pure $! AddDir d
  htraverse f (MkDir (Var dir)) = MkDir . Var <$> f dir
  htraverse f (Open (Var dir) n) = (`Open` n) . Var <$> f dir

data SUTRsp
  = SUTError SUT.Error
  | SUTSigSt (SUTSignal, SUT.State)
  | AddedDir Dir
  deriving (Eq, Ord, Show)

data SUTSignal
  = SigMkDir Dir
  | SigOpen File
  deriving (Eq, Ord, Show)

data AbstractState (v :: * -> *)
  -- NOTE: the abstract state could includes a list of predicate failures (so
  -- it is isomorphic to the return type of @applySTSIndifferently@) so that we
  -- can check the abstract failures against the concrete ones.
  = AbstractState
  { abstractDirNames   :: Set (Var SUTRsp v)
  -- ^ I had to do this contortion to be able to generate directory names, and
  -- link them to symbolic variables when calling mkdir.
  , stsLastSeenState   :: State (STS.FS (Var SUTRsp v))
  , stsLastSeenFailure :: [PredicateFailure (STS.FS (Var SUTRsp v))]
  }

fsCmdsDef
  :: forall m
   . (MonadTest m, MonadIO m)
  => IORef (SUT.State)
  -> Command Gen m AbstractState
fsCmdsDef stsStRef = Command gen execute callbacks
  where
    gen :: AbstractState Symbolic -> Maybe (Gen (Cmd Symbolic))
    gen = undefined
--    gen = Just $! pure MkDir Var

    execute :: Cmd Concrete -> m SUTRsp
    execute (AddDir dir) = do
      pure $! AddedDir $ dir
    execute (MkDir vDir) = do
      st <- evalIO $ readIORef stsStRef
      let
        AddedDir dir = concrete vDir
        rsp = SUT.mkdir st dir
      updateAndReturn stsStRef (SigMkDir dir) rsp
    execute (Open vDir n) = do
      st <- evalIO $ readIORef stsStRef
      let
        AddedDir d = concrete vDir
        f = File d n
        rsp = SUT.open st f
      updateAndReturn stsStRef (SigOpen f) rsp

    updateAndReturn ref sig rsp = do
      case rsp of
        Left err -> pure $! SUTError err
        Right st' -> do
          evalIO $ writeIORef ref st'
          pure $! SUTSigSt (sig, st')

    callbacks :: [Callback Cmd SUTRsp AbstractState]
    callbacks = [Require pre, Update update, Ensure post]

    pre
      :: AbstractState Symbolic
      -> Cmd Symbolic
      -> Bool
    pre _ _ = True

    update
      :: forall v
       . (Ord1 v) => AbstractState v
      -> Cmd v
      -> Var SUTRsp v
      -> AbstractState v
    update (ast@AbstractState { abstractDirNames }) (AddDir vDir) rsp =
      ast { abstractDirNames = Set.insert rsp abstractDirNames }
    update (ast@AbstractState { stsLastSeenState = st }) (MkDir vDir) (Var symOrConcRsp) =
      case applySTS @(STS.FS (Var SUTRsp v)) $ TRC ((), st, l) of
        Left pfs -> ast { stsLastSeenFailure = pfs }  -- Do not update the state on failure.
        Right st' -> ast { stsLastSeenState = st', stsLastSeenFailure = [] }
      where
        l = STS.MkDir vDir
    -- update (AbstractState (st, _)) (Open (Var vDir) n) (Var symOrConcRsp) =
    --   undefined
    --   where
    --     l = STS.Open (File d??? n)

    post
      :: AbstractState Concrete
      -> AbstractState Concrete
      -> Cmd Concrete
      -> SUTRsp
      -> Test ()
    post = undefined
