{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
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
  = MkDir (Var Dir v)
  | Open (Var Dir v) String
  deriving (Eq, Show)

instance HTraversable Cmd where
  htraverse
    :: Applicative f
    => (forall a. g a -> f (h a))
    -> Cmd g
    -> f (Cmd h)
  htraverse f (MkDir (Var dir)) = MkDir . Var <$> f dir
  htraverse f (Open (Var dir) n) = (`Open` n) . Var <$> f dir

type SUTResp = Either SUT.Error (SUTSignal, SUT.State)

data SUTSignal
  = SigMkDir Dir
  | SigOpen File

data AbstractState (v :: * -> *) =
  -- NOTE: the abstract state could includes a list of predicate failures (so
  -- it is isomorphic to the return type of @applySTSIndifferently@) so that we
  -- can check the abstract failures against the concrete ones.
  AbstractState (State (STS.FS (Var Dir v)), [PredicateFailure (STS.FS (Var Dir v))])

fsCmdsDef
  :: forall m
   . (MonadTest m, MonadIO m)
  => IORef (SUT.State)
  -> Command Gen m AbstractState
fsCmdsDef stsStRef = Command gen execute callbacks
  where
    gen :: AbstractState Symbolic -> Maybe (Gen (Cmd Symbolic))
    gen = undefined

    execute :: Cmd Concrete -> m SUTResp
    execute (MkDir vDir) = do
      st <- evalIO $ readIORef stsStRef
      let
        d = concrete vDir
        resp = SUT.mkdir st d
      updateAndReturn stsStRef (SigMkDir d) resp
    execute (Open vDir n) = do
      st <- evalIO $ readIORef stsStRef
      let
        d = concrete vDir
        f = File d n
        resp = SUT.open st f
      updateAndReturn stsStRef (SigOpen f) resp

    updateAndReturn ref sig resp = do
      case resp of
        Left err -> pure $! Left err
        Right st' -> do
          evalIO $ writeIORef ref st'
          pure $! Right (sig, st')

    callbacks :: [Callback Cmd SUTResp AbstractState]
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
      -> Var SUTResp v
      -> AbstractState v
    update (AbstractState (st, _)) (MkDir vDir) (Var symOrConcResp) =

      -- What is the type we need to pass to 'applySTS'?
      --
      --  - STS.FS v won't work (kind mismatch)
      --
      --  - STS.FS (Var Dir v) won't work either (need a buch of constraints, but even this is not enough)

      undefined $ applySTS @(STS.FS (Var Dir v)) $ TRC ((), st, l)
      --   Left pfs -> undefined --AbstractState (st, pfs) -- Do not update the state on failure.
      --   Right st' -> undefined -- AbstractState (st', [])
      where
        l = STS.MkDir vDir
    -- update (AbstractState (st, _)) (Open (Var vDir) n) (Var symOrConcResp) =
    --   undefined
    --   where
    --     l = STS.Open (File d??? n)

    post
      :: AbstractState Concrete
      -> AbstractState Concrete
      -> Cmd Concrete
      -> SUTResp
      -> Test ()
    post = undefined
