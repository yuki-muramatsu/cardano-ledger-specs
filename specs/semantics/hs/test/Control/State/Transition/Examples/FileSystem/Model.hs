{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | State machine model of the filesystem.
module Control.State.Transition.Examples.FileSystem.Model where

import Control.Monad.IO.Class (MonadIO)
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
  , Var
  , (===)
  , annotate
  , assert
  , checkParallel
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
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.State.Transition
import Control.State.Transition.Examples.FileSystem.Common
import qualified Control.State.Transition.Examples.FileSystem.STS as STS
import qualified Control.State.Transition.Examples.FileSystem.SUT as SUT

prop_SUTImplementsSTS :: Property
prop_SUTImplementsSTS = withTests 1000 $ property $ do
  stsStRef <- evalIO $ newIORef SUT.initSt

  actions <- forAll $
    Gen.sequential
      (Range.linear 1 100)
      initialState
      [fsCmdsDef stsStRef]

  executeSequential initialState actions

data AbstractState (v :: * -> *) =
  -- NOTE: the abstract state could includes a list of predicate failures (so
  -- it is isomorphic to the return type of @applySTSIndifferently@) so that we
  -- can check the abstract failures against the concrete ones.
  AbstractState (State STS.FS, [PredicateFailure STS.FS])
  deriving (Show)

initialState :: AbstractState v
initialState = AbstractState $ (STS.initSt, [])

data Cmd (v :: * -> *) = Cmd STS.Cmd
  deriving (Eq, Show)

instance HTraversable Cmd where
  htraverse
    :: Applicative f
    => (forall a. g a -> f (h a))
    -> Cmd g
    -> f (Cmd h)
  htraverse _ (Cmd cmd) = pure $! Cmd cmd -- f (Cmd ())

type SUTResp = Either SUT.Error SUT.State

allNames :: [String]
allNames = ["a", "b", "c", "d", "e", "f", "g", "h"]

genDir :: Gen Dir
genDir = Dir <$> Gen.list (Range.linear 1 10) (Gen.element allNames)

genFile :: Set Dir -> Gen File
genFile dirs = do
  d <- Gen.choice
       [ Gen.element $ Set.toList dirs
       , genDir -- We also want to generate invalid files!
       ]
  n <- Gen.element allNames
  pure $! File d n

fsCmdsDef
  :: forall m
   . (MonadTest m, MonadIO m)
  => IORef (SUT.State)
  -- ^ Reference to the concrete state. In a state machine model we shouldn't
  -- need to use the concrete state of the SUT. However, when testing STS's we
  -- need the STS state when define the @execute@ function, so it seems there's
  -- no way around it.
  -> Command Gen m AbstractState
fsCmdsDef stsStRef = Command gen execute callbacks
  where
    gen :: AbstractState v -> Maybe (Gen (Cmd v))
    gen (AbstractState ((dirs, _), _)) =
      Just $ Cmd <$>
        Gen.choice [ STS.MkDir <$> genDir
                   , STS.Open <$> genFile dirs
                   ]

    execute :: Cmd v -> m (SUTResp)
    execute (Cmd (STS.MkDir d)) = do
      st <- evalIO $ readIORef stsStRef
      let resp = SUT.mkdir st d
      updateAndReturn stsStRef resp
    execute (Cmd (STS.Open f)) = do
      st <- evalIO $ readIORef stsStRef
      let resp = SUT.open st f
      updateAndReturn stsStRef resp

    updateAndReturn ref resp = do
      case resp of
        Left _ -> pure $! ()
        Right st' -> evalIO $ writeIORef ref st'
      pure $! resp

    callbacks :: [Callback Cmd SUTResp AbstractState]
    callbacks = [Require pre, Update update, Ensure post]

    pre
      :: AbstractState Symbolic
      -> Cmd Symbolic
      -> Bool
    pre _ _ = True

    update
      :: AbstractState v
      -> Cmd v
      -> Var SUTResp v
      -> AbstractState v
    update (AbstractState (st, _)) (Cmd l) _ =
      case applySTS @STS.FS $ TRC ((), st, l) of
        Left pfs -> AbstractState (st, pfs) -- Do not update the state on failure.
        Right st' -> AbstractState (st', [])

    post
      :: AbstractState Concrete
      -> AbstractState Concrete
      -> Cmd Concrete
      -> SUTResp
      -> Test ()
    post _ (AbstractState (_, pfs)) _ eErrSt =
      case (pfs, eErrSt) of
        ([], Right _) ->
          -- The STS transition and the actual function succeeded. We do not
          -- want to relate the abstract and concrete state in this case. What
          -- is important here is that if the STS can do a transition, we can
          -- successfully apply the concrete function.
          success
        (_:_, Right _) -> do
          annotate $ "The STS failed (" ++ show pfs ++ "), the SUT didn't"
          failure
        ([], Left err) -> do
          annotate $ "The STS didn't fail, the SUT did (" ++ show err ++ ")"
          failure
        (_:_, Left _) -> do
          -- The STS and the SUT failed. We do not check the equivalence of the
          -- returned errors for now.
          success

tests :: IO Bool
tests =
  checkParallel $$(discover)
