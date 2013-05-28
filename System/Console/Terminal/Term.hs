{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : System.Console.Terminal.Term
Copyright   : (c) Craig Roche 2013
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : experimental
Portability : non-portable

This module defines 'TermT', a 'MonadTerm' instance implementated in terms of
Haskeline.
-}

module System.Console.Terminal.Term
(
-- * Term
  TermT
, Term

-- ** Evaluation
-- | The following functions utilize the @MonadBaseControl IO@ parameter to
-- provide exception-safe resource handling of the underlying Haskeline
-- 'H.InputState':

, runTerm
, runTermWith
, loopTerm
, loopTermWith


-- * Haskeline Utils
, liftInput
, interpretTerm
, runHaskeline
)
where


import qualified System.Console.Haskeline as H
import qualified System.Console.Haskeline.IO as H

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Morph

import Control.Monad.Trans.Maybe

import Control.Monad.Base          ( MonadBase, liftBase )
import Control.Monad.Trans.Control ( MonadBaseControl, liftBaseOp )
import Control.Exception           ( bracketOnError )

import System.Console.Terminal.Class


-- private
data TermEnv = TE
    { termPrompt :: Prompt
    , termInputState :: H.InputState
    }

mapPrompt :: (Prompt -> Prompt) -> TermEnv -> TermEnv
mapPrompt f te = te { termPrompt = f (termPrompt te) }


-- | An abstract monad transformer that encapsulates a Haskeline terminal.
newtype TermT m a = TermT { unTermT :: ReaderT TermEnv (MaybeT m) a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix,
              MonadIO, MonadState s, MonadWriter w, MonadError e,
              MonadBase b)

instance (MonadReader e m) => MonadReader e (TermT m) where
    ask = lift ask
    local f = hoist (local f)
    reader = lift . reader
 
instance MFunctor TermT where
    hoist nat (TermT m) = TermT $ hoist (hoist nat) m

instance MonadTrans TermT where
    lift = undefined

type Term = TermT IO



-- | Interpret a TermT computation as a function of a Haskeline 'H.InputState'.
-- Users should avoid this in favor of 'runTerm' or 'loopTerm'.
interpretTerm :: TermT m a -> H.InputState -> m (Maybe a)
interpretTerm (TermT m) = runMaybeT . runReaderT m . TE ""


-- | Run a @TermT@ computation, returning @Just@ a value or @Nothing@ if EOF
-- was encountered. Equivalent to @'runTermWith' 'H.defaultSettings'@
runTerm :: (MonadBaseControl IO m) => TermT m a -> m (Maybe a)
runTerm = runTermWith H.defaultSettings

-- | Run a @TermT@ computation with custom Haskeline settings.
-- Returns @Just@ a value or @Nothing@ if EOF was encountered.
runTermWith :: (MonadBaseControl IO m) => H.Settings IO -> TermT m a -> m (Maybe a)
runTermWith hs = runHaskeline hs . interpretTerm


-- | Equivalent to @'loopTermWith' 'H.defaultSettings'@
loopTerm :: (MonadBaseControl IO m) => TermT m a -> m [a]
loopTerm = loopTermWith H.defaultSettings

-- | Run a @TermT@ computation repeatedly until it returns 'Nothing',
-- collecting a list of the results.
loopTermWith :: (MonadBaseControl IO m) => H.Settings IO -> TermT m a -> m [a]
loopTermWith hs t = runHaskeline hs loop
  where
    loop is = do
        ma <- interpretTerm t is 
        case ma of
            Nothing -> return []
            Just a  -> fmap (a:) (loop is)


instance (MonadIO m) => MonadTerm (TermT m) where
    showPrompt    = TermT (asks termPrompt)
    localPrompt f = TermT . local (mapPrompt f) . unTermT
    tryInputLine  = liftInput $ H.getInputLine ""
    tryInputChar  = liftInput $ H.getInputChar ""
    outputStr     = liftInput . H.outputStr



------------------------------------------------------------------------------
-- Haskeline Utils

-- | Lift a Haskeline InputT action to a TermT action.
liftInput :: (MonadIO m) => H.InputT IO a -> TermT m a
liftInput m = do
    i <- TermT (asks termInputState)
    liftIO $ H.queryInput i m

-- | Run a Haskeline 'InputState' computation in an exception-safe manner.
runHaskeline :: (MonadBaseControl IO m)
             => H.Settings IO
             -> (H.InputState -> m a)
             -> m a
runHaskeline hs k = bracket $ \i -> k i <* liftBase (H.closeInput i)
  where
    bracket = liftBaseOp $
        bracketOnError (H.initializeInput hs) H.cancelInput

