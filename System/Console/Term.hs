{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : System.Console.Term
Copyright   : (c) Craig Roche 2013
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : experimental
Portability : non-portable

This module defines 'TermT', an abstract Monad Transformer used to enrich a
monad with Haskeline functionality. It exports the class 'MonadTerm' which is
polymorphic over transformer stacks that contain 'TermT' and 'IO'.

@MonadTerm m@ is equivalent in power to @ReaderT ('Prompt', 'H.InputState') m@.
However, @MonadTerm@ has a @MonadReader@ instance that lifts the functionality
of the underlying monad instead of \"stealing\" the @MonadReader@ interface.

/Note:/ This module uses OverlappingInstances to implement the @MonadTerm@
class for all instances of @MonadTrans@. This is required because @TermT@ is
itself an instance of @MonadTrans@. It should not be necessary for users to
define new instances of @MonadTerm@.
-}

module System.Console.Term
(
-- * Term
  TermT
, Term
, mapTermT
, runTerm
, runTermWith
-- * MonadTerm
, MonadTerm ( inputState, showPrompt, localPrompt )
, Prompt
, withPrompt
-- * Haskeline Functionality
-- | The following functions reimplement Haskeline functionality in terms of
-- the 'MonadTerm' class.
-- ** Input
, inputLine
, inputChar
-- *** Continuation-style
, inputLineTo
, inputCharTo
-- ** Output
, outputLine
, outputStr
-- * Monadic Combinators
, retryWhen
, retryUnless
, retryJust
, retryBlank
-- * Haskeline Utils
, liftInput
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

import Control.Monad.Base          ( MonadBase, liftBase )
import Control.Monad.Trans.Control ( MonadBaseControl, liftBaseOp )
import Control.Exception           ( bracketOnError )


-- | A textual prompt displayed when reading user input from stdin.
type Prompt = String

-- private
data TermEnv = TE
    { tePrompt :: Prompt
    , teInputState :: H.InputState
    }

mapPrompt :: (Prompt -> Prompt) -> TermEnv -> TermEnv
mapPrompt f te = te { tePrompt = f (tePrompt te) }


-- | An abstract monad transformer that encapsulates a Haskeline terminal.
newtype TermT m a = TermT { unTermT :: ReaderT TermEnv m a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix,
              MonadIO, MonadTrans, MonadState s, MonadWriter w, MonadError e,
              MonadBase b)

mapTermT :: (m a -> n b) -> TermT m a -> TermT n b
mapTermT f = TermT . mapReaderT f . unTermT

instance (MonadReader e m) => MonadReader e (TermT m) where
    ask = lift ask
    local = mapTermT . local
    reader = lift . reader

instance MFunctor TermT where
    hoist f = TermT . mapReaderT f . unTermT


type Term = TermT IO

-- | Run a @TermT@ computation in an exception-safe manner.
runTerm :: (MonadBaseControl IO m) => TermT m a -> m a
runTerm = runTermWith H.defaultSettings

-- | Run a @TermT@ computation with custom Haskeline settings.
runTermWith :: (MonadBaseControl IO m) => H.Settings IO -> TermT m a -> m a
runTermWith hs ma = runHaskeline hs $ runReaderT (unTermT ma) . TE ""


class (MonadIO m) => MonadTerm m where
    -- | Retrieve the Haskeline 'InputState'.
    inputState :: m H.InputState
    -- | Retrieve the terminal's string prompt.
    showPrompt :: m String
    -- | Execute a computation with a modified 'Prompt'.
    -- Similar to 'local'.
    localPrompt :: (Prompt -> Prompt) -> m a -> m a

instance (MonadIO m) => MonadTerm (TermT m) where
    inputState = TermT (asks teInputState)
    showPrompt = TermT (asks tePrompt)
    localPrompt f = TermT . local (mapPrompt f) . unTermT

instance (MFunctor t, MonadTrans t, MonadTerm m, MonadIO (t m))
        => MonadTerm (t m) where
    inputState = lift inputState
    showPrompt = lift showPrompt
    localPrompt f = hoist (localPrompt f)

-- | Prompt for one line of input.
-- Returns @Just@ the input or @Nothing@ on EOF.
inputLine :: (MonadTerm m) => m (Maybe String)
inputLine = liftInput . H.getInputLine =<< showPrompt

-- | Prompt for one character of input.
-- Returns @Just@ the input or @Nothing@ on EOF.
inputChar :: (MonadTerm m) => m (Maybe Char)
inputChar = liftInput . H.getInputChar =<< showPrompt

-- | Prompt for one line of input and pass it to a continuation.
-- Equal to @return ()@ on EOF.
inputLineTo :: (MonadTerm m) => (String -> m ()) -> m ()
inputLineTo k = maybe (return ()) k =<< inputLine

-- | Prompt for one character of input and pass it to a continuation.
-- Equal to @return ()@ on EOF.
inputCharTo :: (MonadTerm m) => (String -> m ()) -> m ()
inputCharTo k = maybe (return ()) k =<< inputLine


-- | Write a String to stdout, followed by a newline.
outputLine :: (MonadTerm m) => String -> m ()
outputLine = liftInput . H.outputStrLn

-- | Write a String to stdout.
outputStr :: (MonadTerm m) => String -> m ()
outputStr = liftInput . H.outputStr

-- | Execute a computation with the given 'Prompt'.
withPrompt :: (MonadTerm m) => String -> m a -> m a
withPrompt = localPrompt . const


------------------------------------------------------------------------------
-- Monadic Combinators

-- | @retryJust f ma@ executes the monadic action @ma@ and applies @f@ to
-- its result. If @ma >>= f@ is @Nothing@, the result of ma is returned. If
-- it is @Just errMsg@, @errMsg@ is printed to stdout and @ma@ is tried
-- again.
retryJust :: (MonadTerm m) => (a -> Maybe String) -> m a -> m a
retryJust f ma = do
    x <- ma
    case f x of
        Nothing -> return x
        Just errStr -> outputLine errStr >> retryJust f ma

retryWhen :: (Monad m) => (a -> Bool) -> m a -> m a
retryWhen p ma = do
    x <- ma
    if p x
      then retryWhen p ma
      else return x

retryUnless :: (Monad m) => (a -> Bool) -> m a -> m a
retryUnless p = retryWhen (not . p)

retryBlank :: (Monad m) => m [a] -> m [a]
retryBlank = retryWhen null


------------------------------------------------------------------------------
-- Haskeline Utils

-- | Lift a Haskeline InputT action to a MonadTerm action.
liftInput :: (MonadTerm m) => H.InputT IO a -> m a
liftInput ma = do
    i <- inputState
    liftIO $ H.queryInput i ma


-- this should really be in System.Console.Haskeline.IO:

-- | Run a Haskeline 'InputState' computation in an exception-safe manner.
runHaskeline :: (MonadBaseControl IO m)
               => H.Settings IO
               -> (H.InputState -> m a)
               -> m a
runHaskeline hs k = bracket $ \i -> k i <* liftBase (H.closeInput i)
  where
    bracket = liftBaseOp $
        bracketOnError (H.initializeInput hs) H.cancelInput


------------------------------------------------------------------------------
-- Simple test

testTerm :: Term ()
testTerm = inputLineTo reader
  where
    reader "quit" = return ()
    reader input  = do
        outputLine $ "got " ++ input
        testTerm

test :: IO ()
test = runTerm $ withPrompt "test> " testTerm


-- TODO
-- add TermT instances for MonadTransControl and MonadBaseControl
-- this will enable the following:
{-
layerTest :: IO ()
layerTest = runTerm $ runTerm term
  where
    term :: TermT Term ()
    term = do
        Just a <- inputLine
        Just b <- lift $ inputLine <* outputLine a
        outputLine b
-}
