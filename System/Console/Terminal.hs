{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : System.Console.Terminal
Copyright   : (c) Craig Roche 2013
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : experimental
Portability : non-portable

This module defines 'TermT', an abstract monad transformer used to add Haskeline
functionality to an IO-based monad stack. It is intended as an alternative to
Haskeline's InputT transformer. It implements primitives in terms of the class
'MonadTerm' which is polymorphic over transformer stacks that contain 'TermT'
and 'IO'.

/Note:/ This module uses OverlappingInstances to implement the @MonadTerm@
class for all instances of @MonadTrans@. This is required because @TermT@ is
itself an instance of @MonadTrans@. It should not be necessary for users to
define new instances of @MonadTerm@.

In the future, this may be removed in favor of the more common practice of
simply defining an instance for each type in the transformers package.
-}

module System.Console.Terminal
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
, tryInputLine
, tryInputChar

-- ** Output
, outputLine
, outputStr

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
        

class (MonadIO m, MonadPlus m) => MonadTerm m where
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

instance (MFunctor t, MonadTrans t, MonadTerm m, MonadIO (t m), MonadPlus (t m))
        => MonadTerm (t m) where
    inputState = lift inputState
    showPrompt = lift showPrompt
    localPrompt f = hoist (localPrompt f)


-- | Prompt for one line of input.
-- Equal to mzero at EOF.
inputLine :: (MonadTerm m) => m String
inputLine = maybe mzero return =<< tryInputLine

-- | Prompt for one line of input.
-- Return @Just@ the input or @Nothing@ at EOF.
tryInputLine :: (MonadTerm m) => m (Maybe String)
tryInputLine = liftInput . H.getInputLine =<< showPrompt

-- | Prompt for one character of input.
-- Equal to mzero at EOF.
inputChar :: (MonadTerm m) => m Char
inputChar = maybe mzero return =<< tryInputChar

-- | Prompt for one character of input.
-- Return @Just@ the input or @Nothing@ at EOF.
tryInputChar :: (MonadTerm m) => m (Maybe Char)
tryInputChar = liftInput . H.getInputChar =<< showPrompt


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
-- Haskeline Utils

-- | Lift a Haskeline InputT action to a MonadTerm action.
liftInput :: (MonadTerm m) => H.InputT IO a -> m a
liftInput m = do
    i <- inputState
    liftIO $ H.queryInput i m


-- perhaps this belongs in System.Console.Haskeline.IO:

-- | Run a Haskeline 'InputState' computation in an exception-safe manner.
runHaskeline :: (MonadBaseControl IO m)
               => H.Settings IO
               -> (H.InputState -> m a)
               -> m a
runHaskeline hs k = bracket $ \i -> k i <* liftBase (H.closeInput i)
  where
    bracket = liftBaseOp $
        bracketOnError (H.initializeInput hs) H.cancelInput


