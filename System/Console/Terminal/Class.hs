{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : System.Console.Terminal.Class
Copyright   : (c) Craig Roche 2013
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : experimental
Portability : non-portable

This module defines 'MonadTerm', a typeclass providing an abstract interface
for terminal operations such as displaying text and prompting the user for
input.

/Note:/ This module uses OverlappingInstances to implement the @MonadTerm@
class for all instances of @MonadTrans@. In the future, this may be removed in
favor of the more common practice of simply defining an instance for each type
in the transformers package.
-}

module System.Console.Terminal.Class
(
-- * MonadTerm
  MonadTerm ( .. )
, Prompt
, withPrompt

-- * Operations
-- | The following functions encapsulate Haskeline functionality within the
-- 'MonadTerm' class.

-- ** Input
, tryPromptLine
, tryPromptChar
, inputLine
, inputChar
, promptLine
, promptChar

-- ** Output
, outputLine
, printLine
)
where

import Control.Monad
import Control.Monad.Morph


-- | A textual prompt displayed when reading user input from stdin.
type Prompt = String

class (Monad m) => MonadTerm m where
    -- | Retrieve the terminal's string prompt.
    showPrompt :: m String
    -- | Execute a computation with a modified 'Prompt'.
    -- Similar to 'local'.
    localPrompt :: (Prompt -> Prompt) -> m a -> m a
    -- | Read one line of input from the user.
    -- Returns @Just@ the input, or @Nothing@ if the user inputs EOF.
    tryInputLine :: m (Maybe String)
    -- | Read one character of input from the user.
    -- Returns @Just@ the input, or @Nothing@ if the user inputs EOF.
    tryInputChar :: m (Maybe Char)
    -- | Write a String to the terminal.
    outputStr :: String -> m ()

instance (MFunctor t, MonadTrans t, Monad (t m), MonadTerm m) => MonadTerm (t m) where
    showPrompt    = lift showPrompt
    localPrompt f = hoist (localPrompt f)
    tryInputLine  = lift tryInputLine
    tryInputChar  = lift tryInputChar
    outputStr     = lift . outputStr



outputPrompt :: (MonadTerm m) => m ()
outputPrompt = showPrompt >>= outputStr

-- | Prompt the user for one line of input.
-- Returns @Just@ the input, or @Nothing@ if the user inputs EOF.
tryPromptLine :: (MonadTerm m) => m (Maybe String)
tryPromptLine = outputPrompt >> tryInputLine
--tryPrompt = liftInput . H.getInputLine =<< showPrompt

tryPromptChar :: (MonadTerm m) => m (Maybe Char)
tryPromptChar = outputPrompt >> tryInputChar


liftMaybe :: (MonadPlus m) => m (Maybe a) -> m a
liftMaybe m = maybe mzero return =<< m

-- | Read one line of input.
-- Equal to mzero at EOF.
inputLine :: (MonadTerm m, MonadPlus m) => m String
inputLine = liftMaybe tryInputLine

-- | Prompt for one character of input.
-- Equal to mzero at EOF.
inputChar :: (MonadTerm m, MonadPlus m) => m Char
inputChar = liftMaybe tryInputChar

-- | Prompt the user for one line of input.
-- Equal to mzero at EOF.
promptLine :: (MonadTerm m, MonadPlus m) => m String
promptLine = liftMaybe tryPromptLine

promptChar :: (MonadTerm m, MonadPlus m) => m Char
promptChar = liftMaybe tryPromptChar

-- | Prompt for one line of input, and 'read' it.
--promptRead :: (MonadTerm m, MonadPlus m, Read a) => m (Maybe a)
--promptRead = liftM readMaybe inputLine


-- | Write a String to stdout, followed by a newline.
outputLine :: (MonadTerm m) => String -> m ()
outputLine = outputStr . (++ "\n")


-- | Write a value to stdout using 'show', followed by a newline.
printLine :: (MonadTerm m, Show a) => a -> m ()
printLine = outputLine . show


-- | Execute a computation with the given 'Prompt'.
withPrompt :: (MonadTerm m) => String -> m a -> m a
withPrompt = localPrompt . const

