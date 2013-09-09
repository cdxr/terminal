{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

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
, promptLine
, promptChar

-- ** Output
, outputLine
, printLine
)
where

import Control.Monad
import Control.Monad.Morph

import Text.Read ( readMaybe )


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
    --
    -- This is provided as a primitive operation. Users are encouraged to use
    -- 'promptLine' instead.
    tryGetLine :: m (Maybe String)
    -- | Read one character of input from the user.
    -- Returns @Just@ the input, or @Nothing@ if the user inputs EOF.
    --
    -- This is provided as a primitive operation. Users are encouraged to use
    -- 'promptChar' instead.
    tryGetChar :: m (Maybe Char)
    -- | Write a String to the terminal.
    outputStr :: String -> m ()

instance (MFunctor t, MonadTrans t, Monad (t m), MonadTerm m) => MonadTerm (t m) where
    showPrompt    = lift showPrompt
    localPrompt f = hoist (localPrompt f)
    tryGetLine    = lift tryGetLine
    tryGetChar    = lift tryGetChar
    outputStr     = lift . outputStr


outputPrompt :: (MonadTerm m) => m ()
outputPrompt = showPrompt >>= outputStr

-- | Prompt the user for one line of input.
-- Returns @Just@ the input, or @Nothing@ if the user inputs EOF.
promptLine :: (MonadTerm m) => m (Maybe String)
promptLine = outputPrompt >> tryGetLine

promptChar :: (MonadTerm m) => m (Maybe Char)
promptChar = outputPrompt >> tryGetChar


-- | Write a String to stdout, followed by a newline.
outputLine :: (MonadTerm m) => String -> m ()
outputLine = outputStr . (++ "\n")


-- | Write a value to stdout using 'show', followed by a newline.
printLine :: (MonadTerm m, Show a) => a -> m ()
printLine = outputLine . show


-- | Execute a computation with the given 'Prompt'.
withPrompt :: (MonadTerm m) => String -> m a -> m a
withPrompt = localPrompt . const

