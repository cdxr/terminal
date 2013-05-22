module System.Console.Terminal.Parse where

import Control.Monad
import Control.Monad.IO.Class

import qualified Text.Parsec as P

import System.Console.Terminal


type ParseTermT m = P.ParsecT String () (TermT m)

type ParseTerm = ParseTermT IO


-- | Prompt for one line of input and parse it.
-- Returns @Nothing@ on parse failure.
-- Equal to mzero on eof.
parseInputLine :: (MonadIO m) => ParseTermT m a -> TermT m (Maybe a)
parseInputLine m = liftM hush . P.runPT m () "" =<< inputLine
  where
    hush = either (const Nothing) Just


