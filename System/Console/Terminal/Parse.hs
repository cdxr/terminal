module System.Console.Terminal.Parse where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import qualified Text.Parsec as P
import Text.Parsec ( manyTill, anyChar, eof, spaces, (<?>))

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



-- | Succeeds at end of line. A specialized version of
-- 'P.eof'.
end :: (Monad m) => ParseTermT m ()
end = eof

-- | Parses a string of continuous non-space characters, and consumes all
-- spaces that immediately follow it.
--
-- TODO: verify the following property:
--
-- @
-- 'P.many' word === fmap words . many 'anyChar'
-- @
--
word :: (Monad m) => ParseTermT m String
word = manyTill anyChar (eof <|> spaces) <?> "word"
