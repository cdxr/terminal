module System.Console.Terminal.Parse
(
-- * Parser Type
  ParseTermT
, ParseTerm
, parseInputLine
-- * Basic Parsers
, word
, end
-- * Select
, Select
, select
, select'
, parseSelect 
, inputSelect 
-- ** Basic Selects
, selYesNo
)
where


import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.Monoid
import Data.List ( intercalate )

import qualified Text.Parsec as P
import Text.Parsec ( manyTill, anyChar, string, eof, spaces, choice, (<?>))

import System.Console.Terminal


type ParseTermT m = P.ParsecT String () (TermT m)

type ParseTerm = ParseTermT IO


runParseTermT :: (MonadIO m) => ParseTermT m a -> String -> TermT m (Maybe a)
runParseTermT m = liftM hush . P.runPT m () ""
  where
    hush = either (const Nothing) Just


-- | Prompt for one line of input and parse it.
-- Returns @Nothing@ on parse failure.
-- Equal to mzero on eof.
parseInputLine :: (MonadIO m) => ParseTermT m a -> TermT m (Maybe a)
parseInputLine m = runParseTermT m =<< inputLine


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


-- | Succeeds at end of line. A specialized version of
-- 'P.eof'.
end :: (Monad m) => ParseTermT m ()
end = eof


-- | A @Select m a@ represents a @ParseTermT m a@ with a list of options
-- that may be displayed to the user in an input prompt.
data Select m a = Select
    { selNames :: [String]
    , selParse :: ParseTermT m a
    }

instance Functor (Select m) where
    fmap f (Select ns p) = Select ns (fmap f p)

instance (Monad m) => Monoid (Select m a) where
    mempty = Select [] mzero
    Select ns p `mappend` Select ns' p' = Select (ns ++ ns') (p <|> p')

-- | @select name result@ will create a @Select@ value containing the
-- single option @name@, and a parser that matches @name@ and returns
-- @result@.
select :: (Monad m) => String -> a -> Select m a
select name = select' name []

-- | @select' name aliases result@ performs the function of @select name
-- result@ but the created parser will also succeed on any element of
-- @aliases@.
select' :: (Monad m) => String -> [String] -> a -> Select m a
select' name aliases result = Select [name] (result <$ p)
  where
    p = choice . map string $ name : aliases

-- | Format a concise String representation of available options.
showOptions :: Select m a -> String
showOptions = ("(" ++) . (++ ")") . intercalate "|" . selNames

-- | Convert a 'Select' into a 'ParseTermT'
parseSelect :: Select m a -> ParseTermT m a
parseSelect s = selParse s <?> showOptions s


-- | Prompt the user for a selection.
inputSelect :: (MonadIO m) => Select m a -> TermT m (Maybe a)
inputSelect s = runParseTermT (parseSelect s)
            =<< localPrompt (showOptions s ++) inputLine


-- | A simple Select that prompts the user for \"yes\" or \"no\".
selYesNo :: (Monad m) => Select m Bool
selYesNo = select' "y" ["yes"] True
        <> select' "n" ["no"]  False
