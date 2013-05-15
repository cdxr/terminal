import System.Console.Term

import Control.Monad       ( mzero, void )


testTerm :: Term ()
testTerm = reader =<< inputLine
  where
    reader "quit" = mzero
    reader input  = outputLine $ "got " ++ input

main :: IO ()
main = void $ runTerm $ withPrompt "test> " testTerm
