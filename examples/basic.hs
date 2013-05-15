import System.Console.Term

import Control.Monad       ( guard )


testTerm :: Term Int
testTerm = do
    s <- inputLine
    guard (s /= "quit")
    outputLine $ if null s
        then "Nothing entered."
        else "Got " ++ s
    return (length s)

main :: IO ()
main = do
    xs <- loopTerm $ withPrompt "test> " testTerm
    putStr "lengths: "
    print xs
