import System.Console.Term


testTerm :: Term ()
testTerm = inputLineTo reader
  where
    reader "quit" = return ()
    reader input  = do
        outputLine $ "got " ++ input
        testTerm

main :: IO ()
main = runTerm $ withPrompt "test> " testTerm
