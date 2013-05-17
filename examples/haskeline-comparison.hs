import System.Console.Haskeline
import System.Console.Terminal
import Text.Read     ( readMaybe )
import Data.Maybe    ( fromMaybe )
import Control.Monad ( guard )


-- Parse the string as an Int, or return 0 in case of parse failure.
readInt :: String -> Int
readInt = fromMaybe 0 . readMaybe


exampleH = runInputT defaultSettings (sumInputsH 0)
 
sumInputsH :: Int -> InputT IO ()
sumInputsH x = do
    outputStrLn $ "Sum: " ++ show x
 
    -- reads a line of input, returning a Maybe String
    ms <- getInputLine "Enter an Int: "
    case ms of
        -- end computation at EOF
        Nothing -> return ()
 
        -- end computation at "quit"
        Just "quit" -> return ()
 
        -- recurse with the new sum
        Just s -> sumInputsH $ x + readInt s


exampleT = runTermWith defaultSettings (sumInputsT 0)

sumInputsT :: (MonadTerm m) => Int -> m a
sumInputsT x = do
    outputLine $ "Sum: " ++ show x

    -- reads a line of input, returning a String.
    -- implicitly ends computation at EOF
    s <- withPrompt "Enter an Int: " inputLine

    -- end computation at "quit"
    guard (s /= "quit")

    -- recurse with the new sum
    sumInputsT $ x + readInt s


main = exampleT
-- main = exampleH
